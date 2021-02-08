source("helper.R")
make_path <- prepare_plot_path(4)

base_parameters <- c("starting_state" = 10, "capacity" = 20, "charge_rate" = 4, "loss_rate" = 1)

da_data <- read_csv("data/dadata.csv",
  col_types = cols(
    dt_start_utc = col_datetime(format = ""),
    sechs_h_regelung = col_double(),
    epex_da_de_eur_mwh = col_double(),
    epex_da_de_mwh = col_logical()
  )
) %>%
  select(dt_start_utc, epex_da_de_eur_mwh) %>%
  rename(da_price = epex_da_de_eur_mwh) %>%
  mutate(
    dt_start_utc = force_tz(dt_start_utc, "UTC"),
    dt_start_berlin = with_tz(dt_start_utc, "Europe/Berlin")
  ) %>%
  filter(dt_start_utc >= cet("20200701"), dt_start_utc < cet("20200704"))

da_res <- optimise_schedule(
  schedule = rep(0, nrow(da_data)),
  prices = format_da_prices(da_data$da_price),
  parameters = base_parameters,
  shift = 24 * base_parameters["loss_rate"]
)

daily_marker <- geom_vline(
  xintercept = force_tz(
    c(ymd_hm("20200702 00:00"), ymd_hm("20200703 00:00")),
    "Europe/Berlin"
  ),
  colour = "grey50",
  linetype = "dashed"
)

pcharge <- da_data %>%
  cbind(state = da_res$state) %>%
  add_row(
    dt_start_berlin = force_tz(ymd_hm("20200630 23:00"), "Europe/Berlin"),
    state = base_parameters["starting_state"]
  ) %>%
  mutate(dt_start_berlin = dt_start_berlin + hours(1)) %>%
  ggplot(aes(y = state, x = dt_start_berlin)) +
  geom_line(size = 1.5) +
  daily_marker +
  labs(
    x = NULL,
    y = "Storage State [MWh]"
  ) +
  custom_y_axis(breaks = c(10, 20)) +
  theme_thesis() +
  blank_x_axis_theme

ppower <- da_data %>%
  cbind(charge = da_res$schedule) %>%
  ggplot(aes(x = dt_start_berlin + minutes(30), y = charge)) +
  geom_col(fill = "black") +
  labs(
    x = NULL,
    y = "Charging Power [MW]"
  ) +
  custom_y_axis(breaks = c(1:4)) +
  daily_marker +
  theme_thesis() +
  blank_x_axis_theme

pprices <- da_data %>%
  add_row(
    dt_start_berlin = force_tz(ymd_hm("20200704 00:00"), "Europe/Berlin"),
    da_price = da_data$da_price[nrow(da_data)]
  ) %>%
  ggplot(aes(x = dt_start_berlin, y = da_price)) +
  geom_step(size = 1.5) +
  custom_y_axis(breaks = c(20, 40)) +
  labs(
    x = NULL,
    y = "DA-Price [€/MW]"
  ) +
  daily_marker +
  theme_thesis()

plot_grid(
  plotlist = list(pcharge, ppower, pprices),
  align = "v",
  ncol = 1
) %>%
  ggsave(
    filename = make_path("optimisation_schedule"),
    width = 10,
    height = 6
  )

id_data <- read_csv("data/indexdata.csv",
  col_types = cols(
    dt_start_utc = col_datetime(format = ""),
    QH_ID1 = col_double(),
    HH_ID1 = col_double(),
    H_ID1 = col_double(),
    QH_ID3 = col_double(),
    HH_ID3 = col_double(),
    H_ID3 = col_double(),
    QH_VWAID = col_double(),
    HH_VWAID = col_double(),
    H_VWAID = col_double(),
    QH_LAST = col_double(),
    HH_LAST = col_double(),
    H_LAST = col_double()
  )
) %>%
  select(dt_start_utc, H_ID1, H_ID3) %>%
  mutate(dt_start_utc = force_tz(dt_start_utc, "UTC")) %>%
  filter(dt_start_utc >= cet("20200701"), dt_start_utc < cet("20200704")) %>%
  filter(!is.na(H_ID1)) %>%
  mutate(H_ID2_mock = H_ID3)


id_res <- optimise_schedule(
  schedule = da_res$schedule,
  prices = format_id_prices(id_data, c("H_ID1", "H_ID2_mock", "H_ID3"))$lookout %>%
    shorten(3),
  parameters = base_parameters,
  shift = 0
)

plot_id_difference <- cbind(da_data,
  da = da_res$state,
  id = id_res$state
) %>%
  select(-da_price) %>%
  mutate(dt_start_berlin = dt_start_berlin + hours(1)) %>%
  add_row(dt_start_berlin = ymd_hm("20200701 0000", tz = "Europe/Berlin"), id = 10, da = 10) %>%
  pivot_longer(cols = c("da", "id")) %>%
  mutate(name = factor(name, levels = c("id", "da"), labels = c("DA & ID", "DA"))) %>%
  ggplot(aes(x = dt_start_berlin, y = value, colour = name)) +
  geom_line(size = 1.5) +
  annotate("text", x = ymd_hm("20200703 1900"), y = 15, label = "DA & ID") +
  annotate("text", x = ymd_hm("20200703 1745"), y = 4.5, label = "DA") +
  labs(
    x = NULL,
    y = "Storage State [MWh]"
  ) +
  guides(colour = FALSE) +
  custom_y_axis(breaks = c(10, 20)) +
  theme_thesis()

ggsave(
  filename = make_path("id_difference"),
  plot = plot_id_difference,
  width = 10,
  height = 4
)

plot_id_trades <- id_res$trades %>%
  filter(between(time, 2, 7)) %>%
  mutate(
    timescale = hours(time - 1) + ymd_hm("20200701 0000", tz = "Europe/Berlin"),
    trading_time = format(hours(trading_time) + ymd_hm("20200701 0000"), "%H:%M"),
    trading_time = factor(trading_time, levels = c("00:00", "01:00", "03:00"))
  ) %>%
  arrange(time, trading_time) %>%
  ggplot(aes(x = timescale)) +
  geom_hline(yintercept = 0) +
  geom_col(aes(
    y = volume,
    fill = trading_time
  ),
  alpha = 0.8,
  position = position_stack()
  ) +
  geom_label(aes(
    y = c(-1.5, 2, -0.75, 0.75, -0.75, -1.5, 1.5),
    label = paste(prices, "€")
  )) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    y = "Traded Volume [MW]",
    x = "Start of Hour-Contract",
    fill = "Time of Trade"
  ) +
  scale_y_continuous(limits = c(-6, 6), breaks = c(-6, -4, -2, 2, 4, 6)) +
  theme_thesis() +
  theme(legend.position = "bottom")

ggsave(
  filename = make_path("id_trades"),
  plot = plot_id_trades,
  width = 10,
  height = 4
)
