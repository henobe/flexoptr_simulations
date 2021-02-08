source("helper.R")
make_path <- prepare_plot_path(7)

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

fixed_schedule <- rep(c(0, 0, 0, 0, 0, 2, 2, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), 3)
blockers <- (fixed_schedule > 0)

blocked_trades <- describe_trades(
  rep(0, length(fixed_schedule)), fixed_schedule,
  da_data$da_price
) %>% append_trades(NULL, 0)


da_res_block <- optimise_schedule(fixed_schedule,
  prices = format_da_prices(da_data$da_price),
  parameters = base_parameters,
  shift = 18 * base_parameters["loss_rate"],
  blockers
)

da_res_block$trades <- rbind(da_res_block$trades, blocked_trades, make.row.names = FALSE)

pcharge <- da_data %>%
  cbind(state = da_res_block$state) %>%
  add_row(
    dt_start_berlin = ymd_hm("20200630 23:00", tz = "Europe/Berlin"),
    state = base_parameters["starting_state"]
  ) %>%
  mutate(dt_start_berlin = dt_start_berlin + hours(1)) %>%
  ggplot(aes(y = state, x = dt_start_berlin)) +
  geom_line(size = 1.5) +
  labs(
    x = NULL,
    y = "Storage State [MWh]"
  ) +
  custom_y_axis(breaks = c(10, 20)) +
  theme_thesis() +
  blank_x_axis_theme

ppower <- da_data %>%
  cbind(charge = da_res_block$schedule) %>%
  ggplot(aes(x = dt_start_berlin + minutes(30), y = charge)) +
  geom_col(
    data = data.frame(
      xpos = da_data$dt_start_berlin[which(blockers)] + minutes(30),
      height = unname(base_parameters["charge_rate"])
    ),
    mapping = aes(x = xpos, y = height), fill = "grey70"
  ) +
  geom_col(fill = "black") +
  annotate("text", x = ymd_hm("20200702 2200"), y = 3.5, label = "blocked") +
  annotate(
    geom = "curve", x = ymd_hm("20200702 2200"), y = 3.2, xend = ymd_hm("20200703 0200"), yend = 2.8,
    curvature = .3, arrow = arrow(length = unit(2, "mm"))
  ) +
  labs(
    x = NULL,
    y = "Charging Power [MW]"
  ) +
  custom_y_axis(breaks = c(1:4)) +
  theme_thesis()

plot_blocked_schedule <- plot_grid(plotlist = list(pcharge, ppower), align = "v", ncol = 1)

ggsave(
  filename = make_path("blocked_schedule"),
  width = 10,
  height = 4
)

extra_costs <- calc_revenue(da_res_block$trades) - calc_revenue(da_res$trades)
perc_extra_costs <- (extra_costs + calc_revenue(da_res$trades)) / calc_revenue(da_res$trades) - 1

rebap_data <- read_csv("data/rebapdata.csv",
  col_types = cols(
    dt_start_utc = col_datetime(format = ""),
    rz_saldo_mwh = col_double(),
    rebap_eur_mwh = col_double()
  )
)

plot_rebap_median <- rebap_data %>%
  mutate(dt_start_cet = with_tz(dt_start_utc, "Europe/Berlin")) %>%
  filter((dt_start_utc >= cet("20200701") & dt_start_utc < cet("20200715")) |
    (dt_start_utc >= cet("20200106") & dt_start_utc < cet("20200120")) |
    (dt_start_utc >= cet("20200914") & dt_start_utc < cet("20200928"))) %>%
  mutate(month = factor(month(dt_start_cet),
    levels = c(1, 7, 9),
    labels = c("January 6-19", "July 1-14", "September 14-27")
  )) %>%
  group_by(month) %>%
  mutate(
    block = ceiling(row_number() / 16),
    rec_block = factor(block %% 6,
      levels = c(1:5, 0),
      labels = c("0-4", "4-8", "8-12", "12-16", "16-20", "20-24")
    )
  ) %>%
  ggplot(aes(y = fct_rev(rec_block), x = abs(rebap_eur_mwh), fill = factor(month))) +
  geom_boxplot(position = position_dodge2()) +
  labs(
    y = NULL,
    x = "Imbalance Price (absolute) [€/MWh]",
    fill = "Time Span",
    caption = "Only values below 200€."
  ) +
  theme_thesis() +
  coord_cartesian(xlim = c(0, 200))


ggsave(
  filename = make_path("rebap_median"),
  width = 10,
  height = 4
)

simtime_labels <- c("6.-19.1. 4-8 PM", "1-14.7. 8-12 AM", "14.-27.9. 12-4 PM")

sim_data <- read_rds("data/blocked_comparison.rds") %>%
  mutate(ident = factor(ident,
    levels = c("20200106", "20200701", "20200914"),
    labels = simtime_labels
  ))

revenue_p_mwh <- sim_data %>%
  group_by(ident) %>%
  filter(row_number() == 1) %>%
  unnest(sims) %>%
  filter(row_number() == 1) %>%
  mutate(revenue_p_mwh = revenue_abs / 336 / loss_rate) %>%
  select(ident, revenue_p_mwh)

blocked_comparison <- sim_data %>%
  unnest(sims) %>%
  filter(type == "id_optim") %>%
  select(-revenue_rel) %>%
  pivot_wider(names_from = mod, values_from = revenue_abs) %>%
  left_join(revenue_p_mwh, by = "ident") %>%
  mutate(
    diff = (block - no_block),
    diff_2mw = diff / loss_rate * 2,
    diff_p_hour = diff_2mw / (4 * 14),
    charge_rel = charge_rate / loss_rate,
    diff_p_hour_mw = diff_p_hour / (charge_rel / 2)
  )

std_tile_plot <- list(
  geom_tile(aes(y = capacity_to_loss, x = charge_rel * 2)),
  scale_y_continuous(
    breaks = seq(6, 24, by = 6),
    labels = function(x) paste(x, "h"),
    sec.axis = sec_axis(~ . * 2,
      name = "Technical Capacity",
      labels = function(x) paste(x, "MWh")
    )
  ),
  scale_x_continuous( # labels = function(x) paste(x, "M"),
    sec.axis = sec_axis(~ . / 2,
      name = "Offered Balancing Power [MW]" # ,labels = function(x) paste(x, "MW")
    )
  ),
  scale_fill_viridis_c(option = "C", direction = -1),
  labs(
    y = "Relative Capacity",
    x = "Charging Power [MW]"
  ),
  theme_thesis(),
  theme(legend.position = "bottom"),
  facet_wrap(~ident)
)

plot_overall_costs <- blocked_comparison %>%
  ggplot(aes(fill = block / (loss_rate * 14 * 24))) +
  std_tile_plot +
  labs(fill = "Overall Marketing Costs [€/MWh]")

ggsave(
  filename = make_path("overall_costs"),
  width = 10,
  height = 4
)

plot_additional_costs <- blocked_comparison %>%
  ggplot(aes(fill = diff_p_hour)) +
  std_tile_plot +
  labs(fill = "Additional costs for each hour of availability [€/h]")

ggsave(
  filename = make_path("additional_costs"),
  width = 10,
  height = 4
)

plot_additional_costs_mw <- blocked_comparison %>%
  ggplot(aes(fill = diff_p_hour_mw)) +
  std_tile_plot +
  labs(fill = "Additional costs for each hour of availability\nand each MW of offered power [€/MWh]")
# more detailled plot in appendix
# use here specific case, use general case in appendix!

ggsave(
  filename = make_path("additional_costs_mw"),
  width = 10,
  height = 4
)


plot_additional_rel_costs <- sim_data %>%
  unnest(sims) %>%
  filter(type == "id_optim") %>%
  select(-revenue_abs) %>%
  pivot_wider(names_from = mod, values_from = revenue_rel) %>%
  mutate(
    charge_rel = charge_rate / loss_rate,
    perc_diff = (block / no_block - 1) / (charge_rel / 10 / 2)
  ) %>%
  ggplot(aes(fill = perc_diff)) +
  std_tile_plot +
  labs(fill = "Additional Costs [%] per offered balancing power")

ggsave(
  filename = make_path("additional_rel_costs"),
  width = 10,
  height = 4
)

find_sim_time <- function(x) {
  if_else(x >= janstart & x < janend,
    1,
    if_else(x >= julstart & x < julend,
      2,
      if_else(x >= sepstart & x < sepend,
        3,
        0
      )
    )
  )
}

is_blocked <- function(simtime, clocktime) {
  switch(paste(simtime),
    "1" = between(clocktime, 16, 19),
    "2" = between(clocktime, 8, 11),
    "3" = between(clocktime, 12, 15)
  )
}

extend_last_hour <- function(df) {
  max_value <- max(df$time)
  last_price <- df$med[df$time == max_value]
  df %>%
    add_row("time" = max_value + 1, "med" = last_price)
}

da_median_prices <- read_da_data(janstart, sepend) %>%
  mutate(simtime = find_sim_time(dt_start_utc)) %>%
  filter(simtime > 0) %>%
  mutate(
    dt_german = with_tz(dt_start_utc, "Europe/Berlin"),
    time = as.numeric(strftime(dt_german, format = "%H"))
  ) %>%
  group_by(simtime, time) %>%
  summarise(med = median(prices), .groups = "drop") %>%
  group_by(simtime) %>%
  nest() %>%
  transmute(data = map(data, extend_last_hour)) %>%
  unnest(data) %>%
  ungroup() %>%
  mutate(
    blocked = map2_lgl(simtime, time, is_blocked),
    simtime = factor(simtime, labels = c("6.-19.1.", "1-14.7.", "14.-27.9."))
  )

focused_data <- da_median_prices %>%
  filter(blocked) %>%
  group_by(simtime) %>%
  nest() %>%
  transmute(data = map(data, extend_last_hour)) %>%
  unnest(data) %>%
  ungroup()

ggplot(da_median_prices, aes(x = time, y = med, group = simtime)) +
  geom_step(colour = "grey80", size = 2) +
  geom_step(data = focused_data, aes(colour = simtime), size = 2) +
  labs(
    x = "Hour",
    y = "Median Day Ahead Price [€/MWh]",
    colour = ""
  ) +
  scale_x_continuous(breaks = seq(0, 24, by = 6)) +
  custom_y_axis() +
  theme_thesis() +
  guides(alpha = FALSE, colour = FALSE) +
  facet_wrap(~simtime)

ggsave(
  filename = make_path("da_blocked_prices"),
  width = 10,
  height = 4
)
