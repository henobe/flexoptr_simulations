source("helper.R")
make_path <- prepare_plot_path(5)

costs_configs <- read_rds("data/costs_configs.rds")


interpreted_costs_configs <- costs_configs %>%
  mutate(revenues = map(sims, extract_revenues)) %>%
  select(-sims) %>%
  unnest(revenues) %>%
  group_by(capacity_to_loss, increase_per_hour) %>%
  mutate(revenue_percent = 1 - (revenue / revenue[1])) %>%
  ungroup() %>%
  arrange(increase_per_hour) %>%
  filter(type != "constant") %>%
  mutate(type = factor(type,
    levels = c("id_optim", "da_optim"),
    labels = c("DA & ID", "DA")
  ))


# fig.width = 7, fig.height = 4}
plot_reduction_configs <- interpreted_costs_configs %>%
  filter(capacity_to_loss %in% c(1, 4, 12, 24)) %>%
  ggplot(aes(x = increase_per_hour, y = revenue_percent, colour = factor(capacity_to_loss), linetype = type)) +
  geom_line(size = 2) +
  scale_colour_viridis_d() +
  scale_y_continuous(
    limits = c(0, NA),
    breaks = seq(0.2, 1, by = 0.2),
    labels = percent_label,
    expand = expansion(mult = c(0, 0.1))
  ) +
  scale_x_continuous(
    limits = c(0, NA),
    breaks = c(0.25, 0.5, 0.75, 1),
    labels = percent_label,
    expand = c(0, 0)
  ) +
  guides(colour = guide_legend(reverse = TRUE)) +
  labs(
    y = "Cost reduction",
    x = "Maximum possible charge per hour",
    colour = "Capacity [h]",
    linetype = "Optimised on..."
  ) +
  theme_classic() +
  theme(
    panel.background = element_rect(fill = "grey80"),
    panel.grid.major.y = element_line(colour = "white", size = 0.5)
  )
# auf power skalierte version in anhang
ggsave(
  filename = make_path("reduction_configs"),
  width = 10,
  height = 4
)

plot_reduction_configs_log <- interpreted_costs_configs %>%
  filter(capacity_to_loss %in% c(4, 8, 12, 16, 20, 24)) %>%
  ggplot(aes(x = increase_per_hour, y = revenue_percent / log10(capacity_to_loss), colour = factor(capacity_to_loss), linetype = type)) +
  geom_line(size = 2) +
  scale_colour_viridis_d() +
  scale_y_continuous(limits = c(0, NA), breaks = seq(0.2, 1, by = 0.2), labels = percent_label, expand = expansion(mult = c(0, 0.1))) +
  scale_x_continuous(limits = c(0, NA), breaks = c(0.25, 0.5, 0.75, 1), labels = percent_label, expand = c(0, 0)) +
  guides(colour = guide_legend(reverse = TRUE)) +
  labs(
    y = "Cost reduction",
    x = "Maximum possible charge per hour",
    colour = "Capacity [h]",
    linetype = "Optimised on..."
  ) +
  theme_classic() +
  theme(
    panel.background = element_rect(fill = "grey80"),
    panel.grid.major.y = element_line(colour = "white", size = 0.5)
  )

ggsave(
  filename = make_path("reduction_configs_log"),
  width = 10,
  height = 4
)


reduction_potential <- function(charge_rate, loss_rate, capacity, value) {
  full_potential <- which(charge_rate == loss_rate + capacity)
  if (length(full_potential) == 0) {
    return(NA)
  }
  if (length(full_potential) > 1) stop("more than one times maximum value")
  value[full_potential]
}
# fig.height=3, fig.width=7}

plot_reduction_max <- interpreted_costs_configs %>%
  filter(capacity_to_loss %% 1 == 0) %>%
  group_by(type, capacity_to_loss) %>%
  summarise(maximum_reduction = reduction_potential(charge_rate, loss_rate, capacity, revenue_percent), .groups = "drop") %>%
  filter(!is.na(maximum_reduction)) %>%
  ggplot(aes(x = capacity_to_loss, y = maximum_reduction, linetype = type, shape = type)) +
  geom_line(size = 1, colour = "grey70") +
  geom_point(size = 1.5) +
  labs(
    x = "Capacity [h]",
    y = "Potential reduction of energy costs"
  ) +
  scale_y_continuous(
    limits = c(0, NA), breaks = seq(0.2, 1, by = 0.2),
    labels = percent_label, expand = expansion(mult = c(0, 0.1))
  ) +
  scale_x_continuous(limits = c(0, 25), breaks = seq(5, 20, by = 5), expand = expansion(mult = 0)) +
  annotate("text", x = 8, y = 0.47, label = "DA & ID") +
  annotate("text", x = 13.5, y = 0.32, label = "DA") +
  guides(linetype = FALSE, shape = FALSE) +
  theme_classic() +
  theme(panel.grid.major.y = element_line(colour = "grey80"))

ggsave(
  filename = make_path("reduction_max"),
  width = 10,
  height = 4
)

# zusätzlich log kurve 0.45 * log10(cap_to_loss)
# legend inside plot as annotation

# fig.height = 4, fig.width = 7}
plot_power_influence <- interpreted_costs_configs %>%
  arrange(capacity_to_loss, charge_rate) %>%
  group_by(capacity_to_loss, type) %>%
  mutate(
    maximum_reduction = reduction_potential(charge_rate, loss_rate, capacity, revenue_percent),
    rel_revenue_perc = revenue_percent / maximum_reduction
  ) %>%
  filter(capacity_to_loss %% 1 == 0) %>%
  # filter(type == "DA & ID") %>%
  ggplot(aes(x = increase_per_hour, y = rel_revenue_perc, colour = capacity_to_loss)) +
  geom_line(aes(group = capacity_to_loss), alpha = 0.7, size = 1) +
  scale_y_continuous(labels = percent_label) +
  scale_x_continuous(labels = percent_label) +
  scale_colour_viridis_c() +
  #  scale_colour_gradientn(colours = viridisLite::viridis(256, option = "plasma")) +
  #  scale_colour_distiller(palette = "Spectral", direction = 1) +
  labs(
    y = "Cost reduction of optimum",
    x = "Maximum possible charge per hour",
    colour = "Capacity [h]"
  ) +
  coord_cartesian(xlim = c(0.05, 0.5)) +
  theme_classic() +
  theme(
    legend.position = "bottom",
    panel.grid.major.y = element_line(colour = "white", size = 0.5),
    panel.background = element_rect(fill = "grey80")
  ) +
  facet_wrap(~ fct_rev(type), ncol = 2)

ggsave(
  filename = make_path("power_influence"),
  width = 10,
  height = 4
)
