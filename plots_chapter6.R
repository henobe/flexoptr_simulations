source("helper.R")
make_path <- prepare_plot_path(6)

costs_configs <- read_rds("data/costs_configs.rds") %>%
  mutate(
    revenues = map(sims, extract_revenues),
    revenues_mod = map(sims, extract_revenues, price_modifier = 0.01)
  )

improvements <- costs_configs %>%
  unnest(c(revenues, revenues_mod), names_repair = "universal") %>%
  filter(`type...9` == "id_optim") %>%
  mutate(improvement = 1 - `revenue...12` / `revenue...10`) %>%
  select(-sims, -`type...9`, -`revenue...10`, -`type...11`, -`revenue...12`)

sensitivity_plot <- list(
  geom_line(
    aes(
      x = increase_per_hour,
      colour = capacity_to_loss,
      group = capacity_to_loss
    ),
    size = 2,
    alpha = 0.7
  ),
  scale_colour_viridis_c(option = "D"),
  labs(
    x = "Possible charge per Hour",
    y = "Improvement on marketing costs per percent of outperforming index prices",
    colour = "Capacity [h]"
  ),
  scale_x_continuous(labels = percent_label),
  theme_thesis()
)

sensitivtiy_data <- improvements %>%
  mutate(increase_per_hour = (charge_rate - loss_rate) / capacity) %>%
  group_by(capacity_to_loss) %>%
  mutate(max_inc = max(increase_per_hour)) %>%
  ungroup()

plot_sensitivity_power <- sensitivtiy_data %>%
  ggplot(aes(y = improvement)) +
  sensitivity_plot +
  custom_y_axis(labels = percent_label)

ggsave(
  filename = make_path("sensitivity_power"),
  width = 10,
  height = 4
)

plot_sensitivity_power_log <- sensitivtiy_data %>%
  filter(capacity_to_loss > 1) %>%
  ggplot(aes(y = improvement / log(capacity_to_loss))) +
  sensitivity_plot

ggsave(
  filename = make_path("sensitivity_power_log"),
  width = 10,
  height = 4
)

plot_sensitivity_correlation <- costs_configs %>%
  select(-sims, -revenues_mod) %>%
  unnest(revenues) %>%
  pivot_wider(names_from = type, values_from = revenue) %>%
  mutate(share_id_optim = (da_optim - id_optim) / (constant - id_optim)) %>%
  right_join(select(improvements, charge_rate, loss_rate, capacity, improvement),
    by = c("charge_rate", "loss_rate", "capacity")
  ) %>%
  ggplot(aes(x = share_id_optim, y = improvement, colour = capacity_to_loss)) +
  scale_colour_viridis_c(option = "D") +
  geom_point() +
  guides(colour = FALSE) +
  custom_y_axis(labels = percent_label) +
  scale_x_continuous(
    limits = c(0, NA),
    labels = percent_label,
    expand = expansion(mult = c(0, 0))
  ) +
  labs(
    x = "Share of Improvement through Intra-Day Optimisation",
    y = "Improvement on marketing costs per percent of outperforming index prices"
  ) +
  theme_thesis()

ggsave(
  filename = make_path("sensitivity_correlation"),
  width = 10,
  height = 4
)
