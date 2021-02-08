library("tidyverse") # data manipulation
library("lubridate") # date time management
library("progress") # show progress bar in terminal
library("flexoptr")

source("helper.R")

# Define functions ----------------------------------------
minimise_parameters <- function(df) {
  df %>%
    mutate(gcd = pmap_dbl(
      list(loss_rate, capacity, charge_rate, starting_state),
      DescTools::GCD
    )) %>%
    mutate(across(c(loss_rate, capacity, charge_rate, starting_state),
      .fns = as.integer
    )) %>%
    mutate(
      loss_rate = loss_rate / gcd,
      capacity = capacity / gcd,
      starting_state = starting_state / gcd,
      charge_rate = charge_rate / gcd
    )
}

simulate_configurations <- function(configs, da_prices, id_prices,
                                    id_index_names, ...) {
  progtick_simulate_strategies <- function(params, pb) {
    res <- simulate_marketing(
      parameters = params,
      da_prices = da_prices,
      id_prices = id_prices,
      id_index_names = id_index_names,
      ...
    )
    pb$tick()
    res
  }

  pb <- progress_bar$new(
    format = "simulating [:bar] :elapsed eta: :eta",
    total = nrow(configs)
  )

  configs %>%
    slice_sample(prop = 1) %>% # randomly -> progress bar is more accurate
    mutate(
      params = pmap(
        list(charge_rate, loss_rate, capacity, starting_state),
        ~ c(
          "charge_rate" = ..1, "loss_rate" = ..2,
          "capacity" = ..3, "starting_state" = ..4
        )
      ),
      sims = map(params, progtick_simulate_strategies, pb = pb)
    ) %>%
    select(-params) %>%
    arrange(capacity_to_loss) # reorder in a more structured fashion
}

helper_block_no_block_sim <- function(da_prices, id_prices, blocked_per_day,
                                      identifier,
                                      configs = block_configs,
                                      id_names = c("H_ID1", "mock_id2", "H_ID3")) {
  sim_shorthand <- function(...) {
    simulate_configurations(
      configs = configs,
      da_prices = da_prices,
      id_prices = id_prices,
      id_index_names = id_names,
      simplify = TRUE,
      ...
    )
  }
  message("now simulating first half...\n")

  no_block_data <- sim_shorthand() %>%
    add_column(mod = "no_block")

  message("now simulating second half...\n")

  block_data <- sim_shorthand(blocked_per_day = blocked_per_day) %>%
    add_column(mod = "block")

  rbind(no_block_data, block_data) %>%
    add_column(ident = identifier)
}


# Generate and save configurations ------------------------
da_prices <- read_da_data(julstart, julend)
id_prices <- read_id_data(julstart, julend)

message("now simulating costs_configs\n")

costs_configs <- data.frame(loss_rate = 20, capacity_to_loss = c(1:24)) %>%
  mutate(
    capacity = capacity_to_loss * loss_rate,
    starting_state = round(capacity / 2),
    charge_rate = map2(
      loss_rate, capacity,
      function(x, y) seq(x, x + y, length.out = 21)
    )
  ) %>%
  unnest("charge_rate") %>%
  mutate(increase_per_hour = (charge_rate - loss_rate) / capacity) %>%
  filter(
    charge_rate %% 1 == 0,
    charge_rate > loss_rate
  ) %>%
  minimise_parameters() %>%
  simulate_configurations(
    da_prices = da_prices$prices,
    id_prices = id_prices,
    id_index_names = c("H_ID1", "mock_id2", "H_ID3")
  )

write_rds(costs_configs, "data/costs_configs.rds")


message("now simulating blocked july\n")

block_configs <- expand_grid(
  capacity_to_loss = c(3:24),
  charge_factor = seq(1.5, 3, by = 0.1)
) %>%
  mutate(
    loss_rate = 20,
    capacity = loss_rate * capacity_to_loss,
    charge_rate = charge_factor * loss_rate,
    starting_state = capacity / 2
  ) %>%
  select(-contains("factor")) %>%
  mutate(across(.fns = as.integer)) %>%
  filter(
    charge_rate %% 2 == 0,
    (charge_rate * 2) < (4 * loss_rate) + capacity
  )
# minimise_parameters() cant minimise or else introducing a pattern where uneven
# charge rates are blocked for a different percentage.

dfjuly <- helper_block_no_block_sim(
  da_prices = da_prices$prices,
  id_prices = id_prices,
  identifier = format(julstart, format = "%Y%m%d"),
  blocked_per_day = c(rep(F, 8), rep(T, 4), rep(F, 12))
)


message("now simulating blocked january\n")
dfjan <- helper_block_no_block_sim(
  da_prices = read_da_data(janstart, janend)$prices,
  id_prices = read_id_data(janstart, janend),
  identifier = format(janstart, format = "%Y%m%d"),
  blocked_per_day = c(rep(F, 16), rep(T, 4), rep(F, 4))
)


message("now simulating blocked september\n")
dfsep <- helper_block_no_block_sim(
  da_prices = read_da_data(sepstart, sepend)$prices,
  id_prices = read_id_data(sepstart, sepend),
  identifier = format(sepstart, format = "%Y%m%d"),
  blocked_per_day = c(rep(F, 12), rep(T, 4), rep(F, 8))
)

rbind(dfjan, dfjuly, dfsep) %>%
  write_rds("data/blocked_comparison.rds")
