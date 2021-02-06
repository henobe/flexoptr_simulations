library("tidyverse")  # data manipulation
library("lubridate")  # date time management
library("progress")  # show progress bar in terminal
library("flexoptr")


# Define functions ----------------------------------------
read_da_data <- function(start_date, end_date) {
  read_csv("data/dadata.csv",
           col_types = cols(dt_start_utc = col_datetime(format = ""),
                            sechs_h_regelung = col_double(),
                            epex_da_de_eur_mwh = col_double(),
                            epex_da_de_mwh = col_logical())) %>%
    select(dt_start_utc, epex_da_de_eur_mwh) %>%
    rename(prices = epex_da_de_eur_mwh) %>%
    mutate(dt_start_utc = force_tz(dt_start_utc, "UTC")) %>%
    filter(dt_start_utc >= start_date, dt_start_utc < end_date)
}

read_id_data <- function(start_date, end_date){
  read_csv("data/indexdata.csv",
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
             H_LAST = col_double())) %>%
    select(dt_start_utc, H_ID1, H_ID3, H_VWAID) %>%
    filter(dt_start_utc >= start_date, dt_start_utc < end_date) %>%
    mutate(mock_id2 = H_ID3) %>%
    filter(!is.na(H_ID1))  # filters all quarter hour rows
}

minimise_parameters <- function(df) {
  df %>%
    mutate(gcd = pmap_dbl(list(loss_rate, capacity, charge_rate, starting_state),
                          DescTools::GCD)) %>%
    mutate(across(c(loss_rate, capacity, charge_rate, starting_state),
                  .fns = as.integer)) %>%
    mutate(loss_rate = loss_rate / gcd,
           capacity = capacity/ gcd,
           starting_state = starting_state / gcd,
           charge_rate = charge_rate / gcd)
}

simulate_configurations <- function(configs, da_prices, id_prices,
                                    id_index_names, ...){
  progtick_simulate_strategies <- function(params, pb){
    res <- simulate_marketing(parameters = params,
                              da_prices = da_prices,
                              id_prices = id_prices,
                              id_index_names = id_index_names,
                              ...)
    pb$tick()
    res
  }
  
  pb <- progress_bar$new(format = "simulating [:bar] :elapsed eta: :eta",
                         total = nrow(configs))
  
  configs %>%
    slice_sample(prop = 1) %>%  # randomly -> progress bar is more accurate
    mutate(params = pmap(list(charge_rate, loss_rate, capacity, starting_state),
                         ~ c("charge_rate" = ..1, "loss_rate" = ..2,
                             "capacity" = ..3, "starting_state" = ..4)),
           sims = map(params, progtick_simulate_strategies, pb = pb)) %>%
    select(-params) %>%
    arrange(capacity_to_loss)  # reorder in a more structured fashion
}

helper_block_no_block_sim <- function(da_prices, id_prices, blocked_per_day,
                                      identifier,
                                      configs = block_configs,
                                      id_names = c("H_ID1", "mock_id2", "H_ID3")){
  sim_shorthand <- function(...){
    simulate_configurations(configs = configs,
                            da_prices = da_prices,
                            id_prices = id_prices,
                            id_index_names = id_names,
                            simplify = TRUE,
                            ...)
  }
  message("now simulating first half...\n")
  
  no_block_data <- sim_shorthand() %>%
    add_column(mod = "no_block")
  
  message("now simulating second half...\n")
  
  block_data <-  sim_shorthand(blocked_per_day = blocked_per_day) %>%
    add_column(mod = "block")
  
  rbind(no_block_data, block_data) %>%
    add_column(ident = identifier)
}

cet <- function(str){
  ymd(str, tz = "Europe/Berlin")
}


# Generate and save configurations ------------------------
start_date <- cet("20200701")
end_date <- cet("20200715")

da_prices <- read_da_data(start_date, end_date)
id_prices <- read_id_data(start_date, end_date)

costs_configs <- data.frame(loss_rate = 20, capacity_to_loss = c(1:24)) %>%
  mutate(capacity = capacity_to_loss * loss_rate,
         starting_state = round(capacity / 2),
         charge_rate = map2(loss_rate, capacity,
                            function(x, y) seq(x, x + y, length.out = 21))) %>%
  unnest("charge_rate") %>%
  mutate(increase_per_hour = (charge_rate - loss_rate) / capacity) %>%
  filter(charge_rate %% 1 == 0,
         charge_rate > loss_rate) %>%
  minimise_parameters() %>%
  simulate_configurations(da_prices = da_prices$prices,
                          id_prices = id_prices,
                          id_index_names = c("H_ID1", "mock_id2", "H_ID3"))


write_rds(costs_configs, "data/costs_configs.rds")


block_configs <- expand_grid(capacity_to_loss = c(3:24),
                                charge_factor = seq(1.5, 3, by = 0.1)) %>%
  mutate(loss_rate = 20,
         capacity = loss_rate * capacity_to_loss,
         charge_rate = charge_factor * loss_rate,
         starting_state = capacity / 2) %>%
  select(-contains("factor")) %>%
  mutate(across(.fns = as.integer)) %>%
  filter(charge_rate %% 2 == 0,
         (charge_rate * 2) < (4 * loss_rate) + capacity)
# minimise_parameters() cant minimise or else introducing a pattern where uneven
# charge rates are blocked for a different percentage.

dfjuly <- helper_block_no_block_sim(da_prices = da_prices$prices,
                                 id_prices = id_prices,
                                 identifier = "20200701",
                                 blocked_per_day = c(rep(F, 8), rep(T, 4), rep(F, 12)))

janstart <- cet("20200106")
janend <- cet("20200120")
dfjan <- helper_block_no_block_sim(da_prices = read_da_data(janstart, janend)$prices,
                                   id_prices = read_id_data(janstart, janend),
                                   identifier = format(janstart, format = "%Y%m%d"),
                                   blocked_per_day = c(rep(F, 16), rep(T, 4), rep(F, 4)))

sepstart <- cet("20200914")
sepend <- cet("20200927")
dfsep <- helper_block_no_block_sim(da_prices = read_da_data(sepstart, sepend)$prices,
                                   id_prices = read_id_data(sepstart, sepend),
                                   identifier = format(sepstart, format = "%Y%m%d"),
                                   blocked_per_day = c(rep(F, 12), rep(T, 4), rep(F, 8)))

rbind(dfjan, dfjuly, dfsep) %>%
  write_rds("data/blocked_comparison.rds")
