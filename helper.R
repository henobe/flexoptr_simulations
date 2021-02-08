library("tidyverse") # data manipulation and presentation
library("lubridate") # date time management
library("cowplot") # plot alignment
library("flexoptr") # self developed library

read_da_data <- function(start_date, end_date) {
  read_csv("data/dadata.csv",
    col_types = cols(
      dt_start_utc = col_datetime(format = ""),
      sechs_h_regelung = col_double(),
      epex_da_de_eur_mwh = col_double(),
      epex_da_de_mwh = col_logical()
    )
  ) %>%
    select(dt_start_utc, epex_da_de_eur_mwh) %>%
    rename(prices = epex_da_de_eur_mwh) %>%
    mutate(dt_start_utc = force_tz(dt_start_utc, "UTC")) %>%
    filter(dt_start_utc >= start_date, dt_start_utc < end_date)
}

read_id_data <- function(start_date, end_date) {
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
      H_LAST = col_double()
    )
  ) %>%
    select(dt_start_utc, H_ID1, H_ID3, H_VWAID) %>%
    filter(dt_start_utc >= start_date, dt_start_utc < end_date) %>%
    mutate(mock_id2 = H_ID3) %>%
    filter(!is.na(H_ID1)) # filters all quarter hour rows
}

cet <- function(str) {
  ymd(str, tz = "Europe/Berlin")
}

# dates of simulation:
simlength <- days(14)
janstart <- cet("20200106")
janend <- janstart + simlength
julstart <- cet("20200701")
julend <- julstart + simlength
sepstart <- cet("20200914")
sepend <- sepstart + simlength



prepare_plot_path <- function(chapter, destination_folder = "../paper/embedded/") {
  function(plotname) {
    paste0(destination_folder, chapter, "_", plotname, ".png")
  }
}




extract_item <- function(result_list, simtype, itemname) {
  `[[`(`[[`(result_list, simtype), itemname)
}
# calc_revenue <- function(trade_df){
#  sum(trade_df$volume * trade_df$prices)
# }
# extract_revenues2 <- function(strategy_simulations){
#
#  simtypes <- c("constant", "da_optim", "id_optim")
#  costs <- map_dbl(simtypes, extract_item,
#                   result_list = strategy_simulations,
#                   itemname = "revenue")
#
#  data.frame(type = simtypes, revenue = costs)
# }
improve_id_tradeprices <- function(trade_df, kind, percent_modifier) {
  if (kind != "id_optim") {
    return(trade_df)
  } else {
    mutate(trade_df,
      prices = prices * (1 + percent_modifier * ifelse(sign(volume) == sign(prices), -1, 1))
    )
  }
}

extract_revenues <- function(strategy_simulations, price_modifier = NULL) {
  simtypes <- c("constant", "da_optim", "id_optim")

  if (is.null(price_modifier)) {
    costs <- map_dbl(simtypes, extract_item,
      result_list = strategy_simulations,
      itemname = "revenue"
    )
  } else {
    get_revenue <- function(name) {
      extract_item(strategy_simulations, name, "trades") %>%
        improve_id_tradeprices(name, price_modifier) %>%
        calc_revenue()
    }
    costs <- map_dbl(simtypes, get_revenue)
    costs[3] <- costs[2] + costs[3]
  }

  data.frame(type = simtypes, revenue = costs)
}

percent_label <- function(x) paste(round(x * 100), "%")

theme_thesis <- function(base_size = 12, base_family = "Helvetica", vlines = TRUE) {
  base_theme <- function() {
    theme_classic(base_size = base_size, base_family = base_family)
  }

  if (vlines) {
    base_theme() %+replace%
      theme(panel.grid.major.y = element_line(colour = "grey80"))
  } else {
    base_theme()
  }
}

custom_y_axis <- function(...) {
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.1)), ...)
}

blank_x_axis_theme <- theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
