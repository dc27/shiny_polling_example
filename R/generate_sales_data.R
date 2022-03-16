# not run
# code to generate a random tibble of sales info for the past 30 days
# saves to 'data/sales.csv'

library(dplyr)
library(readr)
library(magrittr)

previous_day_sales <- function(date) {
  nrows <- sample(25:50, 1)
  
  random_ids <- sample(11:20, nrows, replace = TRUE)
  random_sales <- sample(1:27, nrows, replace = TRUE, prob = seq(0.5, 0.1, -0.015))
  
  sales <- tibble(
    employee_id = paste0("0", random_ids),
    date = rep(date, nrows),
    sales = random_sales
  )
}

dates <- Sys.Date() - 1:30

previous_day_sales_data <- sapply(dates, previous_day_sales, simplify = FALSE)

bind_rows(previous_day_sales_data) %>%
  arrange(date) %>%
  write_csv("data/sales.csv")