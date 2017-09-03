rm(list = ls())
appname <- "Robert's Budget"
library(tidyverse)
library(magrittr)
library(lubridate)

source('temporal_plot.R')
source('summary_plot.R')
source('helper functions.R')

# Options
N_PLOT_MAX <- 10 # Visually acceptible max. Actual max will also depend on number of categories etc.
LAB_LENGTH_MAX <- 20

# Get data ----------------------------------------------------------------

load('data/latest.RData')
desc_categories <- read.csv('data/descriptions.csv', stringsAsFactors = FALSE)
dat <- raw_dat %>%
    transmute(
        date = format_date(Date),
        description = Description,
        desc_len = nchar(description),
        debit = Debit,
        credit = Credit
    ) %>%
    left_join(desc_categories) %>%
    replace_na(list(category = 'uncategorised'))

debits <- dat %>%
    filter(!is.na(debit)) %>%
    select(-credit)

credits <- dat %>%
    filter(!is.na(credit)) %>%
    select(-debit)

quant95 <- quantile(debits$debit, 0.95) %>% as.numeric()
quant99 <- quantile(debits$debit, 0.99) %>% as.numeric()
daterange <- range(debits$date)
alldescs <- unique(debits$description)
allcats <- unique(debits$category)

max_nplots <- min(N_PLOT_MAX, max(length(alldescs), length(allcats)))

# Truncate long descriptions/category names
clean_field <- function(x) {
    gsub('[[:space:]]+', ' ', x) %>%
        substr(1, LAB_LENGTH_MAX)
}
debits %<>%
    mutate(
        category = clean_field(category),
        description = clean_field(description)
    )
