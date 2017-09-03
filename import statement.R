### TO DO:

### Set up Shiny interface with these widgets
# - global date selector
# - a temporal plot (options for temporal grouping: daily, weekly, monthly, day of the week, and cumulative vs. non-cumulative)
# - a collapsed-over-time plot (options for proportions: pi, or totals: bar)
# - an 'uncategorised' report box
# - separate level (category vs. description) and category/description selectors for the two plots, and a switch to choose to lock them together

### Future updates:
# - easily handle uncategorised transactions via a button in the report box (creating temp files, opening them with a system call, then merging)
# - combine 'bank statements' into one RData file. Newer ones do NOT go as far back as the older ones. Also, can download older ones from bank website, I think
# - modify to work with other accounts?

# Begin -------------------------------------------------------------------

library(tidyverse)
library(magrittr)


# Helper functions --------------------------------------------------------

format_date <- function(x) {
    # Assumes 4-digit year
    year <- gsub('.*([0-9]{4}).*', '\\1', x)
    # Take out 4-digit number
    x %<>% gsub('(.*)[0-9]{4}(.*)', '\\1\\2', .)
    # Get the other two
    a <- gsub('^[[:punct:]]*([0-9]{2}).*', '\\1', x)
    b <- gsub('.*([0-9]{2})[[:punct:]].*', '\\1', x)
    if (max(as.integer(a)) <= 12) {
        month <- a
        day <- b
    } else {
        month <- b
        day <- a
    }
    x %<>% gsub('([0-9]{4})([0-9]{2})([0-9]{2})', '\\1-\\2-\\3', .)
    paste(year, month, day, sep = '-') %>%
        as.Date()
}

summary_line <- function(x, f) {
    # Assumes 2-column data frame, with amount in 2nd column
    y <- f(x[[2]])
    d <- tibble(date = range(x$date), amt = rep.int(y, 2))
    geom_line(aes(date, amt), data = d, colour = 'red')
}

summary_lab <- function(x, f, lab) {
    # Assumes 2-column data frame, with amount in 2nd column
    # browser()
    y <- f(x[[2]])
    d <- tibble(range(x$date), amt = rep.int(y, 2), lab = rep.int(lab, 2))
    geom_text(aes(date, amt, label = lab), data = d)
}

summarise_spending <- function(x, f = sum, min_date = min(x$date), max_date = max(x$date), by_category = TRUE) {
    if (by_category) {
        x %<>%
            filter(date >= min_date & date <= max_date) %>%
            group_by(category)
    } else {
        x %<>%
            filter(date >= min_date & date <= max_date) %>%
            group_by(description)
    }
        out <- x %>%
            summarise(y = f(debit)) %>%
            arrange(desc(y))
    out
}

#   -----------------------------------------------------------------------


get_statement_date <- function(fn) {
    gsub('trans([0-9]{2})([0-9]{2})([0-9]{2})\\.csv', '20\\3-\\2-\\1', fn) %>% as.Date()
}

update_latest_data <- function(fn) {
    raw_dat <- read.csv(paste0('bank statements/', fn), stringsAsFactors = FALSE)
    latest_dat_date <- get_statement_date(fn)
    save(raw_dat, latest_dat_date, file = 'data/latest.RData')
}

statement_files <- tibble(filename = dir('bank statements/')) %>%
    mutate(filedate = get_statement_date(filename)) %>%
    arrange(desc(filedate)) %>%
    mutate(latest = row_number() %in% 1)
latest_datafile <- statement_files %>%
    filter(latest) %>%
    pull(filename)
update_latest_data(latest_datafile)

#   -----------------------------------------------------------------------


raw_dat <- read.csv(paste0('bank statements/', latest_datafile), stringsAsFactors = FALSE)
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
# separate(description, c('what', 'where'), 'TAB', extra = 'merge', fill = 'right') %>%


debits <- dat %>%
    filter(!is.na(debit)) %>%
    select(-credit)

credits <- dat %>%
    filter(!is.na(credit)) %>%
    select(-debit)

# Visualise data ----------------------------------------------------------

quant95 <- quantile(debits$debit, 0.95) %>% as.numeric()
quant99 <- quantile(debits$debit, 0.99) %>% as.numeric()

# Top 10 description spends
top_n <- function(x, n) {
    # Makes certain assumptions about x's format
    x %>%
        summarise_spending() %>%
        filter(row_number() <= n) %>%
        `$`('category')
}
top10 <- top_n(debits, 10)

# Put all dates for each of the top 10 descriptions

# Daily total:
cum_spending <- function(debits, exclusion_cats) {
    dt_dat <- debits %>%
        filter(!category %in% exclusion_cats) %>%
        group_by(date, category) %>%
        summarise(debit = sum(debit)) %>%
        ungroup()
    all_dates_all_categories <- dt_dat %$%
        expand.grid(unique(date), unique(category), stringsAsFactors = FALSE) %>%
        rename(date = Var1, category = Var2)
    dt_dat %>%
        right_join(all_dates_all_categories) %>%
        replace_na(list(debit = 0)) %>%
        group_by(category) %>%
        arrange(date) %>%
        mutate(debit = cumsum(debit)) %>%
        ggplot(aes(date, debit, colour = category, group = category)) +
        geom_line()
}

cum_spending(debits, exclusion_cats = 'travel') %>% print()


# pi chart:
spending_pi <- function(debits, exclusion_cats = NULL, focus_cat = unique(debits$category), n_plot = 10, ...) {
    spending_summary <- debits %>%
        filter(category %in% focus_cat) %>%
        summarise_spending(...)
    if ('category' %in% names(spending_summary)) {
        spending_summary %<>%
            filter(!category %in% exclusion_cats) %>%
            rename(description = category)
    }
    spending_summary %>%
        filter(row_number() <= n_plot) %>%
        ggplot(aes(x = " ", y, fill = description)) +
        geom_bar(width = 1, colour = '#404040', stat = 'identity') +
        coord_polar(theta = 'y')
}
spending_pi(debits, focus_cat = 'booze', by_category = FALSE)

category_breakdown <- function(debits, focus_cat, n_plot = 10) {
    debits %>%
        filter(category %in% focus_cat) %>%
        summarise_spending(by_category = FALSE) %>%
        filter(row_number() <= n_plot) %>%
        ggplot(aes(description, y, fill = description)) +
        geom_bar(stat = 'identity')
}
category_breakdown(debits, 'snack', 20)

# Report on how many are uncategorised
uncat_report <- function(debits, min_date = min(debits$date), max_date = max(debits$date)) {
    debits %>%
        filter(date >= min_date & date <= max_date) %>%
        mutate(uncat = category %in% 'uncategorised') %>%
        summarise(uncat_trans = mean(uncat), uncat_total = sum(debit * uncat) / sum(debit))
}