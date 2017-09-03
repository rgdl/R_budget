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

filter_debits <- function(x, catlevel = 'Between', min_date = min(x$date), max_date = max(x$date), cats = unique(x$category), descs = unique(x$description)) {
    x %<>%
        filter(date >= min_date & date <= max_date)

    if (catlevel %in% 'Between') {
        x %<>%
            filter(category %in% cats) %>%
            group_by(category)
    } else {
        x %<>%
            filter(category %in% cats[1] & description %in% descs) %>%
            group_by(description)
    }
}

# Report on how many are uncategorised
uncat_report <- function(debits, min_date = min(debits$date), max_date = max(debits$date)) {
    debits %>%
        filter(date >= min_date & date <= max_date) %>%
        mutate(uncat = category %in% 'uncategorised') %>%
        summarise(uncat_trans = mean(uncat), uncat_total = sum(debit * uncat) / sum(debit))
}

fmt_pct <- function(x, digits = 2) {
    (x * 100) %>%
        round(digits) %>%
        paste0(., '%')
}