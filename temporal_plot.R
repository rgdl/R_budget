temporal_plot <- function(x, tempgrouping = 'Daily', proportot = 'Totals', catlevel = 'Between', min_date = min(x$date), max_date = max(x$date), cumulative = TRUE, n_plot = 7, cats = unique(x$category), descs = unique(x$description)) {

    # Filter ~ min_date, max_date, cats, descs, catlevel
    x %<>% filter_debits(catlevel, min_date, max_date, cats, descs)

    # Aggregate data ~ tempgrouping
    if (tempgrouping %in% 'Weekly') {
        x %<>% mutate(date = week(date))
        timelab <- 'Week'
    } else if (tempgrouping %in% 'Monthly') {
        x %<>% mutate(date = month(date, TRUE, TRUE))
        timelab <- 'Month'
    } else if (tempgrouping %in% 'Day of the week') {
        x %<>% mutate(date = wday(date, TRUE, TRUE))
        timelab <- 'Day'
    } else {
        timelab <- 'Date'
    }

    # Further filtering ~ catlevel, cumulative, n_plot
    if (catlevel %in% 'Between') {
        top_cats <- x %>%
            summarise(y = sum(debit)) %>%
            arrange(desc(y)) %>%
            filter(row_number() <= n_plot) %>%
            pull(category)
        x %<>% filter(category %in% top_cats)
        all_dates <- x$date %>% unique()
        all_cats <- x$category %>% unique()

        x %<>%
            full_join(
                expand.grid(all_dates, all_cats, stringsAsFactors = FALSE) %>%
                    rename(date = Var1, category = Var2)
            ) %>%
            replace_na(list(debit = 0)) %>%
            group_by(category, date) %>%
            arrange(desc(date)) %>%
            summarise(y = sum(debit))
    } else {
        top_descs <- x %>%
            summarise(y = sum(debit)) %>%
            arrange(desc(y)) %>%
            filter(row_number() <= n_plot) %>%
            pull(description)
        x %<>% filter(description %in% top_descs)
        all_dates <- x$date %>% unique()
        all_descs <- x$description %>% unique()

        x %<>%
            full_join(
                expand.grid(all_dates, all_descs, stringsAsFactors = FALSE) %>%
                    rename(date = Var1, description = Var2)
            ) %>%
            replace_na(list(debit = 0)) %>%
            group_by(description, date) %>%
            arrange(desc(date)) %>%
            summarise(y = sum(debit))
    }

    if (cumulative) {
        x %<>% mutate(y = cumsum(y))
    }
    if (proportot %in% 'Proportions') {
        x %<>%
            group_by(date) %>%
            mutate(y = y / sum(y))
    }

    # Plot ~ proportot, catlevel, tempgrouping

    if (catlevel %in% 'Between') {
        p <- x %>% ggplot(aes(date, y, group = category, fill = category, colour = category))
    } else {
        p <- x %>% ggplot(aes(date, y, group = description, fill = description, colour = description))
    }
    if (proportot %in% 'Proportions') {
        p + geom_area() + xlab(timelab) + ylab('Proportion of spending')
    } else {
        p + geom_line() + xlab(timelab) + ylab('Amount ($)')
    }
}
