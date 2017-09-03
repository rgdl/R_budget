# Inputs that could effect this plot:

# Compare between or within categories?
# Proportions vs totals
# If between cats, number and names of cats
# If within cats, number and names of descriptions
# Date selector

summary_plot <- function(x, proportot = 'Proportions', catlevel = 'Between', min_date = min(x$date), max_date = max(x$date), n_plot = 7, cats = unique(x$category), descs = unique(x$description)) {
    # filter and summarise data ~ catlevel, n_plot, cats, descs, min_date, max_date

    x %<>% filter_debits(catlevel, min_date, max_date, cats, descs)
    x %<>%
        summarise(y = sum(debit)) %>%
        arrange(desc(y)) %>%
        filter(row_number() <= n_plot)

    # plot ~ proportot
    if (proportot %in% 'Proportions') {
        if (catlevel %in% 'Between') {
            p <- x %>%
                ggplot(aes(x = " ", y, fill = category))
        } else {
            p <- x %>%
                ggplot(aes(x = " ", y, fill = description))
        }
        p + geom_bar(width = 1, colour = '#404040', stat = 'identity') +
            coord_polar(theta = 'y') +
            theme(axis.title = element_blank(),
                  axis.text = element_blank())
    } else {
        if (catlevel %in% 'Between') {
            p <- x %>%
                ggplot(aes(x = category, y, fill = category))
        } else {
            p <- x %>%
                ggplot(aes(x = description, y, fill = description))
        }
        p + geom_bar(width = 1, colour = '#404040', stat = 'identity') +
            ylab('Amount ($)') +
            theme(
                axis.text.x = element_blank(),
                axis.title.x = element_blank(),
                axis.ticks.x = element_blank()
            )
    }
}
