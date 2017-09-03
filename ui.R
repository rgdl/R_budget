library(shiny)
source('initialise.R')

shinyUI(fluidPage(
    theme = 'style.css',
    title = appname,
    h1(appname),
    fluidRow( # Plots
        column(8, # Temporal plot top left
               plotOutput('temporalplot')
        ),
        column(4, # Summary plot top right
               plotOutput('summaryplot')
        )
    ),
    hr(),
    fluidRow( # Options along the bottom
        column(3, # Temporal grouping
               radioButtons('tempgrouping', 'Temporal Grouping', list('Daily', 'Weekly', 'Monthly', 'Day of the week')),
               hr(),
               radioButtons('catlevel', 'Compare between or within categories?', list('Between', 'Within'))
        ),
        column(3, # Cumulative?
               radioButtons('cumulative', 'Cumulative?', list(yes = TRUE, no = FALSE)),
               hr(),
               radioButtons('proportot', 'Display Mode', list('Totals', 'Proportions'))
        ),
        column(3, # Number to display (Hide one or the other of these inputs depending on catlevel, and filter descriptions to just those in the selected cat)
               selectInput('nplot', 'Categories/descriptions to show:', 1:max_nplots, selected = min(10, max_nplots)),
               uiOutput('uio_cats'),
               uiOutput('uio_descs')
        ),
        column(3, # Date selector
               dateRangeInput('daterange', 'Date Range',
                              daterange[1], daterange[2],
                              daterange[1], daterange[2]
               ),
               textOutput('uncatreport'),
               br(),
               actionButton('addcats', 'Categorise new transactions', icon('file-text', 'fa-spin'))
        )
    )
))