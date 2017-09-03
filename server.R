library(shiny)
source('initialise.R')

shinyServer(function(input, output) {

    # Plots
    output$temporalplot <- renderPlot({
        cats <- if (is.null(input$cats)) {
            allcats
        } else {
            input$cats
        }
        descs <- if (is.null(input$descs)) {
            debits %>%
                filter(category %in% cats[1]) %>%
                pull(description)
        } else {
            input$descs
        }
        temporal_plot(
            debits,
            input$tempgrouping,
            input$proportot,
            input$catlevel,
            input$daterange[1],
            input$daterange[2],
            input$cumulative,
            as.integer(input$nplot),
            cats,
            descs
        )
    })
    output$summaryplot <- renderPlot({
        cats <- if (is.null(input$cats)) {
            allcats
        } else {
            input$cats
        }
        descs <- if (is.null(input$descs)) {
            debits %>%
                filter(category %in% cats[1]) %>%
                pull(description)
        } else {
            input$descs
        }
        summary_plot(
            debits,
            input$proportot,
            input$catlevel,
            input$daterange[1],
            input$daterange[2],
            as.integer(input$nplot),
            cats,
            descs
        )
    })
    output$uncatreport <- renderText({
        ucr <- uncat_report(debits, input$daterange[1], input$daterange[2])
        sprintf("%s of transactions in this period (%s of dollars spent) are uncategorised.", fmt_pct(ucr$uncat_trans), fmt_pct(ucr$uncat_total))
    })
    output$uio_cats <- renderUI({
        selectInput('cats', 'Select Categories', allcats, multiple = input$catlevel %in% 'Between', selectize = TRUE) # just allow selection of a single category if catlevel is 'Within'
    })
    output$uio_descs <- renderUI({
        alldescs <- debits %>%
            filter(category %in% input$cats[1]) %>%
            pull(description) %>%
            unique()
        if (input$catlevel %in% 'Between') {
            NULL
        } else {
            selectInput('descs', 'Select Descriptions', alldescs, multiple = TRUE, selectize = TRUE)
        }
    })
})
