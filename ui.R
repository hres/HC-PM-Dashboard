htmlTemplate("index.html",
    sidebar = fluidPage(
        fluidRow(
            selectInput("search_field", "Select category or field to search:",
                c(preds, dpd_fields)),
            textInput("search_term", "Enter search term:", "",
                placeholder = "leave blank to search all"),
            actionButton("search_button", "Search"),
            downloadButton("download", "Download Filtered Data")
        ),
        br(),
        br(),
        fluidRow(
            withSpinner(dataTableOutput("table"))
        )
    )
)