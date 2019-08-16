htmlTemplate("index.html",
    sidebar = fluidPage(
        fluidRow(
            selectInput("search_field", "Select category or field to search:",
                c(preds, dpd_fields)),
            selectInput("search_language", "Select language:",
                c("English", "French")),
            textInput("search_term", "Enter search term:", "",
                placeholder = "leave blank to search all"),
            br(),
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