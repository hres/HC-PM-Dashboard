htmlTemplate("index.html",
            sidebar = fluidPage(
                fluidRow(
                    selectInput("search_field", "Select category or field to search:",
                    c(preds, dpd_fields)), 
                    autocomplete_input("search_term", "Enter search term:", c(),
                placeholder = "leave blank to search all", max_options = 100),
                    actionButton("search_button", "Search"), downloadButton("download", "Download Filtered Data")
                ), br(), br(), fluidRow(
                    withSpinner(dataTableOutput("table"))
                )
    )
)

