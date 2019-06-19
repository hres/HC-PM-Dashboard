fluidPage(

    titlePanel("Product Monograph Dashboard"),
    
    br(),

    sidebarLayout(
      
        sidebarPanel(
            selectInput("search_field", "Select category or field to search:",
                c(preds, dpd_fields)),
            textInput("search_term", "Enter search term:", "", NULL,
                "leave blank to search all"),
            br(),
            br(),
            actionButton("search_button", "Search"),
            br(),
            br(),
            br(),
            downloadButton("download", "Download Filtered Data")
        ),

        mainPanel(
            withSpinner(dataTableOutput("table"))
        )
        
    )
  
)