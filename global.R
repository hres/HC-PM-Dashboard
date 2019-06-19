library(shiny)
library(elastic)
library(dplyr)
library(tibble)
library(jsonlite)
library(DT)
library(shinycssloaders)
library(readr)

# open connection to Elastic Search server
connect(es_host = "elastic-gate.hc.local", es_port = 80, errors = "complete")

get_query <- function(index, query_string, size) {
    
    data <- Search(index = index, body = query_string, size = size,
        raw = TRUE) %>%
        fromJSON()
    data <- data$hits$hits$`_source`
    return (data)
    
}

preds <- c("all categories", "adverse reactions", "contraindications",
    "geriatric use", "indications", "pediatric use", "pregnancy", "warnings")
dpd_fields <- c("active ingredients", "brand name", "company name")