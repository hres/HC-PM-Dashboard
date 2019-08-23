# open libraries

library(shiny)
library(elastic)
library(dplyr)
library(tibble)
library(jsonlite)
library(DT)
library(shinycssloaders)
library(readr)
library(stringr)

# open connection to Elastic Search
connect(es_host = "elastic-gate.hc.local", es_port = 80, errors = "complete")

# generic function for getting a query result
get_query <- function(index, query_string, size) {
    data <- Search(index = index, body = query_string, size = size,
        raw = TRUE) %>%
        fromJSON()
    data <- data$hits$hits$`_source`
    return (data)
}

# generic function for getting all values of a field
get_values <- function(field) {
    query <- paste0('{"query":{"match_all":{}},"aggs":{"key":{"terms":{"field":"',
        field, '","size":"400000"}}}}')
    search_result <- Search(index = "dpd_drug", body = query)
    values <- search_result$aggregations$key$buckets %>%
        lapply("[" , "key") %>% 
        unlist() %>%
        unique() %>%
        sort()
    return(values)
}

# lists of some fields to use later
en_url_list <- get_values("product_monograph_en_url")
fr_url_list <- get_values("product_monograph_fr_url")
ingredients_list <- get_values("active_ingredients.keyword")
brand_list <- get_values("brand_name")
company_list <- get_values("company.company_name.keyword")
preds <- c("all categories", "adverse reactions", "contraindications",
    "geriatric use", "indications", "pediatric use", "pregnancy", "warnings")
dpd_fields <- c("active ingredients", "brand name", "company name",
    "product monograph number")