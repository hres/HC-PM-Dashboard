shinyServer(function(input, output, session) {
    
    active_data <- eventReactive(input$search_button, {
        
        if (input$search_term == "") {
            search_term <- "*"
        } else {
            search_term <- input$search_term
        }
        
        search_field <- case_when(
            input$search_field == "all categories" ~ "*",
            input$search_field == "adverse reactions" ~ "adverse_reactions",
            input$search_field == "contraindications" ~ "contraindication",
            input$search_field == "geriatric use" ~ "geriatric_use",
            input$search_field == "pediatric use" ~ "pediatric_use",
            input$search_field == "warnings" ~ "WARNINGS",
            input$search_field == "active ingredients" ~ "active_ingredients",
            input$search_field == "brand name" ~ "brand_name",
            input$search_field == "company name" ~ "company.company_name",
            TRUE ~  input$search_field
        )
        
        print(paste0(search_term, ", ", search_field))
        
        if (input$search_field %in% preds) {
            query <- paste0('{"query":{"query_string":{"query":"preds_combined:',
                search_field, ' AND ', search_term, '"}},',
                '"aggs":{"key":{"terms":{"field":"pm.keyword","size":"400000"}}}}')
            search_result <- Search(index = "indication2", body = query)
            key <- search_result$aggregations$key$buckets %>%
                lapply("[" , "key") %>% 
                unlist() %>%
                unique()
            if (is.null(key))
                return(data.frame(brand = "", ingredients = "", url = ""))
            urls <- data.frame(key, stringsAsFactors = FALSE) %>%
                mutate(en_url = paste0("https://pdf.hres.ca/dpd_pm/", key, ".PDF"))
        } else {
            query <- paste0('{"query":{"query_string":{"query":"',
                search_field, ' LIKE ', search_term, '"}},',
                '"aggs":{"key":{"terms":{"field":"product_monograph_en_url",',
                '"size":"400000"}}}}')
            search_result <- Search(index = "dpd_drug", body = query)
            en_url <- search_result$aggregations$key$buckets %>%
                lapply("[" , "key") %>% 
                unlist() %>%
                unique()
            if (is.null(en_url))
                return(data.frame())
            urls <- data.frame(en_url, stringsAsFactors = FALSE) %>%
                mutate(key = substr(en_url, 28, 35))
        }
        
        data <- data.frame()
        n <- nrow(urls) %/% 1000
        
        for (i in 0:n) {
            
            if (i == n) {
                urls_subset <- urls %>% slice((i * 1000 + 1):nrow(urls))
            } else {
                urls_subset <- urls %>% slice(i * 1000 + 1:1000)
            }
            
            query_string <- paste0('{"query":{"bool":{"should":[{"terms":{',
                '"product_monograph_en_url":["',
                paste(urls_subset$en_url, collapse = '","'), '"]}}]}}}')
            query_result_dpd <- get_query("dpd_drug", query_string, 10000)
            
            query_result_dpd <- query_result_dpd %>%
                select(product_monograph_en_url, active_ingredients, brand_name) %>%
                distinct(product_monograph_en_url, .keep_all = TRUE) %>%
                left_join(urls, c("product_monograph_en_url" = "en_url"), keep = TRUE)
            
            query_string <- paste0('{"query":{"bool":{"should":[{"terms":{',
                '"pm":["', paste(urls_subset$key, collapse = '","'), '"]}}]}}}')
            query_result_part3 <- get_query("part3textml", query_string, 2700)
            
            if (is.null(query_result_part3)) {
                query_result_part3 <- data.frame(pm = c(""),
                    nonmedingredients = c(""), url = c(""),
                    stringsAsFactors = FALSE)
            } else { 
                query_result_part3 <- query_result_part3 %>%
                select(pm, nonmedingredients, url)
            }
            
            data <- query_result_dpd %>%
                left_join(query_result_part3, c("key" = "pm")) %>%
                rowwise() %>%
                mutate(ingredients = toString(unlist(active_ingredients)),
                    nonmedingredients = toString(unlist(nonmedingredients)),
                    url = ifelse(is.na(url), product_monograph_en_url, url)) %>%
                select(brand = brand_name, ingredients, nonmedingredients,
                    url) %>%
                bind_rows(data)
    
        }
        
        data <- data %>%
            arrange(url)
            
        return (data)
    })
    
    output$table <- renderDataTable(mutate(active_data(),
            url = paste0("<a href='", url,"' target='_blank'>", url,"</a>")),
            rownames = FALSE, colnames = c("Brand Name (DPD)",
            "Active Ingredient(s) (DPD)", "Non Medical Ingredients",
            "Product Monograph Link"), selection = "none", escape = FALSE)
    
    output$download <- downloadHandler(
        filename = "Product_Monographs.csv",
        content = function(file) {
            write_csv(active_data()[input$table_rows_all,], file)
        }
    )

})