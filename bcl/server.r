library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(tidyverse)
library(leaflet)
library(shinyjs)
library(RCurl)

## load the data (retrieve and clean raw data if this is the first time)
filename <- file.path("data", "bcl-data.csv")
if (file.exists(filename)) {
  bcl <- read.csv(filename, stringsAsFactors = FALSE)
} else {
  bcl <- read.csv("http://pub.data.gov.bc.ca/datasets/176284/BC_Liquor_Store_Product_Price_List.csv",
                  stringsAsFactors = FALSE)
  products <- c("BEER", "REFRESHMENT BEVERAGE", "SPIRITS", "WINE")
  bcl <- dplyr::filter(bcl, PRODUCT_CLASS_NAME %in% products) %>%
    dplyr::select(-PRODUCT_TYPE_NAME, -PRODUCT_SKU_NO, -PRODUCT_BASE_UPC_NO,
                  -PRODUCT_LITRES_PER_CONTAINER, -PRD_CONTAINER_PER_SELL_UNIT,
                  -PRODUCT_SUB_CLASS_NAME) %>%
    rename(Type = PRODUCT_CLASS_NAME,
           Subtype = PRODUCT_MINOR_CLASS_NAME,
           Name = PRODUCT_LONG_NAME,
           Country = PRODUCT_COUNTRY_ORIGIN_NAME,
           Alcohol_Content = PRODUCT_ALCOHOL_PERCENT,
           Price = CURRENT_DISPLAY_PRICE,
           Sweetness = SWEETNESS_CODE)
  bcl$Type <- sub("^REFRESHMENT BEVERAGE$", "REFRESHMENT", bcl$Type)
  dir.create("data", showWarnings = FALSE)
  write.csv(bcl, filename, row.names = FALSE)
}

## load json for countries boundaries
WorldCountry <-geojsonio::geojson_read("./data/countries.geo.json", what = "sp")

server <- function(input, output, session) {
  ## render input box for country
  output$countrySelectorOutput <- renderUI({
    selectInput("countryInput", "Country",
                sort(unique(bcl$Country)),
                selected = "CANADA")
  })
  
  ## render input box for product type
  output$typeSelectOutput <- renderUI({
    selectInput("typeInput", "Product type",
                sort(unique(bcl$Type)),
                multiple = TRUE,
                selected = c("BEER", "WINE"))
  })
  
  ## render slider for sweetness
  output$sweetnessOutput <- renderUI({
    sliderInput("sweetness", "Sweetness of wine", 0, 10, value = c(0, 10))
  })

  ## render input box for subtype
  output$subtypeSelectOutput <- renderUI({
    selectInput("subtypeInput", "Product subtype",
                sort(unique(bcl$Subtype[bcl$Type %in% input$typeInput])),
                multiple = TRUE)
  })
  
  ## a conditionalPanel for ascending or descending ordering of price
  output$PriceSortOutput <- renderUI({
    radioButtons("priceSortOrder", "Ascending/Descending",
                 c("Ascending" = "asce",
                   "Descending" = "desc"))
  })
  
  ## get number of options seleteced (originally implemented)
  output$summaryText <- renderText({
    numOptions <- nrow(prices())
    if (is.null(numOptions)) {
      numOptions <- 0
    }
    paste0("We found ", numOptions, " options for you!")
  })
  
  ## generate price table
  prices <- reactive({
    prices <- bcl
    
    if (is.null(input$countryInput)) {
      return(NULL)
    }
    
    # filter by type
    prices <- dplyr::filter(prices, Type %in% input$typeInput)
    
    # filter by sweetness
    if (length(input$typeInput) == 1 && input$typeInput[1] == "WINE") {
      prices <- dplyr::filter(prices, Sweetness >= input$sweetness[1],
                              Sweetness <= input$sweetness[2])
    }
    
    # filter by subtype
    if (length(input$subtypeInput) != 0) {
      prices <- dplyr::filter(prices, Subtype %in% input$subtypeInput)
    }
    
    # filter by country
    if (input$filterCountry) {
      prices <- dplyr::filter(prices, Country == input$countryInput)
    }
    # filter by range of price
    prices <- dplyr::filter(prices, Price >= input$priceInput[1],
                            Price <= input$priceInput[2])
    # add an option to sort the results table by price
    if (input$sortByPrice) {
      if (input$priceSortOrder == "asce") {
        prices <- dplyr::arrange(prices, Price)
      } else {
        prices <- dplyr::arrange(prices, dplyr::desc(Price))
      }
    }
    
    if(nrow(prices) == 0) {
      return(NULL)
    }
    
    # final data frame
    prices
  })
  
  output$plot <- renderPlotly({
    if (is.null(prices())) {
      return(NULL)
    }
    
    gp <- ggplot(prices(), aes(prices()[, input$plotType], fill = Type)) +
      # alpha controlled by UI
      geom_histogram(colour = "black", alpha = input$plotAlpha) +
      # color scheme controlled by UI
      scale_fill_brewer(
        palette = input$fillBrewer
      ) +
      # modify label
      labs(
        x = input$plotType,
        y = "Count"
      ) +
      theme_bw()
    
    plotly::ggplotly(gp)
  })
  
  ## interative table (originally implemented)
  output$prices <- DT::renderDataTable({
    prices()
  }, selection = 'single')
  
  ## download button (originally implemented)
  output$download <- downloadHandler(
    filename = function() {
      "bcl-results.csv"
    },
    content = function(con) {
      write.csv(prices(), con)
    }
  )
  
  ## leaflet map
  output$map <- renderLeaflet({
    if (is.null(prices())) {
      return(NULL)
    }
    
    # modify prices data frame to get info we want
    prices_for_map <- prices() %>%
      # group by country and count
      group_by(Country) %>%
      # count and calculate average price
      summarise(
        Count = n(),
        AvgPrice = mean(Price)
      )
    
    if (is.null(prices_for_map)) {
      return(NULL)
    }
    
    # copy a S4 object
    WorldCountry_for_map <- WorldCountry
    
    # convert data to upper case and same format
    prices_for_map <- prices_for_map %>% 
      # mutate to name
      mutate(
        name = toupper(Country)
      ) %>% 
      # delete column Country
      select(-Country)
    
    WorldCountry_for_map@data <- WorldCountry_for_map@data %>% 
      mutate(
        name = toupper(name)
      )
    
    # filter not found country
    WorldCountry_for_map <- WorldCountry_for_map[WorldCountry_for_map$name %in% prices_for_map$name, ]
    
    # left join to combine two data frames
    WorldCountry_for_map@data <- left_join(WorldCountry_for_map@data, prices_for_map, by = "name")
    
    # define color mappings
    pal <- colorNumeric(palette = input$mapColor, domain = WorldCountry_for_map$Count)
    
    # create leaflet map
    map <- leaflet(WorldCountry_for_map) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~pal(Count),
        weight = 1,
        opacity = 0.7,
        color = "black",
        fillOpacity = 1,
        label = stringr::str_c(
          WorldCountry_for_map$name,
          " | Number of liquor: ", WorldCountry_for_map$Count,
          " | Average price: ", paste0("$", formatC(as.numeric(WorldCountry_for_map$AvgPrice), format="f", digits=2, big.mark=","))
          )
        ) 
    
    # show final map
    clearBounds(map)
  })
  
  ## show plot and table (baed on whether fold or not)
  output$showResults <- renderUI({
   if (input$foldResults) {
     tabsetPanel(id = "resultsTabs", type = "tabs",
       # tabPanel for plot
       tabPanel("Plot",
        plotlyOutput("plot")
       ),
       # tabPanel for table
       tabPanel("Table",
        DT::dataTableOutput("prices"),
        # search button
        actionButton("search", "Search selected row on Google", icon = icon("search", lib = "glyphicon")),
        # download button (originally implemented)
        downloadButton("download", "Download results")
       ),
       # tabPanel for map
       tabPanel("Where are your liquor from?",
        leafletOutput("map", height = 800)
       )
     )
   } else {
     tabsetPanel(id = "resultsTabs", type = "tabs",
       # tabPanel for plot and table
       tabPanel("Plot & Table",
         plotlyOutput("plot"),
         DT::dataTableOutput("prices"),
         # search button
         actionButton("search", "Search selected row on Google", icon = icon("search", lib = "glyphicon")),
         # download button (originally implemented)
         downloadButton("download", "Download results")
       ),
       # tabPanel for map
       tabPanel("Where are your liquor from?",
         leafletOutput("map", height = 800)
       )
     )
   }
  })
  
  ## search function
  observeEvent(input$search, {
    # read name of liquor
    selectedRow <- input$prices_rows_selected
    
    if (length(selectedRow)) {
      liquor_name <- prices()[selectedRow,4]
      # encode URL
      url <- paste0("https://google.com/search?q=",curlEscape(liquor_name))
      print(url)
      # use runjs in shinyjs to open new window with Google
      runjs(paste0("window.open('", url, "')"))
    }
  })
}