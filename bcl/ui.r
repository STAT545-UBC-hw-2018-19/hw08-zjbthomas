library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)

ui <- fluidPage(
  #include CSS
  includeCSS("./www/style.css"),
  # embed gif into titlePanel
  titlePanel(
    img(src = "logo.gif")
  ),
  sidebarLayout(
    sidebarPanel(
      # description
      h4(
        "Had a long day?  This app will help you find the right drink for tonight! Just use the filters below..."
      ),
      br(),
      tabsetPanel(id = "optionTabs", type = "tabs",
        ## tabPanel for sort and filter
        tabPanel("Sort & Filter",
         # filter by range of price
         sliderInput("priceInput", "Price", 0, 100, c(25, 40), pre = "$"),
         # filter by product type
         uiOutput("typeSelectOutput"),
         # filter by country
         checkboxInput("filterCountry", "Filter by country", FALSE),
         conditionalPanel(
           condition = "input.filterCountry",
           uiOutput("countrySelectorOutput")
         ),
         # sort by price
         checkboxInput("sortByPrice", "Sort by price", FALSE),
         # a conditionalPanel for ascending or descending ordering
         conditionalPanel(
           condition = "input.sortByPrice",
           uiOutput("PriceSortOutput"))
        ),
        ## tabPanel for changing appearance
        tabPanel("Appearance",
         # add alpha parameter to the plot
         sliderInput("plotAlpha", "Alpha of bars", 0, 1, value = 0.7),
         # add color parameter to the plot
         radioButtons("fillBrewer", "Color scheme",
            c("Set3" = "Set3",
              "Set2" = "Set2",
              "Set1" = "Set1",
              "Pastel2" = "Pastel2",
              "Paired" = "Paired",
              "Dark2" = "Dark2",
              "Accent" = "Accent"))
        ),
        # fold plot and table into tabs
        checkboxInput("foldResults", "Fold plot and table into tabs", FALSE)
      ),
      ## license
      hr(),
      span("Data source:", 
        tags$a("OpenDataBC",
             href = "https://www.opendatabc.ca/dataset/bc-liquor-store-product-price-list-current-prices")),
      br(), br(),
      em(
        span("Improvded by Junbin ZHANG, Created by Dean Attali"),
        HTML("&bull;"),
        span("Original Code ", a(href = "https://github.com/daattali/shiny-server/tree/master/bcl", "on GitHub"))
      )
    ),
    mainPanel(
      h3(textOutput("summaryText")),
      # download button (originally implemented)
      downloadButton("download", "Download results"),
      br(),
      uiOutput("showResults")
    )
  )
)