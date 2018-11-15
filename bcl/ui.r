library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(shinydashboard)
library(shinyjs)

ui <- dashboardPage(skin = "black",
  dashboardHeader(
    title = "BC Liquor store prices",
    titleWidth = 300
  ),
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      menuItem("Welcome!", tabName = "index", icon = icon("home", lib = "glyphicon")),
      menuItem("Find my liquor!", tabName = "database", icon = icon("glass", lib = "glyphicon")),
      menuItem("Settings", startExpanded = TRUE, icon = icon("cog", lib = "glyphicon"),
        tabsetPanel(id = "optionTabs", type = "tabs",
          ## tabPanel for sort and filter
          tabPanel("Sort & Filter", icon = icon("search", lib = "glyphicon"),
                   # filter by range of price
                   sliderInput("priceInput", "Price", 0, 100, c(25, 40), pre = "$"),
                   # sort by price
                   checkboxInput("sortByPrice", "Sort by price", FALSE),
                   # a conditionalPanel for ascending or descending ordering
                   conditionalPanel(
                     condition = "input.sortByPrice",
                     uiOutput("PriceSortOutput")),
                   # filter by product type
                   uiOutput("typeSelectOutput"),
                   # filter by sweetness
                   conditionalPanel(
                     condition = "input.typeInput == 'WINE'",
                     uiOutput("sweetnessOutput")
                   ),
                   # filter by subtype
                   uiOutput("subtypeSelectOutput"),
                   # filter by country
                   checkboxInput("filterCountry", "Filter by country", FALSE),
                   conditionalPanel(
                     condition = "input.filterCountry",
                     uiOutput("countrySelectorOutput")
                   )
          ),
          ## tabPanel for changing appearance
          tabPanel("Appearance", icon = icon("heart", lib = "glyphicon"),
                   # provide different plots
                   radioButtons("plotType", "Plot type",
                                c("Alcohol Content" = "Alcohol_Content",
                                  "Price" = "Price")),
                   # add alpha parameter to the plot
                   sliderInput("plotAlpha", "Alpha of bars", 0, 1, value = 0.7),
                   # add color parameter to the plot
                   radioButtons("fillBrewer", "Color scheme for plot",
                                c("Set3" = "Set3",
                                  "Set2" = "Set2",
                                  "Set1" = "Set1",
                                  "Pastel2" = "Pastel2",
                                  "Paired" = "Paired",
                                  "Dark2" = "Dark2",
                                  "Accent" = "Accent")),
                   # add color parameter to the map
                   radioButtons("mapColor", "Color scheme for map",
                                c("Red" = "Reds",
                                  "Purple" = "Purples",
                                  "Orange" = "Oranges",
                                  "Grey" = "Greys",
                                  "Green" = "Greens",
                                  "Blue" = "Blues")),
                   # fold plot and table into tabs
                   checkboxInput("foldResults", "Fold plot and table into tabs", FALSE)
          )
        )
      )
    )
  ),
  dashboardBody(
    # include shinyjs
    useShinyjs(),
    # include CSS
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    # define a function to open tab
    tags$script(HTML("
        var openTab = function(tabName){
          $('a', $('.sidebar')).each(function() {
            if(this.getAttribute('data-value') == tabName) {
              this.click()
            };
          });
        }
      ")),
    # tabItems
    tabItems(
      # welcome page
      tabItem(tabName = "index",
        fluidRow(
          # embed gif as logo
          div(id="logo",
              img(src = "logo.gif")
          ),
          # description
          h4(
            "Had a long day?  This app will help you find the right drink for tonight! Just click on ",
            a("Find my liquor", onclick = "openTab('database')", href = "#"),
            " and use the filters at the left!"
          ),
          ## license
          hr(), br(), br(),
          em(
            span("Data source:", 
                 tags$a("OpenDataBC",
                        href = "https://www.opendatabc.ca/dataset/bc-liquor-store-product-price-list-current-prices")),
            br(), 
            span("Improvded by Junbin ZHANG, Created by ", a(href = "https://github.com/daattali/shiny-server/tree/master/bcl", "Dean Attali"))
          )
      )),
      # data page
      tabItem(tabName = "database",
        fluidRow(
          h3(textOutput("summaryText")),
          br(),
          uiOutput("showResults")
        ))
    )
  )
)