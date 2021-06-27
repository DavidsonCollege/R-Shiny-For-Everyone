#
# Property Tracker shinydashboard application featuring a custom UI using a CSS style sheet
# shinyWidgets inputs, echarts4R graphics, DT datatables, leaflet map, and Google Sheets API connection.
# 
#
# Author: Owen Bezick
#

# Dependencies ----
library(shiny)
library(shinydashboard)
library(DT)
library(echarts4r)
library(leaflet)
library(shinyWidgets)
library(googlesheets4)
library(scales)
library(dplyr)

# Define UI for template application
ui <- dashboardPage(skin = "black",
  dashboardHeader(
    title = "Property Tracker"
  ),
  dashboardSidebar(disable = T
  ),
  dashboardBody(
    includeCSS("www/style.css")
    , chooseSliderSkin( skin = "Shiny", color = "black")
    , fillPage(
      fluidRow(
        box(width = 6, height = "40vh", title = "Inputs", status = "primary"
            , column(width = 6
                     , uiOutput("propertyType")
                     , br()
                     , uiOutput("propertyPrice")
                     , br()
                    , uiOutput("countBedroom")
            )
            , column(width = 6
                     , HTML("<b>Property Types Selected:</b>")
                     , verbatimTextOutput("type_value")
                     , br()
                     , br()
                     , HTML("<b>Maximum Property Price:</b>")
                     , verbatimTextOutput("price_value")
                     , br()
                     , br()
                     , HTML("<b>Minimum Bedroom Count:</b>")
                     , verbatimTextOutput("bedroom_value")
            )
        )
        , tabBox(width = 6, height = "40vh"
                 , tabPanel(title = NULL, icon = icon("dollar")
                            , echarts4rOutput("scatter", height = "30vh"))
                 , tabPanel(title = NULL, icon = icon("bed")
                            , echarts4rOutput("bar", height = "30vh"))
        )
      )
      , fluidRow(
        box(width = 6, height = "45vh", status = "primary"
            , DTOutput("property_data")
        )
        , box(width = 6, height = "45vh", status = "primary"
              , leafletOutput("map", height = "43vh")
        )
      )
    )
  ),
  title = "Property Tracker"
)

# Server logic
server <- function(input, output) {
  
  # Google Sheets API authentication flow ----
  # options(gargle_oauth_cache = ".cache") # designate project-specific cache
  # gargle::gargle_oauth_cache() # check the value of the option
  # googlesheets4::gs4_auth()# trigger auth on purpose to store a token in the specified cache
  cache_directory <- ".cache/" # can add to config file
  # list.files(cache_directory) # see your token file in the cache
  # googlesheets4::gs4_deauth() # de auth
  
  gs4_auth(email = "YOUR EMAIL", cache = cache_directory)
  
  property_data <- range_read(ss = "YOUR SHEET URL")
  
  # Filters ----
  output$propertyType <- renderUI({
    choices <- unique(property_data$property_type)
    selectInput("propertyType", label = "Property Type", 
                   choices = choices, 
                   selected = choices,
                multiple = T)
  })
  
  output$propertyPrice <- renderUI({
    min <- min(property_data$property_price)
    max <- max(property_data$property_price)
    sliderInput("propertyPrice", label = "Property Price"
                , min = min, 
                  max = max
                , value = max)
  })
  
  output$countBedroom <- renderUI({
    min <- min(property_data$count_bed)
    max <- max(property_data$count_bed)
    numericInput("countBedroom", label = "Count Bedroom"
                 , min = min, 
                 max = max
                 , value = min)
  })
  
  # Reactive property data ----
  reactive_property_data <- reactive({
    req( input$propertyType, input$propertyPrice, input$countBedroom)
    property_data %>%
      filter(
        property_type %in% input$propertyType
        , property_price <= input$propertyPrice
        , count_bed >= input$countBedroom
      )
  })
  
  # DT Table ----
  output$property_data <- renderDT({
    datatable(reactive_property_data()
              , rownames = F # removes first column of rownames
              , colnames = c("Property ID", "Property Type", "Bedroom Count", 
                             "Property Area", "Area Unit", "Property Price"
                             , "Latitude", "Longitude", "Address")
              , options = list(scrollY = "30vh", scrollX = "100%" # set height and enable scrolling
                               , autoWidth = T
                               , columnDefs = list(list(width = '200px', targets = c(8))))) %>% # define column width for column number 8
      # currency formatter 
      formatCurrency(columns = c(6))
  })
  

  # Graphics ----
  output$scatter <- renderEcharts4r({
    reactive_property_data() %>%
      e_charts(property_id) %>%
      e_scatter(property_price, name = "Property Price", symbol_size = 15, color = "black") %>%
      e_axis_labels(y = "Price", x = "ID") %>%
      e_legend(show = F) %>%
      e_labels(
        position = "top"
        , color = "#111"
        # Custom JS formatter
        , formatter = htmlwidgets::JS("function(params){ 
                                      return('$' + parseInt(params.value[1]/1000000) + 'M')
                                      }")) %>%
     e_tooltip(
        position = "right"
        # Custom JS formatter
        , formatter = htmlwidgets::JS("
             function(params){
              var formatter = new Intl.NumberFormat('en-US', { style: 'currency', currency: 'USD'});
              
               return( 'Property ID: ' + params.value[0]+ '<br/>' + 'Property Price: ' + formatter.format(params.value[1]/ 1000000)+ 'M')
            }")
      ) %>%
      e_y_axis(formatter = e_axis_formatter(style = "currency")) %>% 
      e_x_axis(axisLabel = list(interval = 1)) 
  })
  
  
  output$bar <- renderEcharts4r({
    reactive_property_data() %>%
      e_charts(property_id) %>%
      e_bar(count_bed, color = "black", barWidth = 10) %>%
      e_legend(show = F) %>%
      e_tooltip(
        position = "right"
        # Custom JS formatter
        , formatter = htmlwidgets::JS("
             function(params){
              var formatter = new Intl.NumberFormat('en-US', { style: 'currency', currency: 'USD'});
              
               return( 'Property ID: ' + params.value[0]+ '<br/>' + 'Bedroom Count: ' + params.value[1])
            }")
      ) %>%
      e_axis_labels(y = "Bedroom Count", x = "ID")
  })
  
  output$map <- renderLeaflet({
    reactive_property_data() %>%
      mutate(popup = paste( # Mutate a column for popup text
        "<center> <b> Address </b>"
        , "<br>"
        , address
        , "</center>"
      )
      ) %>%
      leaflet() %>%
      addProviderTiles(provider = "Esri.WorldImagery") %>% # satallite imagery provider tile
      addMarkers(lng = ~long
                 , lat = ~lat
                 , label = ~property_id
                 , popup = ~popup)
  })
  
  # Filter outputs 
  output$type_value <- renderPrint({ input$propertyType })
  output$price_value <- renderPrint({ dollar(input$propertyPrice) })
  output$bedroom_value <- renderPrint({ input$countBedroom })
  
  # Action button observe event handler
  observeEvent(input$actionBttn, {
    # showNotification("Action Bttn Clicked!")
    showModal(
      modalDialog( title = "Action Button Clicked", easyClose = T, footer = NULL
                   , fluidRow(
                     box(width = 12, status = "primary"
                         , "Hello World!"
                     )
                   )
      )
    )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
