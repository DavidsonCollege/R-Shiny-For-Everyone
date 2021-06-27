#
# shinydashboard application for Week Two in the EdX R Shiny For Everyone Course
# featuring shiny widgets inputs, echarts4R graphics, DT datatable, and leaflet map
#
# Author: Owen Bezick
# 27 June 2021

library(shiny)
library(shinydashboard)
library(DT)
library(echarts4r)
library(leaflet)
library(shinyWidgets)

##### UI #####
ui <- dashboardPage(
  dashboardHeader(
    title = "Week Two"
  ),
  dashboardSidebar(
    actionBttn(
      "actionBttn",
      label = "Button",
      style = "pill",
      color = "default",
      size = "lg",
      block = F,
      no_outline = TRUE
    )
  ),
  dashboardBody(
    fluidRow(
      box(width = 6
          # Select input ----
          , selectInput("select", label = h3("Select box"), 
                      choices = list("Choice 1" = 1, "Choice 2" = 2, "Choice 3" = 3), 
                      selected = 1)
          
          , hr()
          # Select input output ----
          , fluidRow(column(3, verbatimTextOutput("value")))
          , br()
          # Slider input ----
          , sliderInput("slider1", label = h3("Slider"), min = 0, 
                      max = 100, value = 50)
          # Slider input output ----
          , verbatimTextOutput("slider_value")
          , br()
          # Text input  ----
          , textInput("text", label = h3("Text input"), value = "Enter text...")
          , hr()
          # Text input output ----
          , fluidRow(column(3, verbatimTextOutput("text_value")))
      )
      # Echarts4R outputs ----
      , tabBox(width = 6
               , tabPanel(title = "Scatter Plot"
                          , echarts4rOutput("scatter"))
               , tabPanel(title = "Bar Chart"
                          , echarts4rOutput("bar"))
      )
    )
    # DT Table output ----
    , fluidRow(
      box(width = 6
          , DTOutput("datatable")
      )
      # Leaflet output ----
      , box(width = 6
            , leafletOutput("map")
      )
    )
  ),
  title = "Dashboard example"
)

##### Server logic #####
server <- function(input, output) {
  # DT Table ----
  output$datatable <- renderDT({
    datatable(iris)
  })
  # Data for Server ----
  df <- data.frame(
    x = seq(50),
    y = rnorm(50, 10, 3),
    z = rnorm(50, 11, 2),
    w = rnorm(50, 9, 2)
  )
  # Scatter Plot ----
  output$scatter <- renderEcharts4r({
    df %>% 
      e_charts(x) %>% 
      e_scatter(y, z) %>% 
      e_visual_map(z, scale = e_scale) %>% # scale color
      e_legend(FALSE) # hide legend
  })
  # Bar and Step Chart ----
  output$bar <- renderEcharts4r({
    df %>% 
      e_charts(x) %>% 
      e_bar(y, name = "Serie 1") %>% 
      e_step(z, name = "Serie 2") %>% 
      e_title("Bar and step charts")
  })
  # Leaflet Map ----
  output$map <- renderLeaflet({
    m <- leaflet() %>%
      addTiles() %>%  # Add default OpenStreetMap map tiles
      addMarkers(lng=174.768, lat=-36.852, popup="The birthplace of R")
    m  # Print the map
  })
  # Shiny Widgets ----
  # You can access the value of the widget with input$select, e.g.
  output$value <- renderPrint({ input$select })
  # You can access the value of the widget with input$slider_value, e.g.
  output$slider_value <- renderPrint({ input$slider1 })
  # You can access the value of the widget with input$text_value, e.g.
  output$text_value <- renderPrint({ input$text })
  # Modal Dialog ----
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
