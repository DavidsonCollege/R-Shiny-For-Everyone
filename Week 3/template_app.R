#
# This is a template shinydashboard application for Week Three in the EdX R Shiny
# For Everyon Course
#
# Author: Owen Bezick
# 27 June 2021

library(shiny)
library(shinydashboard)
library(DT)
library(echarts4r)
library(leaflet)
library(shinyWidgets)

# Define UI for template application
ui <- dashboardPage(
  dashboardHeader(
    title = "Week Two"
  ),
  dashboardSidebar(
    column(width = 12, align = "center"
           , actionBttn(
             "actionBttn",
             label = "Button",
             style = "pill",
             color = "default",
             size = "lg",
             block = F,
             no_outline = TRUE
           )
    )
  ),
  dashboardBody(
    fillPage(
      fluidRow(
        box(width = 6, height = "40vh", title = "Inputs"
            , column(width = 6
                     ,  selectInput("select", label = "Select box", 
                                    choices = list("Choice 1" = 1, "Choice 2" = 2, "Choice 3" = 3), 
                                    selected = 1)
                     , br()
                     , sliderInput("slider1", label = "Slider", min = 0, 
                                   max = 100, value = 50)
                     , br()
                     , textInput("text", label = "Text input", value = "Enter text...")
            )
            , column(width = 6
                     , br()
                     , verbatimTextOutput("value")
                     , br()
                     , br()
                     , br()
                     , verbatimTextOutput("slider_value")
                     , br()
                     , br()
                     , br()
                     , verbatimTextOutput("text_value")
            )
        )
        , tabBox(width = 6, height = "40vh"
                 , tabPanel(title = "Scatter Plot"
                            , echarts4rOutput("scatter", height = "30vh"))
                 , tabPanel(title = "Bar Chart"
                            , echarts4rOutput("bar", height = "30vh"))
        )
      )
      , fluidRow(
        box(width = 6, height = "40vh"
            , DTOutput("datatable")
        )
        , box(width = 6, height = "40vh"
              , leafletOutput("map", height = "38vh")
        )
      )
    )
  ),
  title = "Dashboard example"
)

# Server logic
server <- function(input, output) {
  output$datatable <- renderDT({
    datatable(iris, options = list(scrollY = "25vh"))
  })
  
  df <- data.frame(
    x = seq(50),
    y = rnorm(50, 10, 3),
    z = rnorm(50, 11, 2),
    w = rnorm(50, 9, 2)
  )
  
  output$scatter <- renderEcharts4r({
    df %>% 
      e_charts(x) %>% 
      e_scatter(y, z) %>% 
      e_visual_map(z, scale = e_scale) %>% # scale color
      e_legend(FALSE) # hide legend
  })
  output$bar <- renderEcharts4r({
    df %>% 
      e_charts(x) %>% 
      e_bar(y, name = "Serie 1") %>% 
      e_step(z, name = "Serie 2") %>% 
      e_title("Bar and step charts")
  })
  
  output$map <- renderLeaflet({
    m <- leaflet() %>%
      addTiles() %>%  # Add default OpenStreetMap map tiles
      addMarkers(lng=174.768, lat=-36.852, popup="The birthplace of R")
    m  # Print the map
  })
  
  output$value <- renderPrint({ input$select })
  output$slider_value <- renderPrint({ input$slider1 })
  output$text_value <- renderPrint({ input$text })
  
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
