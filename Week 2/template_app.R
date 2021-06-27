#
# Template shinydashboard application
#
# Author: Owen Bezick
# June 27 2021


library(shiny)
library(shinydashboard)

# UI ----
ui <- dashboardPage(
    dashboardHeader(
        title = "Week Two"
    ),
    dashboardSidebar(disable = T
    ),
    dashboardBody(
        fluidRow(
            box(width = 6)
            , box(width = 6)
        )
        , fluidRow(
            box(width = 6)
            , box(width = 6)
        )
    ),
    title = "Dashboard example"
)

# Server ----
server <- function(input, output) {
    
}

# Run the application 
shinyApp(ui = ui, server = server)
