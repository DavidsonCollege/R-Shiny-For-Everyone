library(shinydashboard)

ui <- dashboardPage(
    dashboardHeader(title = "National Parks")
    , dashboardSidebar(
        menuItem(tabName = "home", text = "Home")
       , menuItem(tabName = "map", text = "Map")
       , menuItem(tabName = "journal", text = "Journal")
    )
    , dashboardBody(
        tabItems(
            tabItem(
                
            )
        )
    )
    , title = "National Parks"
)

server <- function(input, output) {
    library(readr)
    parks <- read_csv("national_parks/data/park_biodiversity_data/parks.csv")
    species <- read_csv("national_parks/data/park_biodiversity_data/species.csv")
}


shinyApp(ui = ui, server = server)
