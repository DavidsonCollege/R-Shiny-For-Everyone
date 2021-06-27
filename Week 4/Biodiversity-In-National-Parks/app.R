#
# This is a Shiny web application featuring data for Biodiversity in National Parks
#
# Author: Owen Bezick
#

library(shiny)
library(shinydashboard)
library(DT)
library(shinyWidgets)
library(dplyr)
library(echarts4r)
library(leaflet)
library(googlesheets4)

# Define UI for application that draws a histogram
ui <-  dashboardPage(
    dashboardHeader(
        title = "Biodiversity"
    ),
    dashboardSidebar( 
        sidebarMenu(
            menuItem(
                tabName = "species"
                , text = "Species"
            )
            , menuItem(
                tabName = "map"
                , text = "Map"
            )
            , menuItem(
                tabName = "journal"
                , text = "Journal"
            )
        )
    ),
    dashboardBody(
        includeCSS("www/style.css")
        , tabItems(
            tabItem(
                tabName ="species"
                , fluidPage(
                    fluidRow(
                        box(width = 4, status = "primary", title = "Filters"
                            , uiOutput("park_picker")
                            , div(class = "filter_box"
                                  , uiOutput("category_check_box"))
                        )
                        , box(width = 4, status = "primary", title = "Overview"
                              , uiOutput("vb_present")
                              , uiOutput("vb_concern")
                              , uiOutput("vb_endangered")
                        )
                        , box(width = 4, status = "primary", title = "Category Count"
                              , echarts4rOutput("category_count_bar", height = "33vh"))
                    )
                    , fluidRow(
                        box(width = 12, status = "primary", title = "Table"
                            , DTOutput("species_table"))
                    )
                )
            )
            , tabItem(
                tabName = "map"
                , fillPage(
                    fluidRow(
                        box(width = 12, status = "primary"
                            , chooseSliderSkin(skin = "Shiny", color = "green")
                            , uiOutput("acreage_slider"))
                    )
                    , fluidRow(
                        box(width = 12, status = "primary"
                            , leafletOutput("map", height = "75vh")
                        )
                    )
                )
            )
            , tabItem(
                tabName = "journal"
                , fluidPage(
                    fluidRow(
                        box(width = 12, status = "primary"
                            , actionBttn("add_entry", icon = icon("plus"))
                            , DTOutput("journal"))
                    )
                )
            )
        )
    ),
    title = "Biodiversity in National Parks"
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    parks_data <- read.csv("data/parks.csv")
    species_data <- read.csv("data/species.csv")
    
    # Species Tab -----
    # Filters -----
    output$park_picker <- renderUI({
        choices <- parks_data$Park.Name
        pickerInput("park_picker", "Select Park"
                    , choices = choices)
    })
    
    output$category_check_box <- renderUI({
        req(input$park_picker)
        categories <- species_data %>%
            filter(Park.Name == input$park_picker) %>%
            select(Category) %>% pull() %>% unique()
        categories <- categories[categories != ""]
        checkboxGroupButtons(
            "category_check_box"
            , label = "Species Category"
            , choices = categories
            , selected = categories
            , status = "default"
            , size = "normal"
            , direction = "vertical"
            , width = "100%"
        )
        
    })
    
    # Reactive Data -----
    reactive_species_data <- reactive({
        req(input$park_picker)
        req(input$category_check_box)
        
        species_data %>%
            filter(Park.Name == input$park_picker
                   , Category %in% input$category_check_box)
    })
    
    # Species Table -----
    output$species_table <- renderDT({
        reactive_species_data() %>%
            select("Common Name" = Common.Names
                   , "Scientific Name" = Scientific.Name
                   , Category
                   , Occurrence
                   , Nativeness
                   , Abundance
                   , "Conservation Status" = Conservation.Status) %>%
            datatable(rownames = F
                      , options = list(paging = F
                                       , scrollY = "30vh"
                                       , scrollX = "100%"))
    })
    
    # Value Boxes -----
    output$vb_present <- renderValueBox({
        present <- reactive_species_data() %>%
            filter(Occurrence == "Present") %>%
            nrow()
        
        valueBox(value = format(present, big.mark = ","), subtitle = "Present Species")
    })
    output$vb_concern <- renderValueBox({
        concern <- reactive_species_data() %>%
            filter(Conservation.Status == "Species of Concern") %>%
            nrow()
        
        valueBox(value = format(concern, big.mark = ","), subtitle = "Species of Concern")
    })
    output$vb_endangered <- renderValueBox({
        endangered <- reactive_species_data() %>%
            filter(Conservation.Status == "Endangered Species") %>%
            nrow()
        
        valueBox(value = format(endangered, big.mark = ","), subtitle = "Endangered Species")
    })
    
    # Category Count Bar ----
    output$category_count_bar <- renderEcharts4r({
        reactive_species_data() %>%
            select(Species.ID, Category) %>%
            group_by(Category) %>%
            table() %>%
            as.data.frame() %>%
            group_by(Category) %>%
            summarise(count = sum(Freq)) %>%
            e_chart(Category) %>%
            e_bar(count, name = "Count Category", color = "green") %>%
            e_legend(show = F) %>%
            e_tooltip() %>%
            e_axis(axisLabel = list(interval = 0, rotate = 45)) %>%
            e_grid(top = 10)
    })
    
    # Map Tab ----
    output$acreage_slider <- renderUI({
        max_acreage <- 8323148
        min_acreage <-  5550
        
        sliderInput("acreageSlider", "Acreage Slider"
                    , min_acreage, max_acreage
                    , value = c(min_acreage, max_acreage), post = " acres")
    })
    
    reactive_map_data <- reactive({
        req(input$acreageSlider)
        parks_data %>%
            filter(Acres >= input$acreageSlider[1]
                   , Acres <= input$acreageSlider[2])
    })
    output$map <- renderLeaflet({
        reactive_map_data() %>%
            mutate(
                acres_f = format(Acres, big.mark = ",")
                , popup = paste(
                "<b>"
                , Park.Name
                , "</b>"
                , "<br>"
                , State
                , "<br>"
                , acres_f
                , "acres"
            )) %>%
            leaflet() %>%
            addProviderTiles(provider = "OpenTopoMap") %>%
            addCircleMarkers(
                lng = ~Longitude
                , lat = ~Latitude
                , radius = ~Acres/150000
                , label = ~Park.Name
                , popup = ~popup
            ) %>%
            setView(lat = "44.00000", lng = "-120.50000", zoom = 3)
    })
    # Journal Tab ----
    # options(gargle_oauth_cache = ".cache") # designate project-specific cache
    # gargle::gargle_oauth_cache() # check the value of the option
    # googlesheets4::gs4_auth()# trigger auth on purpose to store a token in the specified cache
    cache_directory <- ".cache/" # can add to config file
    # list.files(cache_directory) # see your token file in the cache
    # googlesheets4::gs4_deauth() # de auth
    
    gs4_auth(email = "owbezick@davidson.edu", cache = cache_directory)
    journal_url <- "https://docs.google.com/spreadsheets/d/1PTh8ezbd28EbSlM7ipl0ednna18x7BJuqoEszOgwb3Y/edit#gid=0"
    journal_data <- range_read(ss = journal_url)
    
    r <- reactiveValues()
    r$journal_data <- journal_data
    output$journal <- renderDT({
        datatable(r$journal_data, rownames = F, options = list(scrollY = "75vh", paging = F))
    })
    
    observeEvent(input$add_entry, {
        showModal(
            modalDialog(title = "New Entry"
                        , footer = fluidRow(
                            column(width = 6
                                   , actionBttn("save", icon = icon("save"))
                                   )
                            , column(width = 6
                                     , actionBttn("dismiss", icon = icon("times"))
                                     )
                        )
                        , fluidRow(
                            column(width = 6
                                   , textInput("name", "Trip Name", value = "Trip Name")
                                   , textInput("date", "Trip Date", value = "Trip Date")
                                   )
                            , column(width = 6
                                     , textInput("parkName", "Park Name", value = "Park Name")
                                     , textAreaInput("notes", "Notes", value = "Notes...")
                                     )
                        )
                
            )
        )
    })
    observeEvent(input$dismiss, {
        removeModal()
    })
    observeEvent(input$save, {
        tripName <- input$name
        tripDate <- input$date
        parkName <- input$parkName
        notes <- input$notes
        
        new_entry <- tibble("Trip Name" = c(tripName)
                            , "Trip Date" = c(tripDate)
                            , "Park Name" = c(parkName)
                            , "Notes" = c(notes))
        
        sheet_append(ss = journal_url, new_entry)
        showNotification("Entry added", type = "message")
        r$journal_data <- r$journal_data %>% rbind(new_entry)
        removeModal()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
