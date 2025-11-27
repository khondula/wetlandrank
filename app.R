#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(sf)
library(DT)
library(leaflet)
library(shinyWidgets)


# Initial Initial processing ---------------------------------------

# Load data
load('data/testappdata.RData')

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Wetland Ranker"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("wetland_area",
                        label = "Wetland Area Range:",
                        min = 0,
                        max = 12001615,
                        value = c(1, 12001615)),
            
            checkboxGroupInput("countries",
                               label = "Select Countries:",
                               choices = list('China' = 'CHN', 
                                              'Spain' = 'ESP', 
                                              'UK' = 'GBR',
                                              'Korea' = 'KOR',
                                              'Australia' = 'AUS',
                                              'France' = 'FRA',
                                              'India' = 'IND',
                                              'Brazil' = 'BRA',
                                              'USA' = 'USA',
                                              'Chile' = 'CHL',
                                              'Canada' = 'CAN',
                                              'Mexico' = 'MEX',
                                              'Thailand' = 'THA'),
                               selected = c('CHN', 
                                            'ESP', 
                                            'GBR',
                                            'KOR',
                                            'AUS',
                                            'FRA',
                                            'IND',
                                            'BRA',
                                            'USA',
                                            'CHL',
                                            'CAN',
                                            'MEX',
                                            'THA'))
        ),

        # Show a plot of the generated distribution
        mainPanel(
          tabsetPanel(type = 'tabs',
                      tabPanel('Map', value = 1,
                               leafletOutput("leafletMap", height = 800),
                               verbatimTextOutput(outputId = "n_wetlands")),
                      tabPanel('Table', value = 2,
                               DT::dataTableOutput('table')))

        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  ramsar_pts_COI_selected <- reactive({
    
      ramsar_pts_COI_sf %>% 
        dplyr::filter(iso3 %in% input$countries) %>%
        dplyr::filter(area_off <= input$wetland_area[2],
                    area_off >= input$wetland_area[1])
 
    })
  
    output$n_wetlands <- renderText({
      glue::glue('{nrow(ramsar_pts_COI_selected())} wetlands out of 857 selected.')
    })
    
    output$table <- renderDataTable({
      ramsar_pts_COI_selected() %>%
        sf::st_drop_geometry() %>%
        dplyr::select(-officialna)
    })
    
    output$leafletMap <- renderLeaflet({
      
      ll <- leaflet() %>%
        addTiles(group = 'OpenStreetMap') %>%
        addProviderTiles(providers$Esri.WorldImagery, group = 'Satellite') %>%
        addMarkers(data = ramsar_pts_COI_selected(), 
                   clusterOptions = markerClusterOptions(),
                   popup = ~glue::glue('{namelabel} ({area_off} km2)
                                       {country_en} ({iso3})')) %>%
        addLayersControl(baseGroups = c('OpenStreetMap', 'Satellite'))
      
      ll
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
