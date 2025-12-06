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
      numericInput('weight_area', 
                   label = 'Scoring weight for Wetland area',
                   value = 0.5),
      numericInput('weight_plan', 
                   label = 'Scoring weight for Management plan implemented',
                   value = 0.5),
      sliderInput("wetland_area",
                  label = "Wetland Area Range:",
                  min = 0,
                  max = 12001615,
                  value = c(1, 12001615)),
      checkboxGroupInput("RamsarWetlandCategory",
                         label = "Ramsar Wetland Categories:",
                         choices = list('Only Inland' = 'type_I',
                                        'Inland & Coastal/Marine' = 'type_IM',
                                        'Inland & Human-made' = 'type_IH',
                                        'Inland & Coastal/Marine & Human-made' = 'type_IMH',
                                        'Only Coastal/Marine' = 'type_M',
                                        'Only Human-made' = 'type_H',
                                        'Coastal/Marine & Human-made' = 'type_MH'),
                         selected = c('type_IH', 'type_I', 'type_IM', 'type_IMH')),
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
                                      'THA')),
      actionButton("apply_filters", "Apply filters & weights")
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(type = 'tabs',
                  tabPanel('Map', value = 1,
                           verbatimTextOutput(outputId = "n_wetlands"),
                           leafletOutput("leafletMap", height = 800)
                           ),
                  tabPanel('Top30', value = 2,
                           plotOutput('barplot', height = '800px')),
                  tabPanel('Table', value = 3,
                           DT::dataTableOutput('table')))
      
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # This reactive only updates when the button is clicked:
  ramsar_pts_COI_selected <- eventReactive(input$apply_filters, {

    weight_sum <- input$weight_plan + input$weight_area
    weight_plan_norm <- input$weight_plan/weight_sum
    weight_area_norm <- input$weight_area/weight_sum
    
    df <- ramsar_pts_COI_sf_wInfo %>%
      dplyr::filter(iso3 %in% input$countries) %>%
      dplyr::filter(wetland_category %in% input$RamsarWetlandCategory) %>%
      dplyr::filter(area_off <= input$wetland_area[2],
                    area_off >= input$wetland_area[1]) %>%
      dplyr::mutate(Plan = ManagementPlanImplemented == 'Yes') %>%
      dplyr::mutate(Plan = as.numeric(Plan)) %>%
      dplyr::mutate(Area_ha_log = log(Area_ha)) %>%
      mutate(Area01 = (Area_ha - min(Area_ha, na.rm = TRUE)) /
               (max(Area_ha, na.rm = TRUE) - min(Area_ha, na.rm = TRUE))) %>%
      mutate(Arealog01 = (Area_ha_log - min(Area_ha_log, na.rm = TRUE)) /
               (max(Area_ha_log, na.rm = TRUE) - min(Area_ha_log, na.rm = TRUE))) %>%
      dplyr::mutate(Plan_weighted = Plan * weight_plan_norm) %>%
      dplyr::mutate(Area_weighted = Arealog01 * weight_area_norm) %>%
      dplyr::mutate(PriorityScore = Plan_weighted + Area_weighted)

    df

  }, ignoreNULL = FALSE)

  
  output$n_wetlands <- renderText({
    glue::glue('You have selected {nrow(ramsar_pts_COI_selected())} wetlands out of 857 in countries of interest')
  })
  
  output$table <- renderDataTable({
    ramsar_pts_COI_selected() %>%
      sf::st_drop_geometry() %>%
      dplyr::select(-officialna, -AnnotatedSummary)
  })
  
  
  output$barplot <- renderPlot({
    
    # weight_plan <- 0.10
    # weight_area <- 0.50
    
    ramsar_pts_COI_selected() %>%
      sf::st_drop_geometry() %>%
      dplyr::arrange(-PriorityScore) %>%
      head(30) %>%
      tidyr::pivot_longer(cols = c(Plan_weighted, Area_weighted)) %>%
      ggplot(aes(x = reorder(SiteName, PriorityScore), y = value)) +
      geom_bar(stat = 'identity', aes(fill = name)) +
      scale_fill_viridis_d() +
      coord_flip() +
      xlab(element_blank()) +
      theme_minimal() +
      theme(text = element_text(size = 18))
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
