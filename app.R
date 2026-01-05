#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#

library(shiny)
# library(sf)
library(DT)
# library(leaflet)
library(shinyWidgets)
library(tidyverse)

# Initial Initial processing ---------------------------------------
# Load data
load('data/sites_data_v02.RData')

logical_cols_all <- c("HighBiodiv_anymetric",
                      "kba_overlap",
                      "HasRamsarBiodiv",
                      "HasRamsarFish",
                      "HasRamsarBirds",
                      "HasRamsarBirdsOrFish",
                      "HighSocial_anymetric",
                      "Critical_NCP",
                      "EcoServices_all4",
                      "HighC_anymetric",
                      "HighResilience",
                      "Type_CoralReefs",
                      "High_totalC_Irr",
                      "High_densityC_Irr",
                      "High_totalC_Manag", 
                      "High_densityC_Manag", 
                      "high_exchange",
                      "area_gt100ha",
                      "warm_climate")

logical_cols <-  c("HighBiodiv_anymetric",
                   "kba_overlap",
                   "HasRamsarBiodiv",
                   "HighSocial_anymetric",
                   "Critical_NCP",
                   "EcoServices_all4",
                   "HighC_anymetric",
                   "HighResilience")

# names(sites_df)
scoring_variables <- c("kba_overlap_percent",
                       "CriterionSum_Biodiv",                
                       "CriterionSum_All",
                       "avg_localNCP_importance",
                       "N_EcoServices",
                       "EcoServices_all4_01",
                       "percent_bothCritical_area",
                       "Sum_Irrecoverable_C_Total_2018",
                       "Sum_Manageable_C_Total_2018",
                       "PerHa_Irrecoverable_C_Total_2018",
                       "PerHa_Manageable_C_Total_2018",
                       "n_resilience_factors",
                       "Type_CoralReefs_01",
                       "ghs_population_sum_100km",
                       "ghs_population_sum_500km")

scoring_variables_long <- c("kba_overlap_01",
                            "HasRamsarBiodiv_01",                
                            "HasRamsarFish_01",
                            "HasRamsarBirds_01",                 
                            "HasRamsarBirdsOrFish_01",
                            "HighSocial_anymetric_01",           
                            "Critical_NCP_01",
                            "EcoServices_all4_01",               
                            "HighC_anymetric_01",
                            "HighResilience_01",                 
                            "Type_CoralReefs_01", 
                            "High_totalC_Irr_01",                
                            "High_densityC_Irr_01",
                            "High_totalC_Manag_01",              
                            "High_densityC_Manag_01",
                            "high_exchange_01",                  
                            "area_gt100ha_01",
                            "warm_climate_01",
                            "Sum_Irrecoverable_C_Total_2018",
                            "Sum_Irrecoverable_C_Soil_2018",     
                            "Sum_Irrecoverable_C_Biomass_2018",
                            "Sum_Manageable_C_Total_2018",       
                            "Sum_Manageable_C_Soil_2018",
                            "Sum_Manageable_C_Biomass_2018",    
                            "PerHa_Irrecoverable_C_Total_2018",  
                            "PerHa_Irrecoverable_C_Soil_2018",  
                            "PerHa_Irrecoverable_C_Biomass_2018",
                            "PerHa_Manageable_C_Total_2018",
                            "High_C_count",
                            "ghs_population_sum",              
                            "ghs_population_sum_100km",
                            "ghs_population_sum_500km",          
                            "n_resilience_factors",  
                            "N_Cultural",                        
                            "N_Provisioning",       
                            "N_Regulating",                      
                            "N_Supporting",           
                            "N_EcoServices",                     
                            "CriterionSum_Biodiv",
                            "CriterionSum_Fish",                 
                            "CriterionSum_Habitat",
                            "CriterionSum_WaterBirds",           
                            "CriterionSum_All",
                            "n_threats",                         
                            "avg_localNCP_importance",
                            "avg_globalNCP_importance",          
                            "avg_globaleezNCP_importance",
                            "avg_overlap_category",              
                            "percent_localCritical_area",
                            "percent_bothCritical_area",         
                            "percent_notCritical_area",
                            "percent_globalCritical_area",       
                            "kba_overlap_percent")
rescale_01_to_100 <- function(x, na.rm = TRUE) {
  rng <- range(x, na.rm = na.rm)
  if (diff(rng) == 0) return(rep(50, length(x)))  # all values identical
  100 * (x - rng[1]) / diff(rng)
}
# --------------------------
# helper: weighted z composite 
# --------------------------
weighted_z_composite <- function(df, vars, weights,
                                       center = c("mean","median"),
                                       scale  = c("sd","mad"),
                                       normalize = TRUE,
                                       na.rm = TRUE) {
  center <- match.arg(center)
  scale  <- match.arg(scale)
  if (length(vars) == 0) return(rep(NA_real_, nrow(df)))
  
  # build a data.frame of the selected vars, coercing logical -> numeric (TRUE=1, FALSE=0)
  mat <- lapply(vars, function(v) {
    x <- df[[v]]
    if (is.logical(x)) {
      # as numeric: TRUE -> 1, FALSE -> 0
      x <- as.numeric(x)
    }
    if (!is.numeric(x)) stop("Variable ", v, " is not numeric/logical.")
    x
  })
  mat <- as.data.frame(mat, stringsAsFactors = FALSE)
  names(mat) <- vars
  
  # compute centers & scales
  means <- sapply(mat, function(x) if (center == "mean") mean(x, na.rm = na.rm) else median(x, na.rm = na.rm))
  sds   <- sapply(mat, function(x) if (scale  == "sd")   sd(x,   na.rm = na.rm) else mad(x, na.rm = na.rm))
  
  # mark zero scales as NA (to drop)
  sds[sds == 0] <- NA_real_
  keep <- !is.na(sds)
  if (!all(keep)) {
    dropped <- names(mat)[!keep]
    warning("Dropping zero-variance variable(s): ", paste(dropped, collapse = ", "))
    mat <- mat[ , keep, drop = FALSE]
    means <- means[keep]
    sds <- sds[keep]
    weights <- weights[keep]
    vars <- vars[keep]
    if (ncol(mat) == 0) stop("No variables left after dropping zero-variance vars.")
  }
  
  if (normalize) weights <- weights / sum(weights)
  
  # z-score
  zmat <- sweep(mat, 2, means, FUN = "-")
  zmat <- sweep(zmat, 2, sds, FUN = "/")
  
  # per-row weighted mean, handling NAs
  w <- as.numeric(weights)
  comp <- apply(zmat, 1, function(rowz) {
    ok <- !is.na(rowz)
    if (!any(ok)) return(NA_real_)
    sum(rowz[ok] * w[ok]) / sum(w[ok])
  })
  
  comp
}

# Define UI for application that draws a histogram
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .small-checkbox label,
      .small-checkbox .checkbox label {
        font-size: 0.85em;
      }
      
      .small-dt table.dataTable {
        font-size: 0.85em;
      }
            /* tighter spacing for checkbox groups */
      .small-checkbox .shiny-options-group { margin-top: 4px; margin-bottom: 6px; }
    "))),
  # Application title
  titlePanel("Wetland Ranker"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      actionButton("apply_filters", 
                   "Update!", 
                   class = "btn-success"),

      div(class = "small-checkbox",
      helpText('Filter sites based on:'),
      uiOutput("logical_filters"),  

      hr(),
      helpText("Select which variables to include in scoring"),
      selectizeInput("scoring_vars", "Select variables",
                  choices = scoring_variables, 
                  multiple = TRUE,
                  selected = scoring_variables)),
      helpText("Assign weights for each variable:"),
      strong('Biodiversity'),
      numericInput("kba_overlap_percent", 
                   label = "kba_overlap_percent", value = 1.0, step = 0.1),
      numericInput("CriterionSum_Biodiv", 
                   label = "CriterionSum_Biodiv", value = 1.0, step = 0.1),
      numericInput("CriterionSum_All", 
                   label = "CriterionSum_All", value = 1.0, step = 0.1),
      strong('SocioEcological'),
      numericInput("avg_localNCP_importance", 
                   label = "avg_localNCP_importance", value = 1.0, step = 0.1),
      numericInput("N_EcoServices", 
                   label = "N_EcoServices", value = 1.0, step = 0.1),
      numericInput("EcoServices_all4_01", 
                   label = "EcoServices_all4_01", value = 1.0, step = 0.1),
      numericInput("percent_bothCritical_area", 
                   label = "percent_bothCritical_area", value = 1.0, step = 0.1),
      strong('Carbon'),
      numericInput("Sum_Irrecoverable_C_Total_2018", 
                   label = "Sum_Irrecoverable_C_Total_2018", value = 1.0, step = 0.1),
      numericInput("Sum_Manageable_C_Total_2018", 
                   label = "Sum_Manageable_C_Total_2018", value = 1.0, step = 0.1),
      numericInput("PerHa_Irrecoverable_C_Total_2018", 
                   label = "PerHa_Irrecoverable_C_Total_2018", value = 1.0, step = 0.1),
      numericInput("PerHa_Manageable_C_Total_2018", 
                   label = "PerHa_Manageable_C_Total_2018", value = 1.0, step = 0.1),
      strong('Resilience'),
      numericInput("n_resilience_factors", 
                   label = "n_resilience_factors", value = 1.0, step = 0.1),
      strong('Other'),
      numericInput("Type_CoralReefs_01", 
                   label = "Type_CoralReefs_01", value = 0, step = 0.1),
      numericInput("ghs_population_sum_100km", 
                   label = "ghs_population_sum_100km", value = 1.0, step = 0.1),
      numericInput("ghs_population_sum_500km", 
                   label = "ghs_population_sum_500km", value = 0, step = 0.1),
      checkboxInput("norm_weights", "Normalize weights (sum to 1)", TRUE),
      radioButtons("center", "Center (for z):", choices = c("mean","median"), selected = "mean", inline = TRUE),
      radioButtons("scale",  "Scale (for z):",  choices = c("sd","mad"), selected = "sd", inline = TRUE)
      ),
    
    mainPanel(
      verbatimTextOutput(outputId = "n_wetlands"),
      
      tabsetPanel(type = 'tabs',
                  tabPanel('Table',
                  div(class = "small-dt",
                      DT::dataTableOutput('table'))),
                  tabPanel('Sites by Ecoregion',
                           DT::dataTableOutput('sites_x_ecoregion')),
                  tabPanel('Extended Sites Table',
                           div(class = "small-dt", DT::dataTableOutput('extended_table')))
      ))
    )
  )


# Define server logic 
server <- function(input, output) {
  
  
  # Build explicit weight map 
  explicit_weight_map <- reactive({
    c(
      kba_overlap_percent = input$kba_overlap_percent,
      CriterionSum_Biodiv = input$CriterionSum_Biodiv,
      CriterionSum_All = input$CriterionSum_All,
      avg_localNCP_importance = input$avg_localNCP_importance,
      N_EcoServices = input$N_EcoServices,
      EcoServices_all4_01 = input$EcoServices_all4_01,
      percent_bothCritical_area = input$percent_bothCritical_area,
      Sum_Irrecoverable_C_Total_2018 = input$Sum_Irrecoverable_C_Total_2018,
      Sum_Manageable_C_Total_2018 = input$Sum_Manageable_C_Total_2018,
      PerHa_Irrecoverable_C_Total_2018 = input$PerHa_Irrecoverable_C_Total_2018,
      PerHa_Manageable_C_Total_2018 = input$PerHa_Manageable_C_Total_2018,
      n_resilience_factors = input$n_resilience_factors,
      Type_CoralReefs_01 = input$Type_CoralReefs_01,
      ghs_population_sum_100km = input$ghs_population_sum_100km,
      ghs_population_sum_500km = input$ghs_population_sum_500km
    )
  })
  

  
  # render UI: one checkboxGroupInput per logical column
  output$logical_filters <- renderUI({
    # create inputs
    inputs <- lapply(logical_cols, function(col) {
      # inputId must be unique
      inputId <- paste0("filter_", col)
      checkboxGroupInput(
        inputId = inputId,
        label = col,
        choices = c("TRUE"),
        selected = c("TRUE")
      )
    })
    do.call(tagList, inputs)
  })
  
  # reactive filtered data
  filtered_df <- eventReactive(input$apply_filters, {
    df <- sites_df
    for (col in logical_cols) {
      sel <- input[[paste0("filter_", col)]]
      # if the input hasn't been created yet (NULL), skip
      if (is.null(sel)) next
      
      # if user unchecked everything -> return zero rows (explicit behavior)
      if (length(sel) == 0) {
        df <- df[0, , drop = FALSE]
        next
      }
      
      # handle logical column values; coerce to "TRUE"/"FALSE"
      # If you also want to handle NA values, see optional section below
      keep <- as.character(df[[col]]) %in% sel
      df <- df[keep, , drop = FALSE]
    }
    df
  }, ignoreInit = TRUE)
  
  # safe getter: show df_orig until filters applied
  get_current_df <- reactive({
    df <- filtered_df()
    if (is.null(df)) df <- sites_df
    df
  })
  
  computed <- eventReactive(input$apply_filters, {
    df <- get_current_df()
    
    vars <- input$scoring_vars
    if (length(vars) == 0) {
      return(list(df = sites_df, vars = character(0), weights = numeric(0)))
    }
    
    # get weights for the selected vars from the explicit map
    wmap <- explicit_weight_map()
    # subset in the same order as vars; if missing in map -> 0
    weights <- as.numeric(wmap[vars])
    names(weights) <- vars
    weights[is.na(weights)] <- 0
    
    # guard: all-zeros -> set equal weights
    # Safety guard: if user set all weights to 0 and normalization is requested,
    # avoid division-by-zero by using equal weights and notify the user.
    if (input$norm_weights && all(weights == 0)) {
      showNotification("All selected weights are zero. Using equal weights to compute score.", type = "warning")
      weights[] <- 1
    }
    
    
    comp <- weighted_z_composite(df, vars = vars, weights = weights,
                                       center = input$center, scale = input$scale,
                                       normalize = input$norm_weights)
    df_out <- df %>%
      mutate(
        weighted_z = comp,
        weighted_z_0100 = rescale_01_to_100(weighted_z)
      ) %>%
      dplyr::mutate(weighted_z_0100 = round(weighted_z_0100, 1))
    
    list(df = df_out, vars = vars, weights = weights)
  }, ignoreNULL = FALSE)
  
  output$n_wetlands <- renderText({
    out <- computed()
    
    df <- out$df %||% sites_df
    n_ecoregions <- df %>% dplyr::pull(ECO_ID) %>% unique() %>% length()
    n_iso3 <- df %>% dplyr::pull(Country) %>% unique() %>% length()
    glue::glue('You have selected {nrow(filtered_df())} wetlands in {n_ecoregions} ecoregions in {n_iso3} countries')
  })

  output$extended_table <- renderDataTable({
    out <- computed()
    
    out$df %||% sites_df %>%
      dplyr::select(-c(SiteName, AnnotatedSummary)) %>%
      dplyr::relocate(ramsarid, namelabel, Country, weighted_z_0100, ECO_NAME, Type_CoralReefs_01) %>%
      dplyr::arrange(-weighted_z_0100) %>%
      datatable(escape = FALSE,
                options = list(pageLength = 30,
                               ColumnDefs = list(
                                 targets = c(20:50),
                               render = JS("
            function(data, type, row, meta) {
              if (type !== 'display') return data;
              if (data.length <= 100) return data;
              return '<span title=\"' + data + '\">' +
                     data.substr(0, 100) + '…</span>';
            }
          "))))
  })
  
  output$table <- renderDataTable({
    out <- computed()
    
    out$df %||% sites_df %>%
      dplyr::select(ramsarid, namelabel, Country, weighted_z_0100, ECO_NAME, Type_CoralReefs_01) %>%
      dplyr::arrange(-weighted_z_0100) %>%
      datatable(options = list(pageLength = 30)) 
  })
  
  output$sites_x_ecoregion <- renderDataTable({
    datatable(ecoregions_df, options = list(pageLength = 50))
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
