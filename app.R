#######################################
#      ACS Data Exploration App       #
# For Madison Student Housing Studies #
#######################################

# Isaac Baumann

library(shiny)
library(tidyverse)
library(tidycensus)
library(leaflet)
library(tigris)
library(janitor)
library(sf)
library(shinydashboard)
library(plotly)
library(scales)

#####
# Define state, county, metro area/city of interest

st <- 'WI'
ct <- 'Dane'
cty <- 'Madison'

current_year <- year(Sys.time())

year_attmpt <- try(
  get_acs(
    state = 'WI',
    county = 'Dane',
    geography = 'tract',
    survey = 'acs5',
    year = 2023,
    variables = 'B14001_008',
    geometry = FALSE
  )
)

max_yr <- ifelse(is.data.frame(year_attmpt),
                 current_year - 1,
                 current_year - 2)

#####
# Set levels of choice variables for user input
acs_years <- seq(2023, max_yr - 1, 1) # seq(2020, 2021, 1)
acs_years_micro <- acs_years[which(acs_years != 2020)]
student_type <- c('Undergraduate', 'Grad/Professional')

#####
# Read data
student_estimates_long <- readRDS('student_estimates_long.rds')
student_estimates_wide <- readRDS('student_estimates_wide.rds')
microdata_5_yr <- readRDS('microdata_5_yr.rds')
microdata_1_yr <- readRDS('microdata_1_yr.rds')
microdata_5_yr <- readRDS('microdata_5_yr.rds')

# Define UI for application
ui <- dashboardPage(
  title = 'Dane County Student Housing Affordability Explorer',
  # skin = 'red',
  dashboardHeader() |> tagAppendChild(
    div(
      "Dane County Student Housing Affordability Explorer",
      style = "
      display: block;
      font-size: 1.5em;
      margin-block-start: 0.5em;
      font-weight: bold;
      color: white;
      margin-right: 50%",
      align = "right"
    ),
    .cssSelector = "nav"
  ),
  dashboardSidebar(
    tags$head(tags$style(HTML('.logo {
                              background-color: #c5050c !important;
                              }
                              .navbar {
                              background-color: #c5050c !important;
                              }
                              '))),
    sidebarMenu(
      menuItem('Estimates Dashboard', tabName = 'est_dashboard', icon = icon('dashboard')),
      menuItem('Microdata Dashboard', tabName = 'micro_dashboard', icon = icon('dashboard')),
      menuItem('Data Details', tabName = 'details', icon = icon('circle-info'))
    ),
    uiOutput("userpanel")
  ),
  dashboardBody(
    tabItems(
      # ACS 5-year point-in-time dashboard
      tabItem(
        tabName = 'est_dashboard',
        fluidRow(
          box(
            width = 9,
            leafletOutput("est_map", height = "600")
          ),
          box(
            title = 'Selections',
            width = 3,
            selectInput(
              inputId = "acs_year_est",
              label = "Select a year (ACS 5 Years Ending)",
              choices = rev(acs_years)
            ),
            radioButtons(
              'map_type_est', 'Map view:',
              c('Students %' = 'pct_students',
                'Undergraduate %' = 'pct_undergrad',
                'Grad/Professional %' = 'pct_grad'
              )
            )
          )
        ),
        fluidRow(
          box(
            title = 'Estimated Rent by Bedrooms (Tract-Level)',
            footer = 'Error bars reflect the Census Bureau\'s margin of error around the rent estimate. 
            Estimates are not available for all rental unit types.',
            width = 6,
            plotlyOutput('rental_ests_plot')
          ),
          box(
            title = 'Estimated Proportions Above/Below Poverty Level (Tract-Level)',
            footer = 'Reflects U.S. Census Bureau estimates, not actual counts. Proportions are within enrollment category.',
            width = 6,
            plotlyOutput('pl_plot')
          )
        )
      ),
      # Microdata dashboard
      tabItem(
        tabName = 'micro_dashboard',
        fluidRow(
          box(
            width = 9,
            leafletOutput("micro_map", height = "600")
          ),
          box(
            title = 'Selections',
            width = 3,
            radioButtons(
              'acs_type', 'ACS Timing:',
              c('1 Year Ending:' = 'microdata_1_yr',
                '5 Years Ending:' = 'microdata_5_yr')
            ),
            selectInput(
              inputId = "acs_year_micro",
              label = "",
              choices = rev(acs_years_micro)
            ),
            radioButtons(
              'student_type_micro', 'Student type:',
              c('Undergraduate' = 15,
                'Grad/Professional' = 16)
            )
          )
        ),
        fluidRow(
          box(
            width = 6,
            plotlyOutput('m_rents_plot')
          ),
          box(
            width = 6,
            plotlyOutput('m_inc_plot')
          ),
        )
      ),
      # Data details page
      tabItem(
        tabName = 'details',
        h2('Data Details'),
        h3('Possibilities and Limitations'),
        p(
          'Data in these dashboards come from the U.S. Census Bureau\'s publicly available American Community Survey responses.
          These data come with benefits and limitations regarding housing affordability:'
        ),
        h3('American Community Survey'),
        p(
          'The U.S. Census Bureau\'s American Community Survey is an annual survey that samples persons across the United States. 
          The ACS includes a wide variety of questions about respondents\' demographic, housing, and economic characteristics.'
        ),
        p(
          'The Census Bureau provides data from the ACS in two forms: as aggregated', strong('estimates'), 'and as de-identified individual-level', strong('microdata'), '.
          These data are available in 5-year or 1-year periods ending in almost any year with different levels of geographic granularity.'
        ),
        h4('Estimates'),
        p(
          'ACS estimates are the Census Bureau\'s estimates of certain values for a geographic area (i.e., the estimated number of housing units in an area, the estimated median income in an area, etc.).
          Estimates are provided at a high level of geographic detail, sometime in areas that correspond roughly to specific neighborhoods in a city.'
        ),
        h4('Microdata'),
        p(
          'ACS public-use microdata samples (PUMS) are de-identified responses from individuals sampled by the ACS. 
          Unlike the ACS estimates, which are aggregated from individual respondents, the microdata are individual-level responses.
          The microdata allow users to calculate their own estimates, though from a smaller number of respondents than the overall ACS estimates.'
        ),
        p(
          'To maintain respondent confidentiality, the PUMS data are provided at a higher geographic level than the estimates.
          PUMS data may be less useful for analyses of specific neighborhoods than the ACS estimates.'
        ),
        p(
          'Reliable microdata are not available for 2020 due to low response rates.'
        ),
        h4('Survey Timing'),
        p(
          'The ACS is a continually ongoing survey. 
          Estimates and microdata are published annually for the previous year (\"ACS 1\").
          Estimates and microdata for the previous five years (\"ACS 5\") are also made available.
          Estimates from ACS 5 are offered at a higher level of geographic detail than ACS 1.'
        ),
        p(
          'Data used in this dashboard are from ACS 5 and reflect estimates and microdata responses from the 5 years ending with the selected year.
          While this means some data between chosen years overlap with one another, the geographic detail of the ACS 5 data may be more useful than using ACS 1 data without overlap between years.'
        ),
        h4('Geography'),
        p(
          'ACS data are provided at various levels of geographic detail to protect respondent confidentiality.
          Generally, microdata are offered at lower levels of detail than estimates. ACS 5 year estimates have more geographic detail than ACS 1 year estimates.'
        ),
        p(
          'Census ', strong('tracts'), ' are the smallest geographic areas that ACS data are available for. Depending on population density tracts may correspond roughly to neighborhoods. 
          The ACS 5 estimates used for the Estimates Dashboard are tract-level.'
        ),
        p(
          strong('PUMA'), 's (Public Use Microdata Areas) are collections of smaller Census Bureau geographies (like tracts) at which PUMS data are made available.
          PUMAs are larger than tracts. Depending on population density, PUMAs may correspond to cities, subdivisions of counties, or whole counties. 
          For example, Dane County typically has three PUMAs: one for Madison, one for east Dane County, and one for West Dane County.
          The ACS 1 and ACS 5 PUMS data used for the Microdata Dashboard are PUMA-level.'
        ),
        p(
          'Census Bureau geographies are typically re-drawn at each decennial census, so boundaries of tracts and PUMAs change roughly each decade as populations fluctuate.'
        )
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Render estimates leaflet map
  output$est_map <- renderLeaflet({
    
    leaflet(options = leafletOptions(zoomControl = FALSE)) |> 
      addTiles() |> 
      # addProviderTiles(providers$Stamen.TonerLite) |> 
      setView(lng = -89.38,
              lat = 43.07,
              zoom = 9.5)
    
  })
  
  # Render microdata leaflet map
  output$micro_map <- renderLeaflet({
    
    leaflet(options = leafletOptions(zoomControl = FALSE)) |> 
      addTiles() |> 
      # addProviderTiles(providers$Stamen.TonerLite) |> 
      setView(lng = -89.38,
              lat = 43.07,
              zoom = 9.5)
    
  })
  
  # Reactive variables
  to_listen_est_map <- reactive({
    list(
      input$acs_year_est, 
      input$map_type_est
    )
  })
  
  to_listen_micro_map <- reactive({
    list(
      input$acs_type, 
      input$acs_year_micro, 
      input$student_type_micro
    )
  })
  
  to_listen_est_plots <- reactive({
    list(
      input$acs_year_est, 
      input$map_type_est,
      input$est_map_shape_click
    )
  })
  
  to_listen_micro_plots <- reactive({
    list(
      input$acs_type, 
      input$acs_year_micro, 
      input$student_type_micro,
      input$micro_map_shape_click
    )
  })
  
  #####
  # Estimates dashboard server-side
  
  # Reactively filter point-in-time students estimates data
  observeEvent(to_listen_est_map(), {
    
    df_to_map_est <- reactive({
      student_estimates_long |> 
        filter(year == input$acs_year_est &
                 var == input$map_type_est
        )
    })
    
    # Build color pallette for map
    conpal_est <- colorNumeric(palette = "RdYlBu", 
                               domain = NULL, # df_to_map_est()$val, 
                               reverse = TRUE,
                               na.color = 'white')
    
    conpal_est_rev <- colorNumeric(palette = "RdYlBu", 
                                   domain = NULL, # df_to_map_est()$val, 
                                   reverse = FALSE,
                                   na.color = 'white')
    
    # Build hover labels for map
    labels_est <- sprintf(
      "%s<br/><b>%g%%</b> %s",
      "Tract Student Pop.:",
      round(df_to_map_est()$val, 2),
      ifelse(input$map_type_est == 'pct_undergrad', 'Undergraduate Students',
             ifelse(input$map_type_est == 'pct_grad', 'Grad/Prof. Students', 'Students'))
    ) |> lapply(htmltools::HTML)
    
    # Build reactive leaflet map
    leafletProxy("est_map") |> 
      clearShapes() |> 
      clearControls() |> 
      addPolygons(
        data = df_to_map_est(),
        layerId = df_to_map_est()$geoid,
        fillColor = ~conpal_est(df_to_map_est()$val),
        weight = 0.5,
        color = 'grey',
        dashArray = "3",
        fillOpacity = 0.4,
        smoothFactor = 0.2,
        highlightOptions = highlightOptions(
          weight = 5,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE),
        label = labels_est,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto")) |> 
      addLegend(
        position = "bottomright",
        pal = conpal_est_rev,
        values = df_to_map_est()$val,
        labFormat = labelFormat(transform = function(x)  sort(x, decreasing = TRUE)),
        title = "Student % of population"
      )
    
  })
  
  observeEvent(to_listen_est_plots(), {
    
    df_stats <- reactive({
      student_estimates_wide |> 
        filter(year == input$acs_year_est
        )
    })
    
    # Pre-filter df for map clicks
    if (is.null(input$est_map_shape_click$id)) {
      df_stats_filtered <- df_stats()
    } else {
      df_stats_filtered <- df_stats() |> 
        filter(geoid == input$est_map_shape_click$id)
    }
    
    # Plot rental estimates by no. rooms
    
    max_rent <- max(
      max(df_stats()$rent_0_est, na.rm = T),
      max(df_stats()$rent_1_est, na.rm = T),
      max(df_stats()$rent_2_est, na.rm = T),
      max(df_stats()$rent_3_est, na.rm = T),
      max(df_stats()$rent_4_est, na.rm = T),
      max(df_stats()$rent_5_est, na.rm = T),
      na.rm = T
    )
    
    max_moe <- max(
      max(df_stats()$rent_0_moe, na.rm = T),
      max(df_stats()$rent_1_moe, na.rm = T),
      max(df_stats()$rent_2_moe, na.rm = T),
      max(df_stats()$rent_3_moe, na.rm = T),
      max(df_stats()$rent_4_moe, na.rm = T),
      max(df_stats()$rent_5_moe, na.rm = T),
      na.rm = T
    )
    
    x_bound <- max_rent + max_moe
    
    output$rental_ests_plot <- renderPlotly(
      
      if (is.null(input$est_map_shape_click$id)) {
        
        prompt_plot_rental_est <- ggplot() +
          geom_text(aes(0,0,label='Click on a census tract')) +
          theme_void()
        
        ggplotly(prompt_plot_rental_est)
        
      } else {
        
        df_stats_rent_est <- df_stats_filtered |> 
          tibble() |> 
          select(rent_0_est, rent_1_est, rent_2_est, rent_3_est, rent_4_est, rent_5p_est) |> 
          pivot_longer(cols = everything(),
                       names_to = 'beds',
                       values_to = 'est'
          ) |> 
          mutate(beds = str_extract(beds, '[0-9]'))
        
        df_stats_rent_moe <- df_stats_filtered |> 
          tibble() |> 
          select(rent_0_est, rent_1_moe, rent_2_moe, rent_3_moe, rent_4_moe, rent_5p_moe) |> 
          pivot_longer(cols = everything(),
                       names_to = 'beds',
                       values_to = 'moe'
          ) |> 
          mutate(beds = str_extract(beds, '[0-9]'))
        
        df_stats_rent_est_w_moe <- df_stats_rent_est |> 
          left_join(df_stats_rent_moe,
                    by = 'beds') |> 
          mutate(beds = case_when(
            beds == 0 ~ 'Studio',
            beds == 5 ~ '5+',
            TRUE ~ beds
          ),
          lower_moe = est - moe,
          upper_moe = est + moe
          ) |> 
          mutate(beds = factor(beds, levels = rev(c('Studio', '1', '2', '3', '4', '5+'))))
        
        tract_rent_est <- df_stats_rent_est_w_moe |> 
          mutate(est = as.numeric(est)) |> 
          ggplot(aes(y = beds, x = est, xmin = 0, xmax = x_bound)) +
          geom_point() +
          geom_errorbar(aes(xmin = lower_moe, xmax = upper_moe)) +
          theme(axis.ticks.y = element_blank(),
                panel.grid.major.y = element_blank()
          ) +
          scale_x_continuous(name = 'Monthly Rent Estimate', labels = label_dollar()) +
          # scale_x_continuous(name = 'Monthly Rent Estimate', labels = f <- function(x) paste0("$",x)) +
          scale_y_discrete(name = 'Bedrooms') +
          theme_minimal()
        
        ggplotly(tract_rent_est)
        
      }
      
    )
    
    output$pl_plot <- renderPlotly(
      
      if (is.null(input$est_map_shape_click$id)) {
        
        prompt_plot_pl_est <- ggplot() +
          geom_text(aes(0,0,label='Click on a census tract')) +
          theme_void()
        
        ggplotly(prompt_plot_pl_est)
        
      } else {
        
        pl_prop_df <- tibble(
          Enrollment = c(rep('Undergraduate', 2), rep('Grad/Professional', 2), rep('Not Enrolled in School', 2)),
          `Poverty Level` = rep(c('Below Poverty Level', 'At or Above Poverty Level'), 3),
          Estimate = c(
            df_stats_filtered$undergrad_pl_est, df_stats_filtered$undergrad_est - df_stats_filtered$undergrad_pl_est,
            df_stats_filtered$grad_pl_est, df_stats_filtered$grad_est - df_stats_filtered$grad_pl_est,
            df_stats_filtered$unenrolled_pl_est, df_stats_filtered$unenrolled_est - df_stats_filtered$unenrolled_pl_est
          )
        ) |> 
          group_by(Enrollment) |> 
          mutate(
            Enrollment = factor(Enrollment, levels = c('Undergraduate', 'Grad/Professional', 'Not Enrolled in School')),
            `Total Estimate` = sum(Estimate)
          )
        
        pl_prop_labels_df <- pl_prop_df |> 
          distinct(Enrollment, `Total Estimate`) |> 
          mutate(position = '.9',
                 `Total Estimate` = paste0("n = ", `Total Estimate`))
        
        pl_est_plot <- ggplot() + 
          geom_bar(data = pl_prop_df, 
                   aes(fill = `Poverty Level`, y = Estimate, x = Enrollment), 
                   position = "fill", stat = "identity") +
          geom_text(data = pl_prop_labels_df, 
                    aes(x = Enrollment, label = `Total Estimate`, y = 1.05, group = Enrollment)) +
          scale_y_continuous(name = 'Estimated Proportion of Subpopulation', labels = label_percent()) +
          theme_minimal() +
          theme(
            panel.grid.major.x = element_blank(),
            legend.title = element_blank(),
            axis.title.x = element_blank(),
            legend.position = 'bottom'
          ) +
          scale_fill_manual(values = c("grey", "black"))
        
        ggplotly(pl_est_plot)
        
      }
      
    )
    
  })
  
  #####
  # Microdata dashboard server-side
  
  # Reactively filter point-in-time students estimates data
  observeEvent(to_listen_micro_map(), {
    
    df_micro_map <- reactive({
      get(ifelse(input$acs_type == 'microdata_1_yr', 
                 'microdata_1_yr', 'microdata_5_yr')) |> 
        filter(year == input$acs_year_micro
        )
    })
    
    df_to_map_micro <- reactive({
      df_micro_map() |> 
        filter(schg == as.numeric(input$student_type_micro) &
                 ten == 3) |> 
        group_by(namelsad20, puma, geometry) |> 
        summarise(n = n(),
                  m_inc = median(pincp),
                  m_grpip = median(grpip),
                  m_rent = median(rntp)) |> 
        ungroup() |> 
        tibble()
    })
    
    # Build color palette for map
    conpal_micro <- colorNumeric(palette = "RdYlBu", 
                                 domain = NULL, # df_to_map_micro()$m_rent, 
                                 reverse = TRUE,
                                 na.color = 'white')
    
    conpal_micro_rev <- colorNumeric(palette = "RdYlBu", 
                                     domain = NULL, # df_to_map_micro()$m_rent, 
                                     reverse = FALSE,
                                     na.color = 'white')
    
    # Build hover labels for map
    labels_micro <- sprintf(
      "%s<br/><b>$%g</b> Median Rent<br/>(n=%s renting students)",
      df_to_map_micro()$namelsad20,
      df_to_map_micro()$m_rent,
      df_to_map_micro()$n
    ) |> lapply(htmltools::HTML)
    
    # Build reactive leaflet map
    leafletProxy("micro_map") |> 
      clearShapes() |> 
      clearControls() |> 
      addPolygons(
        data = st_as_sf(df_to_map_micro()),
        layerId = df_to_map_micro()$puma,
        fillColor = ~conpal_micro(df_to_map_micro()$m_rent),
        weight = 0.5,
        color = 'grey',
        dashArray = "3",
        fillOpacity = 0.4,
        smoothFactor = 0.2,
        highlightOptions = highlightOptions(
          weight = 5,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE),
        label = labels_micro,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto")) |> 
      addLegend(
        position = "bottomright",
        pal = conpal_micro_rev,
        values = df_to_map_micro()$m_rent,
        labFormat = labelFormat(transform = function(x)  sort(x, decreasing = TRUE)),
        title = "Students' Median Rent"
      )
    
  })
  
  observeEvent(to_listen_micro_plots(), {
    
    df_micro_plot <- reactive({
      get(ifelse(input$acs_type == 'microdata_1_yr', 
                 'microdata_1_yr', 'microdata_5_yr')) |> 
        filter(year == input$acs_year_micro
        )
    })
    
    df_to_plot_micro <- reactive({
      df_micro_plot() |> 
        filter((schg == as.numeric(input$student_type_micro) | 
                  schg == 'bb') & # Do we want to filter in only those not attending school?
                 ten == 3) |> 
        tibble()
    })
    
    # Pre-filter df for map clicks
    if (is.null(input$micro_map_shape_click$id)) {
      df_to_plot_filtered <- df_to_plot_micro()
    } else {
      df_to_plot_filtered <- df_to_plot_micro() |> 
        filter(puma == input$micro_map_shape_click$id)
    }
    
    # Create microdata rents plot
    
    output$m_rents_plot <- renderPlotly({
      m_rents_plot <- df_to_plot_filtered |> 
        mutate(student_type = case_when(
          schg == 'bb' ~ 'Non-Student',
          schg == 15 ~ 'Undergraduate',
          schg == 16 ~ 'Grad/Prof.',
        )
        ) |> 
        mutate(student_type = factor(student_type, levels = c('Non-Student', 'Undergraduate', 'Grad/Prof.'), ordered = T)) |> 
        ggplot(aes(y = rntp, x = student_type, fill = student_type)) +
        # geom_jitter(alpha = 0.6) +
        geom_boxplot(alpha = 0.6) +
        # geom_vline(xintercept = median(df_to_plot_micro()[df_to_plot_micro()$schg == 'bb',]$rntp, na.rm = T), color = 'red') +
        # geom_vline(xintercept = median(df_to_plot_micro()[df_to_plot_micro()$schg == as.numeric(input$student_type_micro),]$rntp, na.rm = T), color = 'blue') +
        scale_fill_brewer(palette = 'Set1') +
        labs(title = 'Reported Rents by Students and Non-Students',
             subtitle = paste0('In ', input$acs_year_micro,' ACS Respondents')) +
        guides(fill = 'none') +
        ylab('Monthly Rent') +
        xlab('') +
        theme(legend.title = element_blank(),
              legend.position = 'none',
              axis.title.x = element_blank()) +
        scale_y_continuous(name = 'Reported Rents', labels = label_dollar()) +
        theme_minimal()
      
      ggplotly(m_rents_plot)
      
    })
    
    # Create income plot
    
    output$m_inc_plot <- renderPlotly({
      m_inc_plot <- df_to_plot_filtered |> 
        mutate(student_type = case_when(
          schg == 'bb' ~ 'Non-Student',
          schg == 15 ~ 'Undergraduate',
          schg == 16 ~ 'Grad/Prof.',
        )
        ) |> 
        mutate(student_type = factor(student_type, levels = c('Non-Student', 'Undergraduate', 'Grad/Prof.'), ordered = T)) |> 
        ggplot(aes(y = pincp, x = student_type, fill = student_type)) +
        # geom_jitter(alpha = 0.6) +
        geom_boxplot(alpha = 0.6) +
        # geom_vline(xintercept = median(df_to_plot_micro()[df_to_plot_micro()$schg == 'bb',]$rntp, na.rm = T), color = 'red') +
        # geom_vline(xintercept = median(df_to_plot_micro()[df_to_plot_micro()$schg == as.numeric(input$student_type_micro),]$rntp, na.rm = T), color = 'blue') +
        scale_fill_brewer(palette = 'Set1') +
        labs(title = 'Reported Income by Students and Non-Students',
             subtitle = paste0('In ', input$acs_year_micro,' ACS Respondents')) +
        guides(fill = 'none') +
        ylab('Income') +
        xlab('') +
        theme(legend.title = element_blank(),
              legend.position = 'none',
              axis.title.x = element_blank()) +
        scale_y_continuous(name = 'Reported Income', labels = label_dollar()) +
        # scale_y_continuous(name = 'Reported Income', labels = f <- function(x) paste0("$",x)) +
        theme_minimal()
      
      ggplotly(m_inc_plot)
      
    })
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
