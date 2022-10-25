library(dplyr)
library(stringr)
library(rgeos)
library(ggplot2)
library(plotly)
library(leaflet)
library(maps)
library(maptools)
library(shiny)

# ----- Preliminary work and Presets -----
##  Read in cleaned bombings CSV file and Vietnam districts shapefile
bombings <- read.csv("./Vietnam_Bombings_Cleaned.csv")
districts <- readShapeSpatial("./Vietnam_Provinces_1997/SDE_DATA_VM_F1PROVINCES_1997")

## Reusable options for labels and highlights.
labels_options <- labelOptions(
  direction = "auto",
  style = list("font-weight" = "normal", "font-size" = "1rem",
               "padding" = "0.5rem 0.5rem", "border" = "none",
               "background-color" = "black", "color" = "white"))

highlight_options <- highlightOptions(
  weight = 2,
  color = "grey",
  fillOpacity = 0.7,
  bringToFront = TRUE)

## Palette for circle markers
markers_palette <- colorFactor(palette = c("navy", "yellow", "red"), 
                               levels = c("B-57", "B-66", "B-52"))

## Pre-defined constants from data set
minYear <- min(bombings$Year)
maxYear <- max(bombings$Year) 
uniqueYears <- unique(bombings$Year) 
uniqueAircrafts <- unique(bombings$VALID_AIRCRAFT_ROOT)
uniquePeriodsOfDay <- unique(bombings$PeriodOfDay)

## Use one layer ID for tiles to prevent tiles from stacking when changing provider
tileLayerID = "tiles" 

## Helper function for pretty-transforming axis labels
makeTitleCase <- function(x) {
  return(tools::toTitleCase(tolower(x)))
}

## Helper function for transforming period of day text into more descriptive versions
makeDayPeriod <- function(s) {
    if (s == "D") {
      return("Day")
    } else if (s == "N") {
      return("Night")
    } else {
      return("Unknown")
    }
}



# ----- UI -----
ui <- navbarPage(title = "Vietnam War Bombings Visualizer",
  # Tab Panel 1: Leaflet Map
  tabPanel(title = "Map of Bombings",
    style = "padding: 2rem 8rem 0 8rem;",
    wellPanel(
      style = "padding: 4rem 8rem; ",
      fluidRow(
        
        # Year slider input and its options
        column(6,
          style = "padding-right: 5%;",
          fluidRow(
            uiOutput("yearSlider")), # Conditionally-rendered year slider
          fluidRow(
            column(6,
              # Radio buttons for year slider options
              radioButtons(inputId = "yearOpt", label = "Plot bombings for...", selected = TRUE,
                           choices = c("A single year" = FALSE, "A span of years" = TRUE))),
              
            column(6, uiOutput("animationOptWrapper")) # Conditionally-rendered animation radio button
          )
        ),
      
        # Aircraft and Period of Day selector inputs
        column(6,
          style = "padding-left: 5%;",
          selectInput(inputId = "aircraft", label = "Aircraft Type:",
                      choices = c("B-52" = "B-52", "B-57" = "B-57"),
                      multiple = TRUE),
          selectInput(inputId = "dayPeriod", label = "Period of Day:",
                      choices = c("Day" = "D", "Night" = "N", "Unknown" = "Unknown"),
                      multiple = TRUE)
        )
      )
    ),
  
    # Main panel for leaflet map
    wellPanel(
      style = "padding: 4rem 4rem;",
      fluidRow(
        column(12,
          leafletOutput("myMap", height = 500)
        ),
      )
    )
  ),
  
  # Tab Panel 2: ggplot graphs
  tabPanel(title = "The Numbers",
    style = "padding: 2rem 8rem 0 8rem;",
    sidebarLayout(position = "left",
                  
      # Sidebar for graph options
      sidebarPanel(
        style = "margin-top: 4rem",
        
        # Selector for x-axis variable for graph
        selectInput(inputId = "xVar", label = "Choose an x-axis variable:",
                    choices = c("Year" = "Year", "Period of Day" = "PeriodOfDay", "Target Type" = "TargetType")),
        # Radio buttons for choosing to show undocumented data of x-axis variable
        radioButtons(inputId = "showUnknown", label = "Include unknown/undocumented instances?",
                                selected = FALSE, choices = c("No" = FALSE, "Yes" = TRUE)),
        # Action button to construct graph
        actionButton(inputId = "goBtn", label = "Construct Graph")
      ),
      
      # Main panel for ggplotly graph
      mainPanel(
        plotlyOutput("plot")
      )
    )
  )
)



# ----- Server -----
server <- function(input, output) {
  # --- Leaflet Map ---
  
  # States to keep track of markers group, animation mode, and provider tiles
  groupCount <- 1
  lastMode <- TRUE
  curProvider <- providers$CartoDB.Voyager
  
  # Reactive bombings data frame
  bombings_filtered <- reactiveValues(data = data.frame(), name = "initial")
  
  # Leaflet provider theme based on period of day
  ## If "Day" only, use light theme
  ## If "Night" only, use dark theme
  ## If both or neither, use colored theme for clarity
  provider <- reactive(ifelse("D" %in% input$dayPeriod & "N" %in% input$dayPeriod,
                              providers$CartoDB.Voyager,
                              ifelse("D" %in% input$dayPeriod,
                                     providers$CartoDB.Positron,
                                     ifelse("N" %in% input$dayPeriod,
                                            providers$CartoDB.DarkMatter,
                                            providers$CartoDB.Voyager))))

  # Observe environment for conditionally rendering UI elements (Year slider & Animation radio buttons)
  observe(
    if (input$yearOpt) {
      # Render double-ended year slider
      output$yearSlider <- renderUI({
        sliderInput(inputId = "year", label = "Time Period (Year):", step = 1, ticks = FALSE,
                    min = minYear, max = maxYear, value = c(minYear, maxYear))
      })
      
      # Render nothing for animation radio buttons
      output$animationOptWrapper <- NULL
      
    } else {
      # Render single-ended year slider
      output$yearSlider <- renderUI({
        sliderInput(inputId = "year", label = "Time Period (Year):", step = 1, ticks = FALSE,
                    min = minYear, max = maxYear, value = minYear,
                    animate = animationOptions(interval = 1500, loop = FALSE, 
                                               playButton = "Run time-lapse",
                                               pauseButton = "Pause"))
      })
      
      # Render animation radio buttons
      output$animationOptWrapper <- renderUI({
        radioButtons(inputId = "animationOpt", label = "Animate bombings...", selected = TRUE,
                     choices = c("Separately each year" = TRUE, "Accumulatively over time" = FALSE))
      })
    }
  )

  # Observe input values to reactively and imperatively render graph without action buttons
  observeEvent({
    input$yearOpt # Year option (single year vs span of years)
    input$animationOpt # Animation option (single year vs accumulative)
    input$year # Year input (slider)
    input$aircraft # Aircraft input (selector, multiple)
    input$dayPeriod # Period of day input (selector, multiple)
    TRUE # Non-NULL value required for technical reasons 
  }, {
    
    # Define variables/filters used based on current user input.
    # If no user input has been selected, get the "maximum" value.
    myYearOpt <- ifelse(is.null(input$yearOpt), TRUE, input$yearOpt)
    
    myAnimationOpt <- ifelse(is.null(input$animationOpt), TRUE, input$animationOpt)
    
    myYears <- if (is.null(input$year)) 
                    {uniqueYears}
               else {input$year}
    
    myAircrafts <- if (is.null(input$aircraft) | length(input$aircraft) < 1) 
                        {uniqueAircrafts} 
                   else {input$aircraft}
    
    myPeriodsOfDay <- if (is.null(input$dayPeriod) | length(input$dayPeriod) < 1)
                           {uniquePeriodsOfDay}
                      else {input$dayPeriod}
    
    # Update reactive bombings data set based on filters above
    if (myYearOpt) {
      # Using span of years slider
      bombings_filtered$data <- filter(bombings, Year >= myYears[1] & Year <= myYears[2] &
                                             VALID_AIRCRAFT_ROOT %in% myAircrafts &
                                             PeriodOfDay %in% myPeriodsOfDay)   
    } else {
      
      # Using single year slider and animating individual years
      if (myAnimationOpt) {
        bombings_filtered$data <- filter(bombings, Year == myYears[1] &
                                           VALID_AIRCRAFT_ROOT %in% myAircrafts &
                                           PeriodOfDay %in% myPeriodsOfDay) 
      } else {
      
      # Using single year slider and animating accumulative years
        bombings_filtered$data <- filter(bombings, Year <= myYears[1] & 
                                           VALID_AIRCRAFT_ROOT %in% myAircrafts &
                                           PeriodOfDay %in% myPeriodsOfDay)
      }
    }
    
    # Update marker labels based on filtered data set used. Transforms period of day text for clarity
    labels_bombings <- paste("Mission Date:", bombings_filtered$data$MSNDATE, "<br/>",
                             "Period of Day:", lapply(bombings_filtered$data$PeriodOfDay, FUN = makeDayPeriod), "<br/>",
                             "Target Type:", makeTitleCase(bombings_filtered$data$TargetType), "<br/>",
                             "Aircraft Type:", bombings_filtered$data$VALID_AIRCRAFT_ROOT, "<br/>",
                             "Duration:", bombings_filtered$data$TIMEONTARGET) %>% lapply(htmltools::HTML)
  
    # Use a proxy to update circle markers on leaflet map
    if (myYearOpt == FALSE & myAnimationOpt == FALSE) {
      # Single year and accumulative animation
      
      # Clear markers if changed modes
      if (lastMode == TRUE) {
        leafletProxy("myMap") %>% clearMarkers()
      }
      
      # Update global state to keep track of animation mode and marker groups.
      lastMode <<- FALSE
      toRemove <- as.character(groupCount)
      groupCount <<- groupCount + 1
      
      # Update map with new marker group and delete last marker group
      leafletProxy("myMap", data = bombings_filtered$data) %>%
        addCircleMarkers(group = as.character(groupCount), 
                         lng = ~TGTLONDDD_DDD_WGS84, lat = ~TGTLATDD_DDD_WGS84,
                         radius = ~log(TIMEONTARGET, 4), stroke = FALSE, 
                         color = ~markers_palette(VALID_AIRCRAFT_ROOT),
                         label = labels_bombings, labelOptions = labels_options,
                         options = pathOptions(panae = "markers")) %>% 
        clearGroup(group = toRemove)

    } else {
      # Other modes. Always clear markers before spawning new ones
      lastMode <<- TRUE
      
      leafletProxy("myMap", data = bombings_filtered$data) %>%
        clearMarkers() %>% 
        addCircleMarkers(lng = ~TGTLONDDD_DDD_WGS84, lat = ~TGTLATDD_DDD_WGS84,
                         radius = ~log(TIMEONTARGET, 4), stroke = FALSE, 
                         color = ~markers_palette(VALID_AIRCRAFT_ROOT),
                         label = labels_bombings, labelOptions = labels_options,
                         options = pathOptions(pane = "markers"))
    }
  })
  
  # Update provider tiles on day period input. 
  observeEvent({
    input$dayPeriod
    TRUE
  }, {
    # Only perform change when different from current provider
    if (provider() != curProvider) {
      curProvider <<- provider() # Update global state
      leafletProxy("myMap") %>% addProviderTiles(curProvider, layerId = tileLayerID)
    }
  })
  
  # Initial leaflet map output
  output$myMap <- renderLeaflet({          
    # HTML labels for district polygons
    labels_districts <- paste("District:", makeTitleCase(districts@data$PROVINCE_N), "<br/>") %>% lapply(htmltools::HTML)
  
    # Generate Leaflet map once at the beginning. (Proxy is used to update map subsequently)
    # Use map panes with z-index to ensure markers are always on top of district polygons
    leaflet() %>%
      setView(lng = 108, lat = 16, zoom = 5) %>% 
      addProviderTiles(providers$CartoDB.Voyager, layerId = tileLayerID) %>%
      addMapPane("polygons", zIndex = 200) %>%
      addMapPane("markers", zIndex = 201) %>%
      addPolygons(data = districts, color = "#5C5D5F", weight = 1.2, fillOpacity = 0.4,
                  highlight = highlight_options,
                  label = labels_districts,
                  labelOptions = labels_options,
                  options = pathOptions(pane = "polygons"))
  })

  # --- ggplotly graphs ---:
  # Note: Graphs are generated with ggplot first for easier manipulation,
  # then turned into plotly graphs for better UI and interaction
  
  output$plot <- renderPlotly({
    # Only render on action button input
    input$goBtn
    
    # Reactive variables
    xVar <- reactive(input$xVar)
    showUnknown <- reactive(input$showUnknown)
    
    # Isolated variables
    myVar <- isolate(xVar())
    myShowUnknown <- isolate(showUnknown())
    
    # Year x-axis
    if (myVar == "Year") {
      ggplotly(
        ggplot(bombings) +
          geom_bar(mapping = aes(x = Year)) + 
          scale_x_continuous(breaks = seq(1965, 1973, 1)) + 
          labs(x = "Year", y = "Bombings") +
          theme(axis.text = element_text(face = "bold")) +
          theme_bw()
      )
    
    # PeriodOfDay x-axis
    } else if (myVar  == "PeriodOfDay") {
      
      # Conditional variables based on whether unknown/unidentified records are shown
      xLabels <- if (myShowUnknown) c("Day", "Night", "Unknown") else c("Day", "Night")
      myBombings <- if (myShowUnknown) {bombings} else {subset(bombings, PeriodOfDay != "Unknown")}
      
      ggplotly(
        ggplot(myBombings) +
          geom_bar(mapping = aes(x = PeriodOfDay)) + 
          scale_x_discrete(labels = xLabels) +
          labs(x = "Period of Day", y = "Bombings") + 
          theme(axis.text = element_text(face = "bold")) +
          theme_bw()
      )
      
    # TargetType x-axis
    } else {
      
      # Conditional variables based on whether unknown/unidentified records are shown
      myBombings <- if (myShowUnknown) {bombings} else {subset(bombings, TargetType != "UNKNOWN\\UNIDENTIFIED")}
      
      # Group TargetType records. Make TargetType labels pretty. Get top 10.
      myBombings <- myBombings %>%
        group_by(TargetType) %>%
        mutate(TargetType = makeTitleCase(TargetType)) %>%
        summarize(count = n()) %>%
        arrange(desc(count)) %>%
        head(10)
      
      ggplotly(
        ggplot(myBombings) +
          geom_col(mapping = aes(x = reorder(TargetType, -count), y = count, text = paste("Count:", count))) +
          labs(x = "Target Type (Top 10)", y = "Bombings") +
          theme_bw() +
          theme(axis.text.x = element_text(angle = 45, vjust = 0.7, face = "bold")),
        tooltip = "text"
      )
    }
  })
}
  
# Build
shinyApp(ui, server)
