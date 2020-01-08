#setwd("~/R/proj/nohrsc/shiny/final/firstpublish")
#Sys.setenv(TZ="America/Los_Angeles")
#setwd("~/R/proj/nohrsc/shiny")
#setwd("~/Documents/shiny_nohrsc/final")
#setwd("~/R/proj/nohrsc/shiny/final")



source("df_build.r")
source("df_build.r")


source("df_build.r")


ui <- 
  
  shinyUI(fluidPage(
    useShinyjs(),
    br(),
    
    sidebarLayout(sidebarPanel(
      
      fluidRow(
        column(3, dateInput('start_date',
                            label = 'start day',
                            value = Sys.Date()-27)), 
        
        column(3,dateInput('end_date',
                           label = 'end day',
                           value = Sys.Date()-1)),
        
        column(3,dateInput('map_date',
                           label = 'map day',
                           value = Sys.Date()-1))),
      # column(3,actionButton("goButton", "run"))),
      
      selectizeInput(
        "nws_basin_code", "nwsid", choices = unique(df$nws_basin_code), 
        selected = c("San Joaquin - Friant Dam (FRAC1)"), 
        multiple = TRUE),
      
      radioButtons("entirebasin", "by elevation zone? (map: no zones)",                
                   choices = c("yes", "no"), selected = "no", inline = T),
      
      #   uiOutput("secondSelection"),
      fluidRow(
        column(5, checkboxGroupInput("parameter", "plot parameter [24-hr avg]",                
                                     choices = unique(df$paramnam),
                                     selected = "water equivalent (swe)") ),
        column(5, radioButtons("map_param", "map parameter",                
                               choices = unique(df$p_unit),
                               selected = "swe [in]") )),
      
      fluidRow(
        column(5, radioButtons('x', 'x-axis', c("date", "dowy"), inline = T)),
        
        
        column(5,radioButtons("free_scale", "y scale",                
                              choices = c("free", "fixed"), selected = "free", inline = T))),
      
      radioButtons("resctricttodowy", "only dates within day of year range",                
                   choices = c("yes", "no"), selected = "no", inline = T),
      
      radioButtons('color', 'color', c("none",  "basin_zone", "param", "nws_basin_code","wy"), selected  = "basin_zone", inline = T ),
      
      radioButtons('linetype', 'line type (eg dashed)', c("none",  "basin_zone", "param", "nws_basin_code","wy"), selected  = "none", inline = T ),
      
      
      radioButtons('facet_row', 'plot row group',
                   
                   c(none='.', "basin_zone", "param", "nws_basin_code","wy"), inline = T,
                   selected = "nws_basin_code"),
      
      radioButtons('facet_col', 'plot column group',
                   
                   c(none='.', "basin_zone", "param", "nws_basin_code","wy"), inline = T,
                   selected = "param")
      
      
      
      
    ),
    
    mainPanel(
      
      tabsetPanel(position=c("right"),
                  
                  tabPanel(strong("image"), 
                           br(),
                           plotOutput("reg_plot",  height = "750px")) ,
                  
                  tabPanel(strong("interactive"), 
                           br(),
                           plotlyOutput("plotly_plot",  height = "750px")) ,
                  
                  # tabPanel(strong("map1"), 
                  # br(),
                  # leafletOutput("mapplot",  height = "750px")) ,
                  
                  tabPanel(strong("map"), 
                           br(),
                           leafletOutput("mapplot",  height = "750px")) )))
  ) 
  )

########################
#### server.r
########################


server <- function(input, output) {
  
  options(shiny.maxRequestSize=225*1024^2) 
  
  output$reg_plot <- renderPlot({
    
    input$goButton
    
    startdowy <- dowy %>% filter(date == input$start_date )
    startdowy <- startdowy$dowy
    enddowy <- dowy %>% filter(date == input$end_date) 
    enddowy <- enddowy$dowy
    
    if (input$resctricttodowy == "no" && input$entirebasin == "yes")
      df <- df  %>% filter(date >= input$start_date) %>% filter(date <= input$end_date) %>%
      filter(nws_basin_code %in% input$nws_basin_code)   %>% 
      filter(paramnam %in% input$parameter)
    
    if (input$resctricttodowy == "yes" && input$entirebasin == "yes")
      
      df <- df  %>% filter(date >= input$start_date) %>% filter(date <= input$end_date) %>%
      filter(nws_basin_code %in% input$nws_basin_code)   %>% 
      filter(paramnam %in% input$parameter) %>% filter(dowy >= startdowy) %>% filter(dowy <= enddowy)
    
    if (input$resctricttodowy == "no" && input$entirebasin == "no")
      df <- df  %>% filter(date >= input$start_date) %>% filter(date <= input$end_date) %>%
      filter(nws_basin_code %in% input$nws_basin_code)   %>% 
      filter(paramnam %in% input$parameter) %>% filter(basin_zone == "Entire Basin")
    
    if (input$resctricttodowy == "yes" && input$entirebasin == "no")
      
      df <- df  %>% filter(date >= input$start_date) %>% filter(date <= input$end_date) %>%
      filter(nws_basin_code %in% input$nws_basin_code)   %>% 
      filter(paramnam %in% input$parameter) %>% filter(dowy >= startdowy) %>% filter(dowy <= enddowy) %>%
      filter(basin_zone == "Entire Basin")
    
    df <- df %>% mutate(wy = as.factor(wy))
    p <- ggplot(df, aes_string(x=input$x, y= "numval")) + geom_point(size = 0.5) + geom_line()  + labs(y = NULL, x = NULL) +
      scale_y_continuous(sec.axis = dup_axis(name = NULL)) + theme_bw(base_size=18)
    
    if (input$color != 'none')
      p <- p + aes_string(color=input$color)
    
    if (input$linetype != 'none')
      p <- p + aes_string(linetype=input$linetype)
    
    facets <- paste(input$facet_row, '~', input$facet_col)
    
    if (facets != '. ~ .' &&  input$free_scale == "free"  )
      p <- p + facet_grid(facets, scales = 'free_y') + theme(strip.text.y = element_text(angle = 0)) 
    
    if (facets != '. ~ .' &&  input$free_scale == "fixed"  )
      p <- p + facet_grid(facets) + theme(strip.text.y = element_text(angle = 0)) 
    
    
    
    print(p)
    
  }) 
  
  
  output$plotly_plot <- renderPlotly({
    
    #input$goButton
    
    startdowy <- dowy %>% filter(date == input$start_date)
    startdowy <- startdowy$dowy
    enddowy <- dowy %>% filter(date == input$end_date) 
    enddowy <- enddowy$dowy
    
    if (input$resctricttodowy == "no" && input$entirebasin == "yes")
      df <- df  %>% filter(date >= input$start_date) %>% filter(date <= input$end_date) %>%
      filter(nws_basin_code %in% input$nws_basin_code)   %>% 
      filter(paramnam %in% input$parameter)
    
    if (input$resctricttodowy == "yes" && input$entirebasin == "yes")
      
      df <- df  %>% filter(date >= input$start_date) %>% filter(date <= input$end_date) %>%
      filter(nws_basin_code %in% input$nws_basin_code)   %>% 
      filter(paramnam %in% input$parameter) %>% filter(dowy >= startdowy) %>% filter(dowy <= enddowy)
    
    if (input$resctricttodowy == "no" && input$entirebasin == "no")
      df <- df  %>% filter(date >= input$start_date) %>% filter(date <= input$end_date) %>%
      filter(nws_basin_code %in% input$nws_basin_code)   %>% 
      filter(paramnam %in% input$parameter) %>% filter(basin_zone == "Entire Basin")
    
    if (input$resctricttodowy == "yes" && input$entirebasin == "no")
      
      df <- df  %>% filter(date >= input$start_date) %>% filter(date <= input$end_date) %>%
      filter(nws_basin_code %in% input$nws_basin_code)   %>% 
      filter(paramnam %in% input$parameter) %>% filter(dowy >= startdowy) %>% filter(dowy <= enddowy) %>%
      filter(basin_zone == "Entire Basin")
    
    df <- df %>% mutate(wy = as.factor(wy))
    p <- ggplot(df, aes_string(x=input$x, y= "numval")) + geom_point(size = 0.5) + geom_line()  + labs(y = NULL, x = NULL) +
      scale_y_continuous(sec.axis = dup_axis(name = NULL)) + theme_bw(base_size=18)
    
    if (input$color != 'none')
      p <- p + aes_string(color=input$color)
    
    if (input$linetype != 'none')
      p <- p + aes_string(linetype=input$linetype)
    
    facets <- paste(input$facet_row, '~', input$facet_col)
    
    if (facets != '. ~ .' &&  input$free_scale == "free"  )
      p <- p + facet_grid(facets, scales = 'free_y') + theme(strip.text.y = element_text(angle = 0)) 
    
    if (facets != '. ~ .' &&  input$free_scale == "fixed"  )
      p <- p + facet_grid(facets) + theme(strip.text.y = element_text(angle = 0)) 
    
    
    p <- ggplotly(p)
    
    print(p)
    
  })
  
  output$mapplot <- renderLeaflet({
    
    # input$goButton
    
    df_map <- df %>% filter(basin_zone == "Entire Basin", date == input$map_date, p_unit %in% input$map_param)
    df_mapcolrange <- df %>% filter(basin_zone == "Entire Basin", p_unit %in% input$map_param)
    viridmax <- max(df_mapcolrange$numval)
    viridmin <- min(df_mapcolrange$numval)
    
    # Convert spatialpolydf to an sf object
    sf_ebasin_kml  <- ebasin_kml  %>%  st_as_sf() %>% transmute(Name, geometry)  %>%
      left_join(df_map, by = c("Name" = "nwscode"))
    
    sf_ebasin_kml_hlite  <- sf_ebasin_kml  %>%  
      filter(nws_basin_code %in% input$nws_basin_code) 
    
    maptypes = c("Stamen.TonerLite", "Stamen.Terrain", "Stamen.TopOSMRelief", "Esri.WorldTopoMap" , "Esri.WorldPhysical",  "OpenTopoMap" ,
                 "NASAGIBS.ModisTerraSnowCover", "NASAGIBS.ModisTerraTrueColorCR", "NASAGIBS.ModisTerraBands367CR")
    
    grp <- c(    "usgs hydrography",   "0.5 reflectivity") #,"hrrr p_1hr", "hrrr p_2hr",   "hrrr p_4hr", "hrrr p_6hr",
    #"mrms p_1hr", "mrms p_24hr", "mrms p_48hr", "mrms p_72hr") # "Coarse Geo") # 
    
    
    m <- mapview(sf_ebasin_kml["numval"], burst = TRUE, hide = TRUE, col.regions = viridisLite::viridis, 
                 alpha.regions = 0.4,  map.types = maptypes,
                 popup = popupTable(sf_ebasin_kml, zcol = c("nws_basin_code", "date", "numval", "paramnam")), 
                 layer.name = "nohrsc daily data")   +
      mapview(sf_ebasin_kml_hlite["numval"], color = "red", 
              alpha.regions = 0.0, 
              popup = popupTable(sf_ebasin_kml_hlite, zcol = c("nws_basin_code", "date", "numval", "paramnam")),
              layer.name = "selected basin(s' ) outline", legend = FALSE) 
    
    m@map = m@map %>% 
      
      addTiles() %>%
      setView(-119.6, 38.05, zoom = 7)  %>%   
      
      addWMSTiles(group= grp[1], baseUrl="https://basemap.nationalmap.gov/arcgis/services/USGSHydroCached/MapServer/WmsServer", layers = "0",
                  options = WMSTileOptions(format = "image/png", transparent = TRUE), attribution = "USGS") %>% 
      
      
      
      addWMSTiles( group = grp[2],baseUrl = 
                     "http://mesonet.agron.iastate.edu/cgi-bin/wms/nexrad/n0r.cgi", 
                   #"https://mesonet.agron.iastate.edu/cgi-bin/wms/nexrad/n0q.cgi?",
                   layers = "nexrad-n0r-900913",
                   options = WMSTileOptions(format = "image/png", transparent = TRUE),
                   attribution = "Weather data  2012 IEM Nexrad") %>%
      
      # addWMSTiles( group = grp[3],baseUrl = 
      #                "https://gibs.earthdata.nasa.gov/twms/epsg4326/best/twms.cgi", 
      #              #"https://mesonet.agron.iastate.edu/cgi-bin/wms/nexrad/n0q.cgi?",
      #              layers = "0",
      #              options = WMSTileOptions(format = "image/png", transparent = TRUE),
      #              attribution = "NASA GIBS imagery") %>%
      #
      
      
      
    mapview:::mapViewLayersControl(names = grp) %>% #hideGroup(grp[1]) #%>% 
      hideGroup(grp[2]) 
    
    
    
    m@map
  })
  
}

shinyApp(ui, server)