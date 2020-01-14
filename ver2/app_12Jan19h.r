#setwd("~/R/proj/nohrsc/shiny/final/firstpublish")
#Sys.setenv(TZ="America/Los_Angeles")
#setwd("~/R/proj/nohrsc/shiny")
#setwd("~/Documents/shiny_nohrsc/final")
#setwd("~/R/proj/nohrsc/shiny/final")

source("df_build.r")

ui <- 
  
  shinyUI(
    fluidPage(
    useShinyjs(),
    br(),
    
                                         sidebarLayout(
                                         sidebarPanel(
    #fluidRow(

    #column(6, 
           
    sliderInput("elev_cdec", "pillow altitude (ft)", 
        min = min(df$elev_cdec, na.rm = TRUE), 
        max = max(df$elev_cdec, na.rm = TRUE), value = c(min,max)), 

    checkboxGroupInput('wateryear',
        choices = sort(unique(df$wateryear)),
        label = 'water year',
        inline = TRUE,
        selected = c(2005, 2011, 2017, 2019, 2020)),

   checkboxGroupInput('basin',
        choices = sort(unique(df$basin)),
        label = 'basin',
        inline = TRUE,
        selected = c("San Joaquin", "Kings")),
   
   actionButton("goButton", "query & plot"),
   br(),
   br(),
      
   fluidRow(column(5,radioButtons('legend', 'legend', c("on", "off"), 
       selected = "on", 
       inline = F)),
        
   column(5,radioButtons("free_scale", "y scale",                
        choices = c("free", "fixed"), 
        selected = "fixed", 
        inline = F))),
   
   radioButtons("parameter", "plot parameter",                
                choices = unique(df$pname),
                selected = "inch", inline = T),
   
   sliderInput("mapdates", "map date range", 
               min = minmapdate,
               max = maxmapdate, value = c(maxmapdate-7, maxmapdate))
   
  
         
            #  ) 
                                               , width = 2 )           
                                                               ,

    mainPanel(
      
      tabsetPanel(position=c("right"),
                  

                  tabPanel(strong("interactive"), 
                           br(),
                           plotlyOutput("plotly_plot",  height = "750px")) ,
                  
                  tabPanel(strong("image"), 
                           br(),
                           plotOutput("reg_plot",  height = "750px")) ,
                  
                  
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
  
  options(shiny.maxRequestSize = 225*1024^2) 
  
  # 
 # output$checkbox <- renderUI({
 #   choice <-  unique(df[df$basin %in% input$basin, "pillow"])
 # 
 #   #print(choice)
 #   selectizeInput("checkbox","select pillow", 
 #                  options = NULL,
 #                  choices = choice ,
 #                  multiple = TRUE, 
 #                  selected = choice)
 #                   
 # })
  output$reg_plot <- renderPlot({
    
    if (input$goButton == 0)
      return()
    
    isolate({
    
      df <- df    %>%
        filter(pname %in% input$parameter) %>%
        filter(wateryear %in% input$wateryear)   %>% 
        filter(basin  %in% input$basin)  %>% 
        filter(elev_cdec <= input$elev_cdec[2]) %>%
        filter(elev_cdec >= input$elev_cdec[1]) #%>%

    
    p <- ggplot(df, aes(x = dowy, y = value, color  = pillow)) + geom_point(size = 0.5) + 
      geom_line()  + labs(y = NULL, x = NULL) +
      scale_y_continuous(sec.axis = dup_axis(name = NULL))  #+ theme_bw(base_size=18) #+
    
    })
   
    if ( input$free_scale == "free"  )
    {p <- p + facet_grid(wateryear ~ basin, scales = 'free_y') }
    
    if ( input$free_scale == "fixed"  )
    {p <- p + facet_grid(wateryear ~ basin) }
    
    if (input$legend == 'off')
      {p <- p + theme(legend.position = "none")}
    
    print(p)
    
   
    
  }) 
  
  
  output$plotly_plot <- renderPlotly({
    
    if (input$goButton == 0)
      return()
  
    isolate({
      
    df <- df    %>%
      filter(pname %in% input$parameter) %>%
      filter(wateryear %in% input$wateryear)   %>% 
      filter(basin  %in% input$basin)  %>% 
      filter(elev_cdec <= input$elev_cdec[2]) %>%
      filter(elev_cdec >= input$elev_cdec[1]) #%>%
      #mutate(date_ch = as.character(date))
    
    # df[df$pname %in% input$parameter] %>%
    # df[df$wateryear %in% input$wateryear]   %>% 
    # df[df$basin  %in% input$basin]  %>% 
    # #df[df$elev_cdec <= input$elev_cdec[2]] %>%
    ## df[df$elev_cdec >= input$elev_cdec[1]] #%>%
    # filter(elev_cdec <= input$elev_cdec[2]) %>%
    #filter(elev_cdec >= input$elev_cdec[1]) #%>%
    
    
    p <- ggplot(df, aes(x = dowy, y = value, color  = pillow, label =date)) + 
      geom_point(size = 0.5) + 
      geom_line()  + labs(y = NULL, x = NULL) +
      scale_y_continuous(sec.axis = dup_axis(name = NULL)) + #+ theme_bw(base_size=18) #+
      facet_grid(wateryear~basin)
    
    })
    
  # if (input$color != 'none')
  #   p <- p + aes_string(color=input$color)
  # 
  # if (input$linetype != 'none')
  #   p <- p + aes_string(linetype=input$linetype)
  # 
  # facets <- paste(input$facet_row, '~', input$facet_col)
  # 
    if ( input$free_scale == "free"  )
    {p <- p + facet_grid(wateryear ~ basin, scales = 'free_y') }
    
    if ( input$free_scale == "fixed"  )
    {p <- p + facet_grid(wateryear ~ basin) }
  # 
  # if (facets != '. ~ .' &&  input$free_scale == "fixed"  )
  #   p <- p + facet_grid(facets) + theme(strip.text.y = element_text(angle = 0)) 

  if (input$legend == 'off')
     {p <- p +  theme(legend.position="none")} #hide_legend(p)
    
    
    print(p)
    p <- ggplotly(p) 
    
    print(p)
    
    
    
  })
  
 # output$mapplot <- renderLeaflet({
 #   
 #   # input$goButton
 #   
 #   df_map <- df %>% filter(basin_zone == "Entire Basin", date == input$map_date, p_unit %in% input$map_param)
 #   df_mapcolrange <- df %>% filter(basin_zone == "Entire Basin", p_unit %in% input$map_param)
 #   viridmax <- max(df_mapcolrange$value)
 #   viridmin <- min(df_mapcolrange$value)
 #   
 #   # Convert spatialpolydf to an sf object
 #   sf_ebasin_kml  <- ebasin_kml  %>%  st_as_sf() %>% transmute(Name, geometry)  %>%
 #     left_join(df_map, by = c("Name" = "nwscode"))
 #   
 #   sf_ebasin_kml_hlite  <- sf_ebasin_kml  %>%  
 #     filter(pillow %in% input$pillow) 
 #   
 #   maptypes = c("Stamen.TonerLite", "Stamen.Terrain", "Stamen.TopOSMRelief", "Esri.WorldTopoMap" , "Esri.WorldPhysical",  "OpenTopoMap" ,
 #                "NASAGIBS.ModisTerraSnowCover", "NASAGIBS.ModisTerraTrueColorCR", "NASAGIBS.ModisTerraBands367CR")
 #   
 #   grp <- c(    "usgs hydrography",   "0.5 reflectivity") #,"hrrr p_1hr", "hrrr p_2hr",   "hrrr p_4hr", "hrrr p_6hr",
 #   #"mrms p_1hr", "mrms p_24hr", "mrms p_48hr", "mrms p_72hr") # "Coarse Geo") # 
 #   
 #   
 #   m <- mapview(sf_ebasin_kml["value"], burst = TRUE, hide = TRUE, col.regions = viridisLite::viridis, 
 #                alpha.regions = 0.4,  map.types = maptypes,
 #                popup = popupTable(sf_ebasin_kml, zcol = c("pillow", "date", "value", "pname")), 
 #                layer.name = "nohrsc daily data")   +
 #     mapview(sf_ebasin_kml_hlite["value"], color = "red", 
 #             alpha.regions = 0.0, 
 #             popup = popupTable(sf_ebasin_kml_hlite, zcol = c("pillow", "date", "value", "pname")),
 #             layer.name = "selected basin(s' ) outline", legend = FALSE) 
 #   
 #   m@map = m@map %>% 
 #     
 #     addTiles() %>%
 #     setView(-119.6, 38.05, zoom = 7)  %>%   
 #     
 #     addWMSTiles(group= grp[1], baseUrl="https://basemap.nationalmap.gov/arcgis/services/USGSHydroCached/MapServer/WmsServer", layers = "0",
 #                 options = WMSTileOptions(format = "image/png", transparent = TRUE), attribution = "USGS") %>% 
 #     
 #     
 #     
 #     addWMSTiles( group = grp[2],baseUrl = 
 #                    "http://mesonet.agron.iastate.edu/cgi-bin/wms/nexrad/n0r.cgi", 
 #                  #"https://mesonet.agron.iastate.edu/cgi-bin/wms/nexrad/n0q.cgi?",
 #                  layers = "nexrad-n0r-900913",
 #                  options = WMSTileOptions(format = "image/png", transparent = TRUE),
 #                  attribution = "Weather data  2012 IEM Nexrad") %>%
 #     
 #     # addWMSTiles( group = grp[3],baseUrl = 
 #     #                "https://gibs.earthdata.nasa.gov/twms/epsg4326/best/twms.cgi", 
 #     #              #"https://mesonet.agron.iastate.edu/cgi-bin/wms/nexrad/n0q.cgi?",
 #     #              layers = "0",
 #     #              options = WMSTileOptions(format = "image/png", transparent = TRUE),
 #     #              attribution = "NASA GIBS imagery") %>%
 #     #
 #     
 #     
 #     
 #   mapview:::mapViewLayersControl(names = grp) %>% #hideGroup(grp[1]) #%>% 
 #     hideGroup(grp[2]) 
 #   
 #   
 #   
 #   m@map
 # })
  
}

shinyApp(ui, server)