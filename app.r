#setwd("~/R/proj/nohrsc/shiny/final/firstpublish")
#Sys.setenv(TZ="America/Los_Angeles")
#setwd("~/R/proj/nohrsc/shiny")
#setwd("~/Documents/shiny_nohrsc/final")
#setwd("~/R/proj/nohrsc/shiny/final")

source("cdec_snow_scraper.r")


#ui <- 

#shinyUI(fluidPage(

ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(),
  dashboardBody(
    
    tags$style(type = "text/css", "#mapplot {height: calc(100vh - 80px) !important;}"),
    leafletOutput("mapplot"),
    useShinyjs()
  )
)
#useShinyjs(),


#actionButton("button", "download full page\nstand alone html") ,

#  verticalLayout(
#    
#    # Sidebar with a slider input
#    wellPanel(
#      sliderInput("symbol_size",
#                  "symbol size factor:",
#                  min = 0.01,
#                  max = 100,
#                  value = 1)
#    ),

# Show a plot of the generated distribution
# mainPanel(

#tabsetPanel(position=c("right"),

#  tabPanel(strong("image"), 
#           br(),
#           plotOutput("reg_plot",  height = "750px")) ,
#  
#  tabPanel(strong("interactive"), 
#           br(),
#           plotlyOutput("plotly_plot",  height = "750px")) ,
#  
# tabPanel(strong("map1"), 
# br(),
# leafletOutput("mapplot",  height = "750px")) ,

#           tabPanel(strong("map"), 
#                    br(),
#                   leafletOutput("mapplot",  height = "750px")) )))
#                   #leafletOutput("mapplot")) )))
#)


########################
#### server.r
########################

date <- Sys.Date()

server <- function(input, output) {
  addClass(selector = "body", class = "sidebar-collapse")
  options(shiny.maxRequestSize=225*1024^2) 
  
  output$mapplot <- renderLeaflet({
    
    mapviewOptions(basemaps = c("CartoDB.DarkMatter", "CartoDB.Positron"),
                   raster.palette = grey.colors,
                   vector.palette = colorRampPalette(c("snow", "cornflowerblue", "grey10")),
                   na.color = "magenta")
                   #layers.control.pos = "topright")
    
    maptypes = c(
      "Stamen.TopOSMRelief", 
      "Stamen.TerrainBackground",
      "NASAGIBS.ModisTerraTrueColorCR",
      #"ESRI.WorldImagery",
      "Esri.WorldPhysical") 
      #"OpenTopoMap" )
    
    grp <- c(    "usgs hydrography",   
                  "swe_latest",
                 "swe_24hrprev",           
                 "swe_7dayprev",           
                 "swe_dailydiff",
                 "swe_weeklydiff",
                 "swe_percapr1",
                 "dwrapr1mean",
                 "dwr_elev",
                 "basins",
                 "zones") 
              
    
    
    lopt <- labelOptions(noHide = TRUE,
                         direction = 'top',
                         textOnly = TRUE)


    m <- mapview(cdec_swe_table["swe_latest"], #burst = TRUE, hide = TRUE, 
                 col.regions = viridisLite::viridis, 
                 cex = cdec_swe_table$swe_latest,
                 alpha.regions = 0.5,
                 map.types = maptypes,
                 legend = TRUE ,
                 popup = popupTable(cdec_swe_table, zcol = 
                                      colnames(cdec_swe_table)),
                 layer.name = "swe_latest")  +
    
      
      mapview(cdec_swe_table["swe_24hrprev"], 
              col.regions = viridisLite::viridis, 
              cex = cdec_swe_table$swe_24hrprev,
              alpha.regions = 0.5,
              map.types = maptypes,
              legend = TRUE,
              popup = popupTable(cdec_swe_table, zcol = 
                                   colnames(cdec_swe_table)),
              layer.name = "swe_24hrprev")     +
      
      mapview(cdec_swe_table["swe_7dayprev"], 
              col.regions = viridisLite::viridis, 
              cex = cdec_swe_table$swe_7dayprev,
              alpha.regions = 0.5,
              map.types = maptypes,
              legend = TRUE,
              popup = popupTable(cdec_swe_table, zcol = 
                                   colnames(cdec_swe_table)),
              layer.name = "swe_7dayprev")     +
      
      mapview(cdec_swe_table["swe_dailydiff"], 
              col.regions = viridisLite::viridis, 
              cex = cdec_swe_table$swe_dailydiff * 50,
              alpha.regions = 0.5,
              map.types = maptypes,
              legend = TRUE,
              popup = popupTable(cdec_swe_table, zcol = 
                                   colnames(cdec_swe_table)),
              layer.name = "swe_dailydiff")     +
      
      mapview(cdec_swe_table["swe_weeklydiff"], 
              col.regions = viridisLite::viridis, 
              cex = cdec_swe_table$swe_weeklydiff * 10,
              alpha.regions = 0.5,
              map.types = maptypes,
              legend = TRUE,
              popup = popupTable(cdec_swe_table, zcol = 
                                   colnames(cdec_swe_table)),
              layer.name = "swe_weeklydiff")     +
      
      mapview(cdec_swe_table["dwrapr1mean"], 
              col.regions = viridisLite::viridis, 
              cex = cdec_swe_table$dwrapr1mean,
              alpha.regions = 0.5,
              map.types = maptypes,
              legend = TRUE,
              popup = popupTable(cdec_swe_table, zcol = 
                                   colnames(cdec_swe_table)),
              layer.name = "dwrapr1mean")     +
      
      mapview(cdec_swe_table["swe_percapr1"], 
              col.regions = viridisLite::viridis, 
              cex = cdec_swe_table$swe_percapr1/2,
              alpha.regions = 0.5,
              map.types = maptypes,
              legend = TRUE,
              popup = popupTable(cdec_swe_table, zcol = 
                                   colnames(cdec_swe_table)),
              layer.name = "swe_percapr1")     +
      
      mapview(cdec_swe_table["dwr_elev"], 
              col.regions = viridisLite::viridis, 
              cex = cdec_swe_table$dwr_elev/1000 ,
              alpha.regions = 0.5,
              map.types = maptypes,
              legend = TRUE,
              popup = popupTable(cdec_swe_table, zcol = 
                                   colnames(cdec_swe_table)),
              layer.name = "dwr_elev")     +
      
      
      mapview(basins["Basin"],
              alpha.regions = 0.0,
              color = "black",
              layer.name = "basins",
              legend = FALSE) + 
      
      
      mapview(zones["Name"],
              alpha.regions = 0.0,
              color = "black",
              layer.name = "zones",
              legend = FALSE) 
      

    
    
    m@map = m@map %>% 
      
      addTiles() %>%
      #leaflet(height = "100%") %>%
      setView(-119.6, 38.05, zoom = 5)  %>%   
      
      addWMSTiles(group= grp[1], baseUrl="https://basemap.nationalmap.gov/arcgis/services/USGSHydroCached/MapServer/WmsServer", layers = "0",
                  options = WMSTileOptions(format = "image/png", transparent = TRUE), attribution = "USGS") %>% 
      

      
      mapview:::mapViewLayersControl(names = grp) %>% 
      #hideGroup(grp[1]) %>% 
      hideGroup(grp[3:11]) 
    
  #  m@map <- m@map %>% addStaticLabels(m@map, data = cdec_usgs_cnrfc, label = cdec_usgs_cnrfc$cfs_most_recent, labelOptions = lopt) %>% 
   #   setView(-119.6, 38.05, zoom = 6.5) 
    m@map
    #mapview:::mapViewLayersControl(names = grp) 
    #m@map
    #m
    
    
   # observeEvent(input$button, {
      
     # saveWidget(m@map, file= paste0("publicgauges_", date, ".html"), selfcontained = TRUE)
      #mapshot(m@map, url = "mydog.html")
  #  })
    
    m@map     
    
  }
  )}
shinyApp(ui, server)