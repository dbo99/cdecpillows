maptypes = c(
  
  "Stamen.TopOSMRelief", 
  
  "Esri.WorldPhysical",  
  "OpenTopoMap" )
pal = mapviewPalette("mapviewSpectralColors")
m <- mapview(cdec_swe_table["swe_latest"], #burst = TRUE, hide = TRUE, 
             #col.regions = RColorBrewer::RdBu, 
             col.regions = pal(100),
             #at = seq(min_rt, max_rt+2500, 2500),
             cex = cdec_swe_table$swe_latest,  #24
             #cex = df_w[31],  #29
             #cex = "storage_dailychange_af_instant_usbr_2019-06-23",  #14          
             alpha.regions = 0.3,
             map.types = maptypes,
             legend = TRUE,
             popup = popupTable(cdec_swe_table),
             #popup = popupTable(gage_points, zcol = c("STATION_NM", "usgs_id", "cfs", "stage_ft", "hist_pkflow",
             #                                         "usgs_nwsfloodstg_ft", "dt")),
             #lwd = nhd_fyh_sf$Q0001A/1000,
             layer.name =  "layername") 

m
