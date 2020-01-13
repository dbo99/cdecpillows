rm(list = ls()) 

source("libs.r")
source("fun_defs.r")

dowy <- as.data.frame(fread("daily_dowy.csv")) %>% 
                      mutate(date = as.character(date), 
                             date = ymd(date))
as_tibble(dowy)
cdec_to_nws_pillowcodes <- as.data.frame(fread("cdec_to_nws_pillowcodes.csv"))
as_tibble(cdec_to_nws_pillowcodes)
df_cumdoy <- read_csv("leap_yrs.csv") 
as_tibble(df_cumdoy)

cdec_stations <- fread("cdec_stations.csv") %>% transmute(dwr3id, elev_cdec,lat,lon)
as_tibble(cdec_stations)


df <- as.data.frame(fread("fulldb_wy17thru_2020-01-13.csv")) %>% 
             mutate(date = as.character(cdecday)) %>% 
             mutate(date = ymd(date)) %>% select(-cdecday)  %>%
              mutate(year = year(date), yday = yday(date))

as_tibble(df)

# join key parameters
df$cumdoy <- df_cumdoy$cumdoy[match(df$year,df_cumdoy$year)] 
as_tibble(df)
df$dowy <- dowy$dowy[match(df$date,dowy$date)]   
as_tibble(df)
df$nws5id <- cdec_to_nws_pillowcodes$nws5id[match(df$dwr3id,cdec_to_nws_pillowcodes$dwr3id)] 
as_tibble(df)
df$basin <- cdec_to_nws_pillowcodes$river[match(df$dwr3id,cdec_to_nws_pillowcodes$dwr3id)] 
as_tibble(df)
df$elev_cdec <- cdec_stations$elev_cdec[match(df$dwr3id,cdec_stations$dwr3id)] 
as_tibble(df)
df <- df %>% mutate(wateryear = water_year(date))
as_tibble(df)


df <- df %>% mutate(pillow = as.factor(paste0(station,
                                              " (", 
                                              basin, ", ", elev_cdec, 
                                               #" ft",
                                              ") (",
                                              nws5id, ") (", 
                                              dwr3id, ")" )),
                    nws5id = as.factor(nws5id), basin = as.factor(basin),
                    pname = as.factor(pname)) %>% 
                    select(-cumdoy, -yday) %>%
                    mutate(date_md = format(date, format = "%m-%d")) #%>%
                    #mutate(date_ch = as.character(date))
                    
as_tibble(df)

df <- with(df, df[order(basin,elev_cdec) , ])
df$pillow <- gsub("'","",df$pillow)

#df <- df %>% mutate(swe = swe_latest) %>% select(-swe_latest)                    
#ebasin_kml <- readOGR("basins.kml", "cnrfc_09122018_basins_thin")


