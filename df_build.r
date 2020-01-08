rm(list = ls()) 

#source("libs.r")
#source("fun_defs.r")

dowy <- as.data.frame(fread("daily_dowy.csv")) %>% 
                         mutate(date = as.character(date), date = ymd(date))
cdec_to_nws_pillowcodes <- as.data.frame(fread("cdec_to_nws_pillowcodes.csv"))
df_cumdoy <- read_csv("leap_yrs.csv") 


df <- as.data.frame(fread("fulldb_wy17thru_2020-01-08.csv"))
df <- df %>% mutate(date = as.character(cdecday)) %>% mutate(date = ymd(date)) %>% select(-cdecday)
df <- df %>% mutate(year = year(date), yday = yday(date))

as_tibble(df)

df$cumdoy <- df_cumdoy$cumdoy[match(df$year,df_cumdoy$year)] 
as_tibble(df)
df$dowy <- dowy$dowy[match(df$date,dowy$date)]   #faster than dplyr _join
as_tibble(df)
as_tibble(tail(df))


df <- df %>% mutate(nws_basin_code = as.factor(paste0(basin," (", nwscode, ")")), nwscode = as.factor(nwscode), basin = as.factor(basin),
                    param = as.factor(param), basin_zone = as.factor(basin_zone)) %>% select(-cumdoy, -yday)


ebasin_kml <- readOGR("basins.kml", "cnrfc_09122018_basins_thin")


