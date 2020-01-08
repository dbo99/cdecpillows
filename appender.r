


{
new <-  as.data.frame(fread("2020-01-08.csv")) %>% mutate(cdecday = as.character(cdecday)) %>% 
        mutate(cdecday = ymd(cdecday)) #%>% mutate(wy = water_year(date), yday = yday(date))

if (nrow(new) > 0 ) {justscraped_lastdate <- max(new$cdecday)}

#new <- new[,order(colnames(new))] 
if (nrow(new) > 0 ) {lastdate <- justscraped_lastdate}
if (nrow(new) == 0) {lastdate <- max(archive$cdecday)}


# remove past week of archive data 
if (nrow(new) > 0 ) { freshenlastarchiveweekthru <- justscraped_lastdate - numdaystogoback 
archive <- archive %>% filter(cdecday < freshenlastarchiveweekthru)
}
}

fulldb <- rbind(new, archive)
#ensure no duplicates
fulldb <- distinct(fulldb)
as_tibble(fulldb)
as_tibble(tail(fulldb))
filename <- paste0("fulldb_wy17thru_", lastdate, ".csv")
write_csv(fulldb, filename)

#overwrite df of recent data as full df
df <- fulldb

rm(fulldb, df_i, cdec_swe_table)




