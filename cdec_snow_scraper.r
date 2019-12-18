rm(list = ls())
rstudioapi::getActiveDocumentContext
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("libs.r")
source("fun_defs.r")


cdec_swe_table <- "http://cdec.water.ca.gov/reportapp/javareports?name=PAGE6"
cdec_swe_table  <- readHTMLTable(cdec_swe_table )


cdec_swe_table_1 <- cdec_swe_table[[1]]
cdec_swe_table_2 <- cdec_swe_table[[2]]
cdec_swe_table_3 <- cdec_swe_table[[3]]
cdec_swe_table_4 <- cdec_swe_table[[4]]


cdec_swe_table  <- rbind(cdec_swe_table_1, cdec_swe_table_2, 
                         cdec_swe_table_3, cdec_swe_table_4)

head(cdec_swe_table)
rm(cdec_swe_table_1, cdec_swe_table_2, cdec_swe_table_3, cdec_swe_table_4)

# clean up table..first add "NA" for any 'blank' ID

cdec_swe_table <- cdec_swe_table %>% mutate(ID = 
                  ifelse(ID %in% c(""," ","  ", "NA"), NA, as.character(ID)))
head(cdec_swe_table)

# clean up table..remove any row without an "ID"
cdec_swe_table <- completeFun(cdec_swe_table, "ID")
# clean up table..remove any row with "ID" for "ID"
cdec_swe_table <- cdec_swe_table[!grepl("ID", cdec_swe_table$ID),]

head(cdec_swe_table)

cdec_stations <- read_csv("cdec_stations.csv")

cdec_swe_table <- left_join(cdec_swe_table, cdec_stations)
rm(cdec_stations)

head(cdec_swe_table)

#colnames <- colnames(cdec_swe_table)
#colnames

cdec_swe_table <- cdec_swe_table %>% transmute(Station, ID, oper, dwr_elev,
                  lat, lon, dwrapr1mean, swe_latest = `Today (IN)`, 
                  swe_percapr1 = `Percent Apr 1`, swe_24hrprev = `24 Hrs Ago (IN)`, 
                  swe_7dayprev = `1 Week Ago (IN)`)

head(cdec_swe_table)

cdec_swe_table$swe_latest <- gsub("r", "", cdec_swe_table$swe_latest)
cdec_swe_table$swe_latest <- gsub("e", "", cdec_swe_table$swe_latest)
cdec_swe_table$swe_latest <- gsub("---", NA, cdec_swe_table$swe_latest)

cdec_swe_table$swe_percapr1 <- gsub("%", "", cdec_swe_table$swe_percapr1)
cdec_swe_table$swe_percapr1 <- gsub("---", NA, cdec_swe_table$swe_percapr1)

cdec_swe_table$swe_24hrprev <- gsub("r", "",   cdec_swe_table$swe_24hrprev)
cdec_swe_table$swe_24hrprev <- gsub("e", "",   cdec_swe_table$swe_24hrprev)
cdec_swe_table$swe_24hrprev <- gsub("---", NA, cdec_swe_table$swe_24hrprev)

cdec_swe_table$swe_7dayprev <- gsub("r", "",   cdec_swe_table$swe_7dayprev)
cdec_swe_table$swe_7dayprev <- gsub("e", "",   cdec_swe_table$swe_7dayprev)
cdec_swe_table$swe_7dayprev <- gsub("---", NA, cdec_swe_table$swe_7dayprev)

cdec_swe_table <- cdec_swe_table %>% mutate(swe_percapr1 = as.numeric(swe_percapr1 ),
                                            swe_latest = as.numeric(swe_latest ),
                                             swe_24hrprev = as.numeric(swe_24hrprev ),
                                              swe_7dayprev = as.numeric(swe_7dayprev))
cdec_swe_table <- cdec_swe_table %>% mutate(dailydiff =  swe_latest - swe_24hrprev,
                                            weeklydiff = swe_latest - swe_7dayprev)

cdec_swe_table <- st_as_sf(cdec_swe_table, coords = c("lon", "lat"), crs = 4326) 


