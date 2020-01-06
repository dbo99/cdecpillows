rm(list = ls())
#rstudioapi::getActiveDocumentContext
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("libs.r")
source("fun_defs.r")


page <- "https://cdec.water.ca.gov/reportapp/javareports?name=PAGE6.20200102"


start_date <- ymd("2019-11-26")
end_date <- ymd("2020-01-06")

dates <- seq(start_date, end_date, by = "days") %>% rev()

# for (i in 1:length(dates)) { 
#
#  year <- as.character(year(dates[i]))
#  monthnum <- as.character(month(dates[i]))
#  dayofmon <- as.character(day(dates[i]))
#  
  
cdec_swe_table <- "http://cdec.water.ca.gov/reportapp/javareports?name=PAGE6.20191230"

date <- mdy(html_text(html_node(read_html(page),"h2 , h2"))) #.h2 & pre Id'd by SelectorGadget

date1 <- gsub("-","",date)

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

#cdec_stations <- read_csv("cdec_stations.csv")

#cdec_swe_table <- left_join(cdec_swe_table, cdec_stations)
rm(cdec_stations)

head(cdec_swe_table)

#colnames <- colnames(cdec_swe_table)
#colnames

cdec_swe_table <- cdec_swe_table %>% transmute(Station, ID,
                  dwrapr1mean = `Apr 1 Avg (IN)`, swe_latest = `Today (IN)`, 
                  swe_percapr1 = `Percent Apr 1`, swe_24hrprev = `24 Hrs Ago (IN)`, 
                  swe_7dayprev = `1 Week Ago (IN)`)

head(cdec_swe_table)


cdec_swe_table2 <- cdec_swe_table %>% pivot_longer(3:7)
head(cdec_swe_table2)

cdec_swe_table2$value <- as.numeric(gsub("[^0-9.-]", "", cdec_swe_table2$value))
head(cdec_swe_table2)


cdec_swe_table <- st_as_sf(cdec_swe_table, coords = c("lon", "lat"), crs = 4326) 


# -----------------------------

zones <- readOGR(".", "cnrfc_zones_11052019_wgs84_thin_0.1_ret") %>% st_as_sf()
basins <- readOGR(".", "cnrfc_basins_11052019_wgs84_unprojected_0.3_ret") %>% st_as_sf()
