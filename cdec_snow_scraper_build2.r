rm(list = ls())
#rstudioapi::getActiveDocumentContext
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("libs.r")
source("fun_defs.r")


#### make list of days for which cdec published pillow data ###
## 12-11-2019 is incomplete - removed from csv output manually


## commented out - code to scrape and find out which days have data
## now - just read in .csv for efficiency


#start_date <- ymd("2018-12-10")
#end_date <- ymd("2018-12-13")
#
#dates <- seq(start_date, end_date, by = "days") #%>% rev()
#dates <- gsub("-","",dates)
#urlbase <- "http://cdec.water.ca.gov/reportapp/javareports?name=PAGE6."



#dayswithdata <- data.frame(h2 = character(), stringsAsFactors = FALSE)
#dayswithdata <- vector(mode = "list")
#head(dayswithdata)
#
#for (i in 1:length(dates)) { 
#
#url <- paste0(urlbase, dates[i])
#dayswithdata_i <- html_text(html_node(read_html(url),"h2 , h2"))
#   
#dayswithdata <- append(dayswithdata,dayswithdata_i) #%>% unique()
#message(dates[i])
#
#}
#   
#dayswithdata
#dayswithdata <- dayswithdata %>% unlist() %>% as.data.frame()
## rename column
#colnames(dayswithdata)[colnames(dayswithdata)=="."] <- "h2"
#head(dayswithdata)
#dayswithdata <- dayswithdata %>% mutate(h2 = mdy(h2), h2 = as.character(h2))
#as_tibble(dayswithdata)
## remove hyphens to match url format
#dayswithdata$h2 <- gsub("-","",dayswithdata$h2)
#as_tibble(dayswithdata)
#
## remove the days with no data (the NAs)
#dayswithdata <- dayswithdata[complete.cases(dayswithdata), ]
#dayswithdata <- data.frame(dayswithdata)
#
#as_tibble(dayswithdata)
#
### make list of urls of these days with data
#
#dayswithdata <- dayswithdata %>% transmute(url = paste0(urlbase, dayswithdata), dayswithdata = 
#                                             ymd(dayswithdata))
#as_tibble(dayswithdata)  
#dayswithdata <- dayswithdata #%>% filter(dayswithdata != "2018-12-11",
                                        #dayswithdata != "2019-01-22")

dayswithdata <- read_csv("dayswithdata.csv") %>% mutate(dayswithdata = mdy(dayswithdata))


## scrape each day of data  

source("df_init.r")
# 

for (i in 1:length(dayswithdata$url)) { 
  
cdec_swe_table <- dayswithdata$url[i]
cdecday <- dayswithdata$dayswithdata[i]

## debug to delete
#cdec_swe_table <- "http://cdec.water.ca.gov/reportapp/javareports?name=PAGE6.20200106"
#cdec_swe_table <- "http://cdec.water.ca.gov/reportapp/javareports?name=PAGE6.20190122"
## debug to delete

cdec_swe_table  <- readHTMLTable(cdec_swe_table) #%>% unlist()

numsubtables <- as.integer(length(cdec_swe_table))

x_cdec_swe_table_1 <- cdec_swe_table[[1]]
if (numsubtables > 1) { x_cdec_swe_table_2 <- cdec_swe_table[[2]] }
if (numsubtables > 2) { x_cdec_swe_table_3 <- cdec_swe_table[[3]] }
if (numsubtables > 3) { x_cdec_swe_table_4 <- cdec_swe_table[[4]] }
if (numsubtables > 4) { x_cdec_swe_table_5 <- cdec_swe_table[[5]] }

## bind into table -make sure not to name any other variable beginning with x
cdec_swe_table <- do.call(rbind, mget(ls(pattern="^x_")))

head(cdec_swe_table)
rm(list = ls(pattern = "^x_"))

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
#rm(cdec_stations)

head(cdec_swe_table)

#colnames <- colnames(cdec_swe_table)
#colnames

cdec_swe_table <- cdec_swe_table %>% transmute(Station, ID,
                  swe_apr1mean = `Apr 1 Avg (IN)`, swe_latest = `Today (IN)`, 
                  swe_percapr1 = `Percent Apr 1`, swe_24hrprev = `24 Hrs Ago (IN)`, 
                  swe_7dayprev = `1 Week Ago (IN)`)

head(cdec_swe_table)


cdec_swe_table <- cdec_swe_table %>% pivot_longer(3:ncol(cdec_swe_table))
#head(cdec_swe_table2)

# remove any non numeric characters
cdec_swe_table$value <- as.numeric(gsub("[^0-9.-]", "", cdec_swe_table$value))
head(cdec_swe_table)

# pivot wider to calculate some new difference parameters

#cdec_swe_table <- cdec_swe_table %>% pivot_wider(names_from = name)
#cdec_swe_table <- cdec_swe_table %>% mutate(dailydiff = swe_latest - swe_24hrprev)
#cdec_swe_table <- cdec_swe_table %>% pivot_longer(3:ncol(cdec_swe_table))
cdec_swe_table <- cdec_swe_table %>% transmute(station = Station, dwr3id = ID, pname = name,
                                               value, cdecday = cdecday)
cdec_swe_table



df_i <- cdec_swe_table
df_i <- df_i[complete.cases(df_i), ]
df <- rbind(df,df_i) 
message(dayswithdata$dayswithdata[i])

} 

cdec_swe_table <- st_as_sf(cdec_swe_table, coords = c("lon", "lat"), crs = 4326) 
head(cdec_swe_table)
# -----------------------------

zones <- readOGR(".", "cnrfc_zones_11052019_wgs84_thin_0.1_ret") %>% st_as_sf()
basins <- readOGR(".", "cnrfc_basins_11052019_wgs84_unprojected_0.3_ret") %>% st_as_sf()
