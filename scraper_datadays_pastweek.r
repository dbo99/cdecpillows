rm(list = ls())
#setwd("~/R/proj/nohrsc/shiny/final/firstpublish")
source("libs.r")
source("fun_defs.r")
source("df_init.r")
#source("archive_readin.r")


#### make list of recent days for which cdec published pillow data ###
{
# b/c data consistently revised, start from week before day through which we already have data
start_date <- Sys.Date() - 7
# assume we want data through today
end_date <- Sys.Date()          #ymd("2020-01-08")  

dates <- seq(start_date, end_date, by = "days") #%>% rev()
dates <- gsub("-","",dates)
urlbase <- "http://cdec.water.ca.gov/reportapp/javareports?name=PAGE6."


#initialize df
#dayswithdata <- data.frame(h2 = character(), stringsAsFactors = FALSE)
dayswithdata <- vector(mode = "list")
head(dayswithdata)

for (i in 1:length(dates)) { 

url <- paste0(urlbase, dates[i])
dayswithdata_i <- html_text(html_node(read_html(url),"h2 , h2"))
   
dayswithdata <- append(dayswithdata,dayswithdata_i) #%>% unique()
message(dates[i])

}
   
dayswithdata
dayswithdata <- dayswithdata %>% unlist() %>% as.data.frame()
as_tibble(dayswithdata)
# rename column
colnames(dayswithdata)[colnames(dayswithdata)=="."] <- "h2"
as_tibble(dayswithdata)
# remove no data days with mdy (long strings won't parse as dates)
dayswithdata <- dayswithdata %>% mutate(h2 = mdy(h2), h2 = as.character(h2))
as_tibble(dayswithdata)
# remove hyphens to match url format
dayswithdata$h2 <- gsub("-","",dayswithdata$h2)
as_tibble(dayswithdata)

# remove the days with no data (the NAs)
dayswithdata <- dayswithdata[complete.cases(dayswithdata), ]
as_tibble(dayswithdata)
dayswithdata <- data.frame(dayswithdata)

as_tibble(dayswithdata)

## make list of urls of these days with data

dayswithdata <- dayswithdata %>% transmute(url = paste0(urlbase, dayswithdata), daywithdata = 
                                             ymd(daywithdata))
as_tibble(dayswithdata)  
# make sure no duplicates
dayswithdata <- dayswithdata %>% distinct()
as_tibble(dayswithdata) 

}


