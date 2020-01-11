rm(list = ls())
#setwd("~/R/proj/nohrsc/shiny/final/firstpublish")
source("libs.r")
source("fun_defs.r")
source("df_init.r")
#source("archive_readin.r")


#### make list of recent days for which cdec published pillow data ###
#{
# b/c data consistently revised, start from week before day through which we already have data
start_date <- ymd("2006-10-01")
# assume we want data through today
#end_date <- ymd("2006-07-19")  
end_date <- ymd("2007-06-16") 
dates <- seq(start_date, end_date, by = "days") #%>% rev()
dates <- gsub("-","",dates)
urlbase <- "http://cdec.water.ca.gov/reportapp/javareports?name=PAGE6."

dayswithdata <- data.frame(dates) %>% mutate(url = paste0(urlbase, dates), 
                                             daywithdata = ymd(dates)) %>% select(-dates)



write_csv(dayswithdata, "cdecpillowdatadays_wy07.csv")
