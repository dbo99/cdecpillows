# new cdec report format starts 11/27/2018
# last day of available old style format is 6/14/18
source("fun_defs.r")
source("libs.r")
source("df_init.r")
dayswithdata <- read_csv("cdecpillowdatadays_wy17wy18.csv") %>%
                mutate(daywithdata = mdy(daywithdata)) %>% 
               filter(daywithdata <= ymd("2017-10-01"))
                
# 
# dayswithdata defined in scraper_datadays.r

{
for (i in 1:length(dayswithdata$url)) { 
  
  cdec_swe_table <- dayswithdata$url[i]
  cdecday <- dayswithdata$daywithdata[i]

  
  cdec_swe_table  <- readHTMLTable(cdec_swe_table) #%>% unlist()
  
  numsubtables <- as.integer(length(cdec_swe_table))
  
  # some cdec days are incomplete, some cdec now have 2 subtables, some 4 (room here for 5)
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
  
  #for older pre java style cdec tables
  cdec_swe_table <- cdec_swe_table %>% row_to_names(row_number = 1)
  
  
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
  
#  cdec_swe_table <- cdec_swe_table %>% transmute(Station, ID,
#                                                 swe_apr1mean = `Apr 1 Avg (IN)`, swe_latest = `Today (IN)`, 
#                                                 swe_percapr1 = `Percent Apr 1`, swe_24hrprev = `24 Hrs Ago (IN)`, 
#                                                 swe_7dayprev = `1 Week Ago (IN)`)

  # older pre java tables have fewer spaces in column names (than above)  
  
cdec_swe_table <- cdec_swe_table %>% transmute(Station, ID,
                  swe_apr1mean = `Apr 1 Avg(IN)`, swe_latest = `Today(IN)`, 
                  swe_percapr1 = `PercentApr 1`, swe_24hrprev = `24 Hrs Ago(IN)`, 
                  swe_7dayprev = `1 Week Ago(IN)`)
  
  head(cdec_swe_table)
  
  
  cdec_swe_table <- cdec_swe_table %>% pivot_longer(3:ncol(cdec_swe_table))
  #head(cdec_swe_table2)
  
  # remove any non numeric characters
  cdec_swe_table$value <- as.numeric(gsub("[^0-9.-]", "", cdec_swe_table$value))
  head(cdec_swe_table)
  
  # pivot wider to calculate some new difference parameters - edit - do after filter now
  
  #cdec_swe_table <- cdec_swe_table[complete.cases(cdec_swe_table), ]
  #cdec_swe_table <- cdec_swe_table %>% pivot_wider(names_from = name)
  #cdec_swe_table <- cdec_swe_table %>% mutate(dailydiff = swe_latest - swe_24hrprev)
  #cdec_swe_table <- cdec_swe_table %>% mutate(weeklydiff = swe_latest - swe_7dayprev)
  #cdec_swe_table <- cdec_swe_table %>% pivot_longer(3:ncol(cdec_swe_table))
  cdec_swe_table <- cdec_swe_table %>% transmute(station = Station, dwr3id = ID, pname = name,
                                                 value, cdecday = cdecday)
  cdec_swe_table
  
  df_i <- cdec_swe_table
  df_i <- df_i[complete.cases(df_i), ]
  as_tibble(df_i)
  # remove non-key parameters, can calculate differences after filtering
  df_i <- df_i %>% filter(pname != "swe_apr1mean", 
                          pname != "swe_7dayprev", 
                          pname != "swe_24hrprev")
  df <- rbind(df,df_i) 
  message(dayswithdata$daywithdata[i])
  
} 



filename <- "cdecpillow_wy17wy18archive.csv"
#setwd("~/R/proj/nohrsc/shiny/final/firstpublish")
write_csv(df,filename)
                }
