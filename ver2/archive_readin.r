#archive <- read_csv("archive_wy2019_6Jan2019.csv") %>% mutate(cdecday = ymd(cdecday))
#
# data.table's fread faster than read_csv()
archive <- as.data.frame(fread("cdecpillowscrape_wy2001_thru8Jan2020.csv")) %>% 
           mutate(cdecday = as.character(cdecday)) %>% 
           mutate(cdecday = mdy(cdecday)) %>% distinct()
