
wy01 <- read_csv("cdecpillow_wy01archive.csv") %>% mutate(cdecday = ymd(cdecday))  
wy02 <- read_csv("cdecpillow_wy02archive.csv") %>% mutate(cdecday = ymd(cdecday))
wy03 <- read_csv("cdecpillow_wy03archive.csv") %>% mutate(cdecday = ymd(cdecday))
wy04 <- read_csv("cdecpillow_wy04archive.csv") %>% mutate(cdecday = ymd(cdecday))
wy05 <- read_csv("cdecpillow_wy05archive.csv") %>% mutate(cdecday = ymd(cdecday))
wy06 <- read_csv("cdecpillow_wy06archive.csv") %>% mutate(cdecday = ymd(cdecday))
wy07 <- read_csv("cdecpillow_wy07archive.csv") %>% mutate(cdecday = ymd(cdecday))
wy08 <- read_csv("cdecpillow_wy08archive.csv") %>% mutate(cdecday = ymd(cdecday))
wy0716 <- read_csv("cdecpillow_7Dec2007wy16archive.csv") %>% mutate(cdecday = ymd(cdecday))
wy1718 <- read_csv("cdecpillow_wy17wy18archive.csv") %>% mutate(cdecday = ymd(cdecday))
wy18part20 <- read_csv("fulldb_wy17thru_2020-01-08.csv") %>% mutate(cdecday = ymd(cdecday))

df <- rbind(wy01, wy02, wy03, wy04, wy05, wy06, wy07, wy08, wy0716, wy1718, wy18part20)


write_csv(df, "cdecpillowscrape_wy2001_thru8Jan2020.csv")
