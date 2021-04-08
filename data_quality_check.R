library(osfr)

data_source <- "input-data/COVerAGE-DB/Output_5.zip"
link_old <- "https://osf.io/7tnfh/download?version=135&displayName=Output_5-2021-01-13T07%3A22%3A39.845954%2B00%3A00.zip"

osf_retrieve_file("7tnfh") %>%
  osf_download(path = "input-data/COVerAGE-DB/", conflicts = "overwrite")

db_cov <-  read_csv("input-data/COVerAGE-DB/Output_5.zip",
                    skip = 3)

db_cov2 <- 
  db_cov %>% 
  filter(Sex == "b",
         Region == "All") %>% 
  mutate(Date = dmy(Date)) %>% 
  group_by(Country) %>% 
  filter(Date == max(Date)) %>% 
  ungroup()


east <- c("Croatia", "Romania", "Malta", "Slovakia", "Poland", "Lithuania")

db_east <- 
  db_cov2 %>% 
  filter(Country %in% east) 


db_east %>% 
  group_by(Country, Date) %>% 
  summarise(Deaths = sum(Deaths))




