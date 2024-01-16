# read in explanatory matrix from the google drive
# then connect with the combined veg data file
# and write out both separate and combined data frames

library(googlesheets4)
library(tidyverse)

expl_sheet <- "https://docs.google.com/spreadsheets/d/12LDWfJvE4Cye56O-Uv_cmGr2a4WLqcKD24OzPVrAEUI/edit?usp=sharing"


# just one row per reserve/site combination - will be repeated for every plot and date
time_component_no <- read_sheet(expl_sheet, 
                           sheet = "Time removed",
                           skip = 5) %>% # unless someone adds another row
    filter(`Reserve File` != "EXAMPLE")  %>% 
    janitor::remove_empty("cols") %>% 
    select(Reserve = `Reserve File`,
           SiteID = `Site ID (reserve files)`,
           everything())

# a row for every year at each reserve - will be repeated for every site/transect/plot within a year
# but be different in different years
time_component_yes <- read_sheet(expl_sheet, 
                             sheet = "Time added",
                             skip = 6,
                             na = c("", "NA"))  %>% 
    janitor::remove_empty("cols") %>% 
    rename(Reserve = `Reserve File`)


# read in veg file  
veg <- read.csv(here::here("data", "compiled",
                           "ALL_veg-grouped.csv"))



# join into one data frame 
veg_and_expl <- left_join(veg, time_component_no, by = c("Reserve", "SiteID")) %>% 
    left_join(time_component_yes, by = c("Reserve", "Year"))


# write out RData file with the three data frames separate (but joinable)  
save(veg, time_component_no, time_component_yes, 
     file = here::here("data", "compiled", "veg_and_expl_dfs.RData"))

# write out joined csv  
write.csv(veg_and_expl,
          file = here::here("data", "compiled", "veg_and_expl.csv"),
          row.names = FALSE,
          na = "")
