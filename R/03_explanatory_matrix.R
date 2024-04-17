# re-run 4/17 after removing original ACE files (ACE has split into 2 files)

# updated 4/16/24 to get updates and try to address joining issues
# updated 2/20/24 (general updates)

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
           everything()) %>% 
    mutate(`SET change` = unlist(`SET change (mm/yr)`)) %>% 
    select(-"SET change (mm/yr)")

# a row for every year at each reserve - will be repeated for every site/transect/plot within a year
# but be different in different years
time_component_yes <- read_sheet(expl_sheet, 
                             sheet = "Time added",
                             skip = 6,
                             na = c("", "NA"))  %>% 
    janitor::remove_empty("cols") %>% 
    rename(Reserve = `Reserve File`)

# make sure ACE shows up as both ACE-EIN and ACE-EIS
# that should be the only one with problems
ace2 <- time_component_yes %>% 
    filter(Reserve == "ACE") %>% 
    mutate(Reserve = "ACE-EIN")
time_component_yes <- time_component_yes %>% 
    mutate(Reserve = case_when(Reserve == "ACE" ~ "ACE-EIS",
                               .default = Reserve)) %>% 
    bind_rows(ace2)
rm(ace2)


# average time-based components to reserve level  
time_based_avgs <- time_component_yes %>% 
    summarize(.by = Reserve,
              across(c(`Local tidal range`,
                `Climate - temp avg`,
                `Climate - precip sum (avg)`),
              function(x) mean(x, na.rm = TRUE)))

# read in veg file  
veg <- read.csv(here::here("data", "compiled",
                           "ALL_veg-grouped.csv"))



# join into one data frame 
veg_and_expl <- left_join(veg, time_component_no, by = c("Reserve", "SiteID")) %>% 
    left_join(time_component_yes, by = c("Reserve", "Year"))


# write out RData file with the data frames separate (but joinable)  
save(veg, time_component_no, time_component_yes, time_based_avgs,
     file = here::here("data", "compiled", "veg_and_expl_dfs.RData"))

# write out joined csv  
write.csv(veg_and_expl,
          file = here::here("data", "compiled", "veg_and_expl.csv"),
          row.names = FALSE,
          na = "")
