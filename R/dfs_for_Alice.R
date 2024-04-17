library(tidyverse)

load(here::here("data", "compiled", "veg_and_expl_dfs.RData"))

# reserve name matching ----
# veg/slopes_by_site don't have the suffixes
# explanatory variables do have suffixes in reserve name
resStMatching <- time_component_no %>% 
    select(File = Reserve, 
           SiteID) %>% 
    separate(File, into = c("Res", "St"),
             remove = FALSE,
             fill = "right") %>% 
    select(-St)

veg <- veg %>% 
    left_join(resStMatching, by = c("Reserve" = "Res",
                                    "SiteID")) %>% 
    relocate(File) %>% 
    mutate(Reserve2 = case_when(is.na(File) ~ Reserve,
                                File != Reserve ~ File,
                                .default = Reserve)) %>% 
    relocate(Reserve2) %>% 
    select(-File, -Reserve) %>% 
    rename(Reserve = Reserve2)

# calculations ----
# need to calculate slopes by plot
# then average up to site and/or zone
# add metric of proportion of site low vs. mid/high

# generate % low zone metrics  ----  
zone_metrics <- veg %>% 
    select(Reserve, SiteID, Vegetation_Zone, TransectID, PlotID) %>% 
    distinct() %>% 
    mutate(zone_coarse = case_match(Vegetation_Zone,
                                    "M-Mudflat" ~ "Low",
                                    "S-Seaward Edge" ~ "Low",
                                    "L-Low Marsh" ~ "Low",
                                    "T-Transition" ~ "Mid",
                                    "H-High Marsh" ~ "Mid",
                                    "UE-Upland Edge" ~ "Up",
                                    "FT-Freshwater Tidal" ~ "Up",
                                    "U-Upland" ~ "Up",
                                    .default = "Other")) %>% 
    summarize(.by = c(Reserve, SiteID),
              proportion_low = sum(zone_coarse == "Low")/n(),
              proportion_midToHigh = sum(zone_coarse == "Mid")/n(),
              proportion_uplandOrFresh = sum(zone_coarse == "Up")/n(),
              proportion_other = sum(zone_coarse == "Other")/n())

# make sure all add up to 1
unique(rowSums(zone_metrics[3:ncol(zone_metrics)]))

# veg slopes ----  
# pivot longer then nest
veg_long_nested <- veg %>% 
    mutate(ResStTrnsPlt = paste(Reserve, SiteID, TransectID, PlotID, sep = "_"),
           date = lubridate::decimal_date(lubridate::ymd(paste(Year, Month, Day)))) %>% 
    select(ResStTrnsPlt,
           date,
           Total.unvegetated:Unveg_to_veg) %>%
    pivot_longer(Total.unvegetated:Unveg_to_veg,
                 names_to = "response",
                 values_to = "cover") %>% 
    group_by(ResStTrnsPlt, response) %>% 
    nest()

# write a function to run models
model_change <- function(df){
    if(sum(!is.na(df$cover)) < 3){
        return(NA)
    }
    lm(cover ~ date, data = df)
}
model_change2 <- possibly(model_change, otherwise = NA)

# and run them on nested df
veg_long_nested <- veg_long_nested %>% 
    mutate(model = map(data, model_change2)) %>% 
    filter(!is.na(model))

# pull out slopes
slopes_long <- veg_long_nested %>% 
    select(ResStTrnsPlt, response, model) %>% 
    mutate(tidy = map(model, broom::tidy)) %>% 
    unnest(tidy) %>% 
    filter(term == "date")

slopes_wide <- slopes_long %>% 
    select(ResStTrnsPlt, response, estimate) %>% 
    separate(ResStTrnsPlt, 
             into = c("Reserve", "SiteID", "TransectID", "PlotID"),
             sep = "_") %>% 
    pivot_wider(names_from = response,
                values_from = estimate)

slopes_by_site <- slopes_wide %>% 
    select(-TransectID, -PlotID) %>% 
    summarize(.by = c(Reserve, SiteID),
              across(everything(),
                     function(x) mean(x, na.rm = TRUE)))

# explanatory slopes ----
nested_explanatory <- time_component_yes %>% 
    select(Reserve, 
           Year,
           tide_range = "Local tidal range",
           temp_avg = "Climate - temp avg",
           precip_sum = "Climate - precip sum (avg)") %>% 
    pivot_longer(tide_range:precip_sum,
                 names_to = "response",
                 values_to = "value") %>% 
    group_by(Reserve, response) %>% 
    nest()

# write a function to run models
model_change_expl <- function(df){
    lm(value ~ Year, data = df)
}
model_change_expl2 <- possibly(model_change_expl, otherwise = NA)

# run models
slopes_explanatory <- nested_explanatory %>% 
    mutate(model = map(data, model_change_expl2)) 

# pull out slopes
slopes_expl_long <- slopes_explanatory %>% 
    select(Reserve, response, model) %>% 
    mutate(tidy = map(model, broom::tidy)) %>% 
    unnest(tidy) %>% 
    filter(term == "Year")

slopes_expl_wide <- slopes_expl_long %>% 
    select(Reserve, response, estimate) %>% 
    pivot_wider(names_from = response,
                values_from = estimate)


# join all ----
# veg slopes, explanatory without time, explanatory slopes, veg zone proportions



# BEWARE! insert random normal variable for SET change  ----








# older - before emails after TWG SEM meeting
# 4/8/24 Alice asked for summarized data frames:
# Mean EIR/zone/site; dist. to water mean/zone/site; everything else to site level (this shouldn’t need changing as all other explanatory variable are already at site level as far as I know)
# Same as above, but at site level (as opposed to zone level).

# veg_and_expl <- read.csv(here::here("data", "compiled", "veg_and_expl.csv"))
# 
# df_char <- veg_and_expl %>% 
#     select_if(is.character) %>% 
#     select(Reserve, SiteID, Vegetation_Zone)
# df_num <- veg_and_expl %>% 
#     select_if(is.numeric) %>% 
#     select(-Date)

# # To zone/site ----
# df_byZone <- bind_cols(df_char, df_num) %>% 
#     summarize(.by = c(Reserve, SiteID, Vegetation_Zone,
#                       Year, Month, Day),
#               across(everything(), function(x) mean(x, na.rm = TRUE)))
# 
# # To site only ----
# df_bySite <- bind_cols(df_char, df_num) %>%
#     select(-Vegetation_Zone) %>% 
#     summarize(.by = c(Reserve, SiteID, 
#                       Year, Month, Day),
#               across(everything(), function(x) mean(x, na.rm = TRUE)))
# 
# write.csv(df_byZone, here::here("data", "compiled", "byZone_for_Alice.csv"),
#           row.names = FALSE)
# write.csv(df_bySite, here::here("data", "compiled", "bySite_for_Alice.csv"),
#           row.names = FALSE)
