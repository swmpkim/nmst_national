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
# and other things that should be averaged by site
site_metrics <- veg %>% 
    select(Reserve, SiteID, Vegetation_Zone, TransectID, PlotID,
           Orthometric_Height, Distance_to_Water) %>% 
    distinct() %>% 
    mutate(zone_coarse = case_match(Vegetation_Zone,
                                    "M-Mudflat" ~ "Low",
                                    "S-Seaward Edge" ~ "Low",
                                    "L-Low Marsh" ~ "Low",
                                    "P-Pools/Pannes" ~ "Low",
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
              siteAvg_distance_to_water = mean(Distance_to_Water, na.rm = TRUE),
              siteAvg_orthometric_height = mean(Orthometric_Height, na.rm = TRUE))

zone_metrics <- veg %>% 
    select(Reserve, SiteID, Vegetation_Zone, TransectID, PlotID,
           Orthometric_Height, Distance_to_Water) %>% 
    distinct() %>% 
    mutate(zone_coarse = case_match(Vegetation_Zone,
                                    "M-Mudflat" ~ "Low",
                                    "S-Seaward Edge" ~ "Low",
                                    "L-Low Marsh" ~ "Low",
                                    "P-Pools/Pannes" ~ "Low",
                                    "T-Transition" ~ "Mid",
                                    "H-High Marsh" ~ "Mid",
                                    "UE-Upland Edge" ~ "Up",
                                    "FT-Freshwater Tidal" ~ "Up",
                                    "U-Upland" ~ "Up",
                                    .default = "Other")) %>% 
    summarize(.by = c(Reserve, SiteID, zone_coarse),
              zoneAvg_distance_to_water = mean(Distance_to_Water, na.rm = TRUE),
              zoneAvg_orthometric_height = mean(Orthometric_Height, na.rm = TRUE))


plot_metrics <- veg %>% 
    select(Reserve, SiteID, TransectID, PlotID,
           plotOrthometric_height = Orthometric_Height, 
           plotDistance_to_water = Distance_to_Water) %>% 
    distinct()

# make sure all add up to 1
unique(rowSums(site_metrics[3:5]))

# veg slopes ----  
# pivot longer then nest
veg_long_nested <- veg %>% 
    mutate(ResStTrnsPlt = paste(Reserve, SiteID, TransectID, PlotID, sep = "_"),
           date = lubridate::decimal_date(lubridate::ymd(paste(Year, Month, Day)))) %>% 
    select(ResStTrnsPlt,
           date,
           Total.unvegetated:Salt_to_Total) %>%
    pivot_longer(Total.unvegetated:Salt_to_Total,
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

slopes_by_plot <- slopes_long %>% 
    select(ResStTrnsPlt, response, estimate) %>% 
    separate(ResStTrnsPlt, 
             into = c("Reserve", "SiteID", "TransectID", "PlotID"),
             sep = "_") %>% 
    pivot_wider(names_from = response,
                values_from = estimate)

slopes_by_site <- slopes_by_plot %>% 
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

# by zone ----
# retrieve zone by plot from veg df
zones_by_plot <- veg %>% 
    select(Reserve, SiteID, TransectID, PlotID, Vegetation_Zone) %>% 
    distinct() %>% 
    mutate(zone_coarse = case_match(Vegetation_Zone,
                                    "M-Mudflat" ~ "Low",
                                    "S-Seaward Edge" ~ "Low",
                                    "L-Low Marsh" ~ "Low",
                                    "P-Pools/Pannes" ~ "Low",
                                    "T-Transition" ~ "Mid",
                                    "H-High Marsh" ~ "Mid",
                                    "UE-Upland Edge" ~ "Up",
                                    "FT-Freshwater Tidal" ~ "Up",
                                    "U-Upland" ~ "Up",
                                    .default = "Other"))
# check
janitor::get_dupes(zones_by_plot, Reserve, SiteID, TransectID, PlotID)

# calculate
# have to keep site to join SET rates etc.
slopes_by_zone <- slopes_by_plot %>% 
    left_join(zones_by_plot) %>% 
    relocate(c(Vegetation_Zone, zone_coarse)) %>% 
    select(-TransectID, -PlotID, -Vegetation_Zone) %>% 
    rename(Vegetation_Zone = zone_coarse) %>% 
    summarize(.by = c(Reserve, SiteID, Vegetation_Zone),
              across(everything(),
                     function(x) mean(x, na.rm = TRUE)))
    


# join all ----
# prep dfs ----
names(time_component_no) <- stringr::str_replace_all(names(time_component_no),
                                                     " ",
                                                     "_")
expl_noTime_toJoin <- time_component_no %>% 
    select(Reserve, SiteID,
           Geomorphology,
           Tidal_Range,
           Salinity_category,
           SLR_since_1970 = SLR_rate_since_1970,
           SLR_last19yrs = `Local_linear_water_level_change_rate_-_19-yr_rate`,
           Latitude:NERRs_Landscape_Pct_MUC_below_MHHW,
           Criteria_for_site_not_met,
           SET_change) %>% 
    mutate(SET_deficit = SLR_since_1970 - SET_change)

# put 'slope' in column names ----
names(slopes_by_plot)[5:ncol(slopes_by_plot)] <- paste0(names(slopes_by_plot)[5:ncol(slopes_by_plot)],
                                                        "_slope")
names(slopes_by_site)[3:ncol(slopes_by_site)] <- paste0(names(slopes_by_site)[3:ncol(slopes_by_site)],
                                                        "_slope")
names(slopes_by_zone)[4:ncol(slopes_by_zone)] <- paste0(names(slopes_by_zone)[4:ncol(slopes_by_zone)],
                                                        "_slope")
names(slopes_expl_wide)[2:4] <- paste0(names(slopes_expl_wide)[2:4], 
                                       "_slope")


#  to slopes by site ----
# veg slopes, explanatory without time, explanatory slopes, veg zone proportions
slopesAndExpl_bySite <- left_join(slopes_by_site,
                                  slopes_expl_wide,
                                  by = "Reserve") %>% 
    left_join(expl_noTime_toJoin,
              by = c("Reserve", "SiteID")) %>% 
    left_join(site_metrics,
              by = c("Reserve", "SiteID"))

# to slopes by zone ----
# keeping zone metrics in case it matters, that a Site-Zone is in a site 
# with mostly the same zone vs. mostly other zones
slopesAndExpl_byZone <- left_join(slopes_by_zone,
                                  slopes_expl_wide,
                                  by = "Reserve") %>% 
    left_join(expl_noTime_toJoin,
              by = c("Reserve", "SiteID")) %>% 
    left_join(site_metrics,
              by = c("Reserve", "SiteID")) %>% 
    left_join(zone_metrics,
              by = c("Reserve", "SiteID", 
                     "Vegetation_Zone" = "zone_coarse"))


# to slopes by plot ----
slopesAndExpl_byPlot <- left_join(slopes_by_plot,
                                  slopes_expl_wide,
                                  by = "Reserve") %>% 
    left_join(expl_noTime_toJoin,
              by = c("Reserve", "SiteID")) %>% 
    left_join(site_metrics,
              by = c("Reserve", "SiteID")) %>% 
    left_join(plot_metrics,
              by = c("Reserve", "SiteID", "TransectID", "PlotID"))

# save ----
save(slopesAndExpl_byPlot,
     slopesAndExpl_bySite,
     slopesAndExpl_byZone,
     file = here::here("data", "compiled", "slopesAndExpl_dfs.RData"))
write.csv(slopesAndExpl_byPlot,
          here::here("data", "compiled", "slopesAndExpl_byPlot.csv"),
          na = "",
          row.names = FALSE)
write.csv(slopesAndExpl_bySite,
          here::here("data", "compiled", "slopesAndExpl_bySite.csv"),
          na = "",
          row.names = FALSE)
write.csv(slopesAndExpl_byZone,
          here::here("data", "compiled", "slopesAndExpl_byZone.csv"),
          na = "",
          row.names = FALSE)







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
