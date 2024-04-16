library(tidyverse)

load(here::here("data", "compiled", "veg_and_expl_dfs.RData"))



# need to calculate slopes by plot
# then average up to site and/or zone
# add metric of proportion of site low vs. mid/high

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



# need slopes (?) for explanatory factors by site as well










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
