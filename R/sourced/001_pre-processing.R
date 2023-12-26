# most of this is copied from reserve level 01_Veg_analyses.Rmd


# read data, metadata, and analysis specifications
dat <- get_data(file_dat, cover_only = TRUE) %>%  # gets rid of density and height columns; keeps F_ columns
    select(Reserve, SiteID, TransectID, PlotID, Year, Month, Day,
           Total:ncol(.))    
stn_tbl <- get_stn_table(file_dat)
stn_tbl <- stn_tbl %>% 
    mutate(PlotID_full = paste(SiteID, TransectID, PlotID, sep = "-"))
species_info <- get_species_info(file_dat)
eis <- get_ecotone_invaders(file = file_specs)

# remove vegetation zones from eis data frame that don't appear in the station table
eis <- eis[eis$Vegetation_Zone %in% unique(stn_tbl$Vegetation_Zone), ]

# add some columns to dat
dat <- dat %>% 
    dplyr::mutate(Date = lubridate::decimal_date(lubridate::ymd(paste(Year, Month, Day, sep = "-"))),
                  Years_sinceStart = round(Date - min(Date), 4),
                  StTrns = paste(SiteID, TransectID, sep = "-"),
                  StTrnsPlt = paste(SiteID, TransectID, PlotID, sep = "-")) %>% 
    dplyr::relocate(c(Date, Years_sinceStart), .before = Year) %>% 
    dplyr::relocate(c(StTrns, StTrnsPlt), .after = PlotID)

dat <- remove_suspect_values(dat, flags = c("-3"))  # remove suspect values. also removes F_columns.
dat <- remove_unsampleds(dat)  # should get rid of any dates where a plot was not sampled or was entirely rejected. 
dat <- na_to_0(dat)  # turn remaining NAs into 0s - this first makes sure all species columns are numeric
dat <- join_zones(dat, stn_tbl)  # add vegetation zones to each row
dat_long <- dat %>%
    pivot_longer(-(Reserve:Total),
                 names_to = "Species",
                 values_to = "Cover") %>% 
    left_join(species_info, by = "Species") %>% 
    mutate(Invasive = case_when(Native_Classification %in% c("Native invasive", "Non-native invasive") ~ "Invasive",
                                .default = "not invasive"),
           Salt_or_Fresh = case_when(Plant_Categories %in% c("B-Brackish", "H-Halophyte", "A-Algae") ~ "Salt",
                                     Plant_Categories %in% c("F-Freshwater", "U-Upland") ~ "Fresh",
                                     .default = "Neither"),
           grouping_category = case_when(!is.na(NMST_Groupings) ~ NMST_Groupings,
                                         !is.na(Plant_Categories) ~ Plant_Categories,
                                         .default = Cover_Categories))


# make the grouped data frame
dat_grouped <- dat_long %>% 
    summarize(.by = c(Reserve, SiteID, TransectID, PlotID, Year, Month, Day, grouping_category),
              Cover = sum(Cover, na.rm = TRUE)) %>% 
    mutate(grouping_category = case_when(grouping_category == "Live vegetation" ~ "Other live vegetation",
                                         .default = grouping_category)) %>% 
    pivot_wider(names_from = grouping_category,
                values_from = Cover)


# calculate metrics and add to both data frames
spps_live <- species_info$Species[species_info$Cover_Categories == "Live vegetation"]
spps_live <- spps_live[which(spps_live %in% names(dat))]
dat_live <- dat %>% 
    select(all_of(spps_live))
swdiv <- diversity(dat_live, index = "shannon")
rich <- specnumber(dat_live)
dat_div <- dat %>% 
    select(Reserve, SiteID, TransectID, PlotID, Year, Month, Day) %>% 
    mutate(SWdiv = swdiv,
           Richness = rich)
dat <- left_join(dat, dat_div)
dat_grouped <- left_join(dat_grouped, dat_div)
rm(dat_live, swdiv, rich, dat_div)

# attach EIR to data frame
EIR <- dat_long %>% 
    left_join(eis) %>% 
    mutate(Invader = case_when(is.na(Invader) ~ 0,
                               .default = 1),
           EI_Cover = Cover * Invader) %>% 
    group_by(Year, Month, Day, Reserve, SiteID, TransectID, PlotID) %>% 
    summarize(EIR = round(sum(EI_Cover) / sum(Cover), 5))
dat <- left_join(dat, EIR)
dat_grouped <- left_join(dat_grouped, EIR)
rm(EIR)

Invasives <- dat_long %>% 
    summarize(.by = c(Year, Month, Day, Reserve, SiteID, TransectID, PlotID, Invasive),
              Invasive_Cover = sum(Cover, na.rm = TRUE)) %>% 
    filter(Invasive == "Invasive") %>% 
    select(Year, Month, Day, Reserve, SiteID, TransectID, PlotID, Invasive_Cover)
dat <- left_join(dat, Invasives)
dat_grouped <- left_join(dat_grouped, Invasives)
rm(Invasives)


# Salt:Fresh commented out because
# some reserves have 0s in Fresh, causing 'Inf'
# and some don't have identified freshwater species at all
# so this errors

# Salt_or_Fresh <- dat_long %>% 
#     summarize(.by = c(Year, Month, Day, Reserve, SiteID, TransectID, PlotID, Salt_or_Fresh),
#               Cover = sum(Cover, na.rm = TRUE)) %>% 
#     pivot_wider(names_from = Salt_or_Fresh,
#                 values_from = Cover) %>% 
#     mutate(Salt_to_Fresh = Salt / Fresh) %>% 
#     select(Year, Month, Day, Reserve, SiteID, TransectID, PlotID, Salt_to_Fresh)
# dat <- left_join(dat, Salt_or_Fresh)
# dat_grouped <- left_join(dat_grouped, Salt_or_Fresh)
# rm(Salt_or_Fresh)

Unveg_to_veg <- dat_long %>% 
    summarize(.by = c(Year, Month, Day, Reserve, SiteID, TransectID, PlotID, Cover_Categories),
              Cover = sum(Cover, na.rm = TRUE)) %>% 
    pivot_wider(names_from = Cover_Categories,
                values_from = Cover) %>% 
    mutate(Unveg_to_veg = `Unvegetated category` / `Live vegetation`) %>% 
    select(Year, Month, Day, Reserve, SiteID, TransectID, PlotID, Unveg_to_veg)
dat <- left_join(dat, Unveg_to_veg)
dat_grouped <- left_join(dat_grouped, Unveg_to_veg)
rm(Unveg_to_veg)


total_live_abiotic <- dat_long %>% 
    summarize(.by = c(Year, Month, Day, Reserve, SiteID, TransectID, PlotID, Cover_Categories),
              Cover = sum(Cover, na.rm = TRUE)) %>% 
    mutate(Cover_Categories = case_when(Cover_Categories == "Unvegetated category" ~ "Total unvegetated",
                                        Cover_Categories == "Live vegetation" ~ "Total live veg",
                                        .default = Cover_Categories)) %>% 
    pivot_wider(names_from = Cover_Categories,
                values_from = Cover) %>% 
    select(Year, Month, Day, Reserve, SiteID, TransectID, PlotID,
           `Total unvegetated`,
           `Total live veg`)
dat <- left_join(dat, total_live_abiotic)
dat_grouped <- left_join(dat_grouped, total_live_abiotic)
rm(total_live_abiotic)
