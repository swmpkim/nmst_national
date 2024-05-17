# troubleshooting

# run 02_combining up through line 41
groupedSpecies %>% 
    filter(!is.na(`NA`)) %>% 
    select(Reserve) %>% 
    distinct()

# CBM, CBV, GTM, KAC, WEL, WQB



# restart R and see what's causing problems at each of these reserves
# by running through 'dat_long' of 001_pre-processing
# and finding which species are associated with NA in grouping_category

library(dplyr)
library(tidyr)
library(stringr)
library(readxl)
library(tibble)
library(vegan)
# library(googlesheets4)
# library(googledrive)


path <- here::here("data", "reserve_level")
outpath <- here::here("data", "intermediate")
reserves <- stringr::str_sub(dir(path, pattern = "_veg.xlsx$"), end = -10)

# read in functions
source(here::here("R", "sourced", "functions_natl.R"))

# get set up for PI-vs-OC processing paths
source(here::here("R", "sourced", "001b_setup-for-PI-or-OC.R"))


res <- "CBM-OPC"
res <- "CBV"
res <- "GTM"
res <- "KAC"
res <- "WEL"
res <- "WQB"

file_dat <- here::here(path, paste0(res, "_veg.xlsx"))
file_specs <- here::here(path, paste0(res, "_veg-specs.xlsx"))


# most of this is copied from reserve level 01_Veg_analyses.Rmd

# check and see if reserve needs to go down PI pathway or not
pi_reserve <- res %in% pi_conv$reserve

# and send it down that pathway.... or not
if(pi_reserve){
    source(here::here("R", "sourced", "001c_pre-processing_PItoOC.R"))
    dat <- dat_out %>% 
        select(-uniqueID)
    # species_info sheet is read in already from 001c
} else {
    dat <- get_data(file_dat, cover_only = TRUE) %>%  # gets rid of density and height columns; keeps F_ columns
        select(Reserve, SiteID, TransectID, PlotID, Year, Month, Day,
               Total:ncol(.)) 
    species_info <- get_species_info(file_dat)
    otherLayers <- unlist(species_info[species_info$Cover_Categories == "Other layer", "Species"])
    dat <- dat %>% 
        select(-any_of(otherLayers))
    rm(otherLayers)
    
    # force to 100
    dat_toForce <- dat %>% 
        select(any_of(species_info$Species))
    dat_toForce$rowTotal <- rowSums(dat_toForce, na.rm = TRUE)
    to_process <- names(dat_toForce)[-which(names(dat_toForce) == "rowTotal")]
    dat_forcedTo100 <- dat_toForce %>% 
        mutate(across(all_of(to_process),
                      .fns = ~round(./rowTotal * 100, 2))) %>% 
        select(-rowTotal)
    dat_forcedTo100$Total <- rowSums(dat_forcedTo100, na.rm = TRUE)
    
    # verify
    hist(dat_forcedTo100$Total, breaks = 20)
    
    # replace columns in dat with the normalized ones
    common_columns <- intersect(names(dat), names(dat_forcedTo100))
    dat[common_columns] <- dat_forcedTo100[common_columns]
    
    # clean up
    rm(dat_toForce, dat_forcedTo100, to_process, common_columns)
}

# now do the rest of the pre-processing

# if there is an F_Record column, get rid of it
if("F_Record" %in% names(dat)) dat$F_Record <- NULL

stn_tbl <- get_stn_table(file_dat)
stn_tbl <- stn_tbl %>%
    mutate(PlotID_full = paste(SiteID, TransectID, PlotID, sep = "-")) %>% 
    select(-Type) %>%    # at least one file has multiple rows per plot based on different types
    distinct()
eis <- get_ecotone_invaders(file = file_specs)

# remove vegetation zones from eis data frame that don't appear in the station table
eis <- eis[eis$Vegetation_Zone %in% unique(stn_tbl$Vegetation_Zone), ]

# add some columns to dat
# and get rid of troublesome columns
dat <- dat %>% 
    select(-any_of(starts_with("Distance")), 
           -any_of(c("Vegetation_Zone", 
                     "Orthometric_Height"))) %>% 
    dplyr::mutate(Date = lubridate::decimal_date(lubridate::ymd(paste(Year, Month, Day, sep = "-"))),
                  Years_sinceStart = round(Date - min(Date), 4),
                  StTrns = paste(SiteID, TransectID, sep = "-"),
                  StTrnsPlt = paste(SiteID, TransectID, PlotID, sep = "-"),
                  Unique_ID = paste0(Reserve, SiteID, TransectID, PlotID,
                                     Year, Month, Day)) %>% 
    dplyr::relocate(c(Date, Years_sinceStart), .before = Year) %>% 
    dplyr::relocate(c(StTrns, StTrnsPlt, Unique_ID), .after = PlotID)


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
           Salt_or_Fresh = factor(Salt_or_Fresh, levels = c("Salt", "Fresh", "Neither")),
           grouping_category = case_when(!is.na(NMST_Groupings) ~ NMST_Groupings,
                                         !is.na(Plant_Categories) ~ Plant_Categories,
                                         .default = Cover_Categories))


dat_long %>% filter(is.na(grouping_category)) %>% 
    select(Species) %>% 
    distinct()