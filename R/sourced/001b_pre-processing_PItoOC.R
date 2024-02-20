library(googlesheets4)
library(googledrive)
library(tidyverse)


source(here::here("R", "sourced", "functions_natl.R"))

# PI reserves ----  

expl_sheet <- "https://docs.google.com/spreadsheets/d/12LDWfJvE4Cye56O-Uv_cmGr2a4WLqcKD24OzPVrAEUI/edit?usp=sharing"


# just one row per reserve/site combination - will be repeated for every plot and date
expl_dat <- read_sheet(expl_sheet, 
                                sheet = "Time removed",
                                skip = 5) %>% # unless someone adds another row
    filter(`Reserve File` != "EXAMPLE")  %>% 
    janitor::remove_empty("cols") %>% 
    select(Reserve = `Reserve File`,
           SiteID = `Site ID (reserve files)`,
           everything()) %>% 
    select(1:6) %>% 
    janitor::clean_names()

pi_conv <- expl_dat %>% 
    filter(pi_or_oc == "PI") %>% 
    select(reserve, pi_points_per_plot) %>% 
    distinct() %>% 
    mutate(pi_mult_factor = round(100/pi_points_per_plot, 3))

# multiplying factor: 100/pi_points_per_plot - want to convert each 
# plot to be as if it had 100 points

# then will apply regression correction factor based on morphological archetype
# and regression factors from New England


# Regression factors ----

morph_conv <- tibble::tribble(
                          ~Morphology, ~`Correction.Factor.(CF)`,
                          "Bare/Dead",                    1.2101,
                          "Broad 'Grasses'",              0.6555,
                           "Climbers",                    0.6023,
                              "Forbs",                    0.3853,
                       "Ground/Algae",                     0.604,
                     "Shrubs & Trees",                    0.5222,
                     "Thin' Grasses'",                    0.4573,
                              "Other",                    0.5378
                  )
names(morph_conv) <- c("Morphology", "Correction_Factor")


# Morphological archetypes ----

# download the latest version of the excel file in google drive
# this is different from reading in the explanatory matrix because
# the former was a google sheet; this is an actual excel file, just stored in drive
morph_sheet <- "https://docs.google.com/spreadsheets/d/14GS4vqd1B62mgTI8_Yj8qde9RIDUPCG6/edit?usp=sharing&ouid=100678166508144783920&rtpof=true&sd=true"
drive_download(morph_sheet, path = here::here("data",
                                              "morph_file.xlsx"),
               overwrite = TRUE)
# and read it in
morph_dat <- readxl::read_xlsx(here::here("data",
                                          "morph_file.xlsx"),
                               sheet = "Unique List - species") 


# test on one reserve ----
# has to happen at reserve level (rather than national data frame) 
# because next the species get condensed to group
# which is in the reserve spec files


reserve <- "ACE"

filename <- paste0(reserve, "_veg.xlsx")
file_dat <- here::here("data", "reserve_level", filename)

mult_fct <- pi_conv$pi_mult_factor[which(pi_conv$reserve == reserve)]

# read data
dat_full <- get_data(file_dat, cover_only = TRUE) %>%  # gets rid of density and height columns; keeps F_ columns
    mutate(uniqueID = paste0(SiteID, TransectID, PlotID, Year, Month, Day)) %>% 
    relocate(uniqueID)

dat_to_convert <- dat_full %>% 
    select(Total:ncol(.),
           -any_of(starts_with("F_")),
           -Total)    

# remove 'Other layer' species
species_info <- get_species_info(file_dat)
otherLayers <- unlist(species_info[species_info$Cover_Categories == "Other layer", "Species"])
dat_to_convert <- dat_to_convert %>% 
    select(-any_of(otherLayers))
rm(otherLayers)

# convert so it's like there were 100 points
dat_to100pts <- dat_to_convert %>% 
    mutate(across(everything(), function(x) x*mult_fct))

# apply regression/morphological correction factor
# need to connect species to archetypes first
morph_toJoin <- morph_dat %>% 
    select(species = "Species/Cover",
           Morphology = "Morphological archetype") %>% 
    left_join(morph_conv) %>% 
    distinct()

dat_toMorph <- dat_to100pts %>% 
    mutate(uniqueID = dat_full$uniqueID) %>% 
    pivot_longer(-uniqueID,
                names_to = "species",
                values_to = "cover") %>% 
    left_join(morph_toJoin) %>% 
    mutate(Correction_Factor = case_when(is.na(Correction_Factor) ~ 1,
                                         .default = Correction_Factor),
           corrected_cover = cover * Correction_Factor)

dat_morphed <- dat_toMorph %>% 
    select(uniqueID, species, corrected_cover) %>% 
    pivot_wider(names_from = species,
                values_from = corrected_cover) %>% 
    column_to_rownames("uniqueID")


# normalize to 100
dat_morphed$rowTotal <- rowSums(dat_morphed, na.rm = TRUE)
to_process <- names(dat_morphed)[-which(names(dat_morphed) == "rowTotal")]
dat_forcedTo100 <- dat_morphed %>% 
    mutate(across(all_of(to_process),
                  .fns = ~round(./rowTotal * 100, 2))) %>% 
    select(-rowTotal) %>% 
    rownames_to_column("uniqueID")

# verify
hist(rowSums(dat_forcedTo100[2:ncol(dat_forcedTo100)], na.rm = TRUE), breaks = 20)

# reconnect it to the full data
dat_meta <- dat_full %>% 
    select(uniqueID:Total,
           -Unique_ID,
           -Total)
dat_out <- left_join(dat_meta, dat_forcedTo100)
# write that out for later qa/qc checking
fileout <- paste0(reserve, "_OCfromPI.csv")
file_dat <- here::here("data", "PItoOC", fileout)
write.csv(dat_out, file_dat, 
          row.names = FALSE,
          na = "")
