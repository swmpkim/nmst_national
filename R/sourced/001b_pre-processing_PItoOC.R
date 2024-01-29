library(googlesheets4)
library(tidyverse)


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
                    "Broad 'Grasses'",                    0.6555,
                           "Climbers",                    0.6023,
                              "Forbs",                    0.3853,
                       "Ground/Algae",                     0.604,
                     "Shrubs & Trees",                    0.5222,
                     "Thin' Grasses'",                    0.4573,
                              "Other",                    0.5378
                  )
names(morph_conv) <- c("Morphology", "Correction_Factor")


# Morphological archetypes ----

morph_dat <- readxl::read_xlsx(here::here("data", 
                                          "National species list and archetypes.xlsx"),
                               sheet = "Unique Species List")

# test on one reserve ----

file_dat <- here::here("data", "reserve_level", "NAR_veg.xlsx")

source(here::here("R", "sourced", "functions_natl.R"))

# read data
dat <- get_data(file_dat, cover_only = TRUE) %>%  # gets rid of density and height columns; keeps F_ columns
    select(Reserve, SiteID, TransectID, PlotID, Year, Month, Day,
           Total:ncol(.),
           -any_of(starts_with("F_")),
           -Total)    


