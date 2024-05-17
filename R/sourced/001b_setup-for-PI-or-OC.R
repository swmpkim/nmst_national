# PI reserves ----  

expl_sheet <- here::here("data", "Explanatory Matrix.xlsx")


# just one row per reserve/site combination - will be repeated for every plot and date
expl_dat <- read_xlsx(expl_sheet, 
                       sheet = "Time removed",
                       skip = 5) %>% # unless someone adds another row
    filter(`Reserve File` != "EXAMPLE")  %>% 
    janitor::remove_empty("cols") %>% 
    select(Reserve = `Reserve File`,
           SiteID = `Site ID (reserve files)`,
           everything()) %>% 
    select(1:8) %>% 
    janitor::clean_names()

pi_conv <- expl_dat %>% 
    filter(pi_or_oc == "PI",
           need_to_convert_pi_to_oc == "Y") %>% 
    select(reserve, 
           pi_points_per_plot = pi_points_reported_in_the_data) %>% 
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
    "Climbers",                     0.6023,
    "Forbs",                        0.3853,
    "Ground/Algae",                 0.604,
    "Shrubs & Trees",               0.5222,
    "Thin' Grasses'",               0.4573,
    "Other",                        0.5378
)
names(morph_conv) <- c("Morphology", "Correction_Factor")


# Morphological archetypes ----

# download the latest version of the excel file in google drive
# this is different from reading in the explanatory matrix because
# the former was a google sheet; this is an actual excel file, just stored in drive

# as of 3/4/24 this is now a csv
# morph_sheet <- "https://docs.google.com/spreadsheets/d/14GS4vqd1B62mgTI8_Yj8qde9RIDUPCG6/edit?usp=sharing&ouid=100678166508144783920&rtpof=true&sd=true"
# morph_sheet <- "https://drive.google.com/file/d/1cb6_G_oFTnIS4fkMtfmv1KDr-zQ9DzFz/view?usp=drive_link"
# drive_download(morph_sheet, path = here::here("data",
#                                               "morph_file.csv"),
#                overwrite = TRUE)

# using previous download as of 5/17/24
# and read it in
morph_dat <- readr::read_csv(here::here("data",
                                          "morph_file.csv")) 

rm(expl_sheet, expl_dat)
