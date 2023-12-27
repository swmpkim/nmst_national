library(dplyr)
library(tidyr)
library(stringr)
library(readxl)
library(purrr)

path <- here::here("data", "intermediate")
outpath <- here::here("data", "compiled")
file_out_fullSpecies <- here::here(outpath, "ALL_veg-fullSpeciesList.csv")
file_out_grouped <- here::here(outpath, "ALL_veg-grouped.csv")

reserves <- stringr::str_sub(dir(path, pattern = "_veg-grouped.RData"), end = -19)

# read in functions
source(here::here("R", "sourced", "functions_natl.R"))

strt<-Sys.time()

# read all stations in to lists and then bind into data frames with purrr

fullSpecies <- map(reserves, 
                   ~get(load(here::here(path, paste0(., "_veg-fullSpeciesList.RData"))))) %>% 
    list_rbind()

groupedSpecies <- map(reserves, 
                      ~get(load(here::here(path, paste0(., "_veg-grouped.RData"))))) %>% 
    list_rbind()



# put columns in a nicer order?
groupedSpecies <- groupedSpecies %>% 
    select(# identifying cols
        Reserve:Years_sinceStart,
        Latitude, Longitude, Orthometric_Height,
        Distance_to_water,
        # abiotic catebories   
        `Total unvegetated`, Bare, Dead, Rock, Wood,
        Wrack, Overstory, Water, `Other Unvegetated`,
        # live plant categories   
        `Total live veg`, `A-Algae`, `B-Brackish`,
        `F-Freshwater`, `H-Halophyte`, `U-Upland`,
        `Other live vegetation`,
        # metrics and ratios
        EIR, Richness, SWdiv, 
        Invasive_Cover, Unveg_to_veg,
        # specific species
        any_of(c("Spartina alterniflora", "Spartina patens",
        "Juncus roemerianus", "Salicornia pacifica")),
        # catchall
        everything()
    )


# write out the data frames  
write.csv(fullSpecies, file = file_out_fullSpecies, row.names = FALSE, na = "")
write.csv(groupedSpecies, file = file_out_grouped, row.names = FALSE, na = "")

Sys.time() - strt
beepr::beep(8)