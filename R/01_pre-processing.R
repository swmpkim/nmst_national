library(foreach)
library(doParallel)


reserves <- stringr::str_sub(dir(path, pattern = "_veg.xlsx$"), end = -10)


# setup parallel backend
ncores <- 8
cl<-makeCluster(ncores)  
registerDoParallel(cl)

strt<-Sys.time()

# process all stations
# wq ----
foreach(res = reserves, .packages = c('dplyr', 'stringr', 'tidyr', 'readxl', 'vegan')) %dopar% {
    # path setup
    path <- here::here("data", "reserve_level")
    outpath <- here::here("data", "intermediate")
    file_dat <- here::here(path, paste0(res, "_veg.xlsx"))
    file_specs <- here::here(path, paste0(res, "_veg-specs.xlsx"))
    file_out_fullSpecies <- here::here(outpath, paste0(res, "_veg-fullSpeciesList.RData"))
    file_out_grouped <- here::here(outpath, paste0(res, "_veg-grouped.RData"))
    
    # read in functions
    source(here::here("R", "sourced", "functions_natl.R"))

    # process the data
    source(here::here("R", "sourced", "001_pre-processing.R"))
    
    
    # write out the data
    assign(dat_full, dat)
    save(list = dat_full, file = file_out_fullSpecies)

assign(dat_grouped, dat)
save(list = dat_grouped, file = file_out_grouped)

# remove all the data frames and stuff
    
}

Sys.time() - strt
stopCluster(cl)
beepr::beep(8)