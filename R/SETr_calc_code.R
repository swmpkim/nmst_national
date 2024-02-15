# 1. Read and pivot a reserve's data ----
# from https://github.com/swmpkim/SETr_example_reserve_pkg/blob/master/R_scripts/01_process_raw_data.R
# this actually writes out a csv file but I don't think you have to
# so I commented out those lines (84-88)
# you could just pivot and keep going to the rate calculations



library(dplyr)
library(tidyr)
library(purrr)
library(readxl)
library(stringr)
library(lubridate)
library(here)
library(nlme)


file_path <- "SOMETHING HERE"


# read all sheets, append to each other
# force all columns to be seen as text so 
# there aren't issues with joining
dat <- file_path %>% 
    excel_sheets() %>% 
    set_names() %>% 
    map_df( ~ read_excel(path = file_path, 
                         sheet = .x, 
                         col_types = "text"), 
            .id = "sheet")

# check to make sure the SET ID that was entered matches the name of each sheet
mismatches <- dat$set_id != dat$sheet

# make this whole script stop if something doesn't match
if(sum(mismatches) > 0){
    print(dat[mismatches, c("sheet", "set_id", "year", "month", "arm_position")])
    stop("There are SET IDs that do not match the sheet name. Please check and correct the rows printed above before proceeding.")
}else{
    # if no problem, do everything else
    
    # first, format the data:
    # get rid of the "sheet" column
    # make sure date is date format; 
    # several columns should be character: set_id, arm_position, 
    # and anything that ends in qaqc_code
    dat_formatted <- dat %>% 
        select(-sheet) %>% 
        mutate_at(c("set_id", "arm_position"), as.character) %>% 
        mutate_at(vars(ends_with("qaqc_code")), as.character)
    
    
    # have to change column names first because there are too many underscores
    names(dat_formatted) <- gsub("qaqc_code", "qaqccode", names(dat_formatted))
    names(dat_formatted) <- gsub("pin_", "pin", names(dat_formatted))
    names(dat_formatted) <- gsub("height_", "height", names(dat_formatted))
    
    # set up to pivot
    spec <- dat_formatted %>% 
        build_longer_spec(
            cols = starts_with("pin1_height"):"pin9_qaqccode",
            names_to = c("pinnumber", ".value"),
            names_sep = "_"
        )
    
    # pivot to longer
    dat_long <- dat_formatted %>% 
        pivot_longer_spec(spec = spec)
    
    # put underscores back in the names
    names(dat_long) <- gsub("pin", "pin_", names(dat_long))
    dat_long$pin_number <- gsub("pin", "pin_", dat_long$pin_number)
    names(dat_long) <- gsub("qaqccode", "qaqc_code", names(dat_long))
    names(dat_long) <- gsub("height", "height_", names(dat_long))
    
    # format and arrange before output
    dat_long <- dat_long %>% 
        select(set_id, year, month, day, arm_position, arm_qaqc_code, 
               pin_number, starts_with("height"), qaqc_code, everything()) %>% 
        arrange(set_id, year, month, day, arm_position, pin_number)
    
    # generate output and write out file
    # file_in <- str_extract(file_path, "[:alpha:]+\\.xls")
    # file_out <- paste0(substr(file_in, 1, nchar(file_in)-4), "_processed.csv")
    # out_path <- here::here("data", "processed")
    # file_out <- paste0(out_path, "/", file_out)
    # write.csv(dat_long, file_out, row.names = FALSE)
    
    message("\n \nDone! Move on to other scripts. \n \n")
}


# 2. Rate calculations   ----
# pulled from https://github.com/swmpkim/SETr_example_reserve_pkg/blob/master/R_scripts/sourced/002_rate_calculations.Rmd
# I'm most worried about this part, because the do() function has been
# deprecated - if it works, great, but if not we'll have to re-think
# the more modern way is to make a nested data frame and use purrr

# assuming the data frame is in the long format from above
# and that pin heights are in mm (some reserves use other units)


# get rid of any placeholders; make sure set_id is a factor;
# make sure reserve, station, arm_position, and pin_number are character vectors
# paste year, month, and day together into "date"
dat <- dat %>%
    mutate(date = lubridate::ymd(paste(year, month, day))) %>% 
    filter(!is.na(date),
           !is.na(pin_number)) %>%
    mutate_at(c("reserve", "set_id",   
                "arm_position", "arm_qaqc_code", 
                "pin_number", "qaqc_code"),
              as.character)

# pull out dates in the dataset
start_whole <- min(dat$date)
end_whole <- max(dat$date)


# figure out which SETs have enough data to perform calculations
to_keep <- unlist(sample_info[sample_info$years_sampled >= 4.5 & sample_info$sample_events >= 5, "set_id"])

not_enough_df <- dat[!(dat$set_id %in% to_keep), ]  # stns with not enough data
dat <- dat[dat$set_id %in% to_keep, ] # stns with enough data

enough_dat <- nrow(dat) > 0 # this will be used to determine the running of future code chunks

partial_dat <- nrow(dat) > 0 & nrow(not_enough_df) > 0  # this will join data frames back together when some sites have enough data and others don't


# calculate
models2 <- dat %>%
    group_by(reserve, set_id) %>%
    do(mod = lme(pin_height ~ date, data = ., random = ~1|arm_position/pin_number, na.action = na.omit))

lmm_out <- models2 %>% 
    mutate(rate = intervals(mod, which = "fixed")$fixed["date", 2] * 365.25,
           CI_low = intervals(mod, which = "fixed")$fixed["date", 1] * 365.25,
           CI_high = intervals(mod, which = "fixed")$fixed["date", 3] * 365.25) %>% 
    select(-mod)

# the rates we need should be in that lmm_out data frame
# the code uses the nlme package

# should probably save results to a csv file

