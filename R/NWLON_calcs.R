library(here)
library(janitor)
library(readr)
library(dplyr)
library(lubridate)
library(rnoaa)



# from google sheet: https://docs.google.com/spreadsheets/d/12LDWfJvE4Cye56O-Uv_cmGr2a4WLqcKD24OzPVrAEUI/edit#gid=1746545735
stns_orig_firstpass <- c("8665530", "8728690", "8728690", "8577330", "8577330", "8577330", "8638610", "8557380", "8557380", "9413450", "9413450", "9413450", "9413450", "9413450", "9413450", "9413450", "9413450", "9413450", "9413450", "9413450", "8735180", "8735180", "8735180", "8735180", "8735180", "8443970", "8443970", "8443970", "8720218", "8720218", "8720218", "8720218", "8720218", "8720218", "8518962", "8518962", "8534319", "9455556", "9455556", "9455556", "9455556", "8774770", "8774770", "8774770", "8774770", "8774770", "8452660", "8452660", "8662245", "8662245", "8658163", "8656483", "8658163", "9432780", "9432780", "9432780", "9432780", "9432780", "9432780", "9432780", "9432780", "9432780", "9410170", "9410170", "9410170", "9410170", "9410170", "9410170", "9410170", "8418150", "8447930", "8447930", "8447930")
notAvailable <- c("8518962", "8534319", "8662245", "8658163", "9455556")
stns <- unique(stns_orig)
stns <- stns[which(!(stns %in% notAvailable))]

stns_orig <- c("8665530", "8728690", "8728690", "8577330", "8577330", "8577330", "8638610", "8557380", "8557380", "9413450", "9413450", "9413450", "9413450", "9413450", "9413450", "9413450", "9413450", "9413450", "9413450", "9413450", "8735180", "8735180", "8735180", "8735180", "8735180", "8443970", "8443970", "8443970", "8720218", "8720218", "8720218", "8720218", "8720218", "8720218", "8518750", "8518750", "8534720", "9455500", "9455500", "9455500", "9455500", "8774770", "8774770", "8774770", "8774770", "8774770", "8452660", "8452660", "8661070", "8661070", "8656483", "8656483", "8656483", "9432780", "9432780", "9432780", "9432780", "9432780", "9432780", "9432780", "9432780", "9432780", "9410170", "9410170", "9410170", "9410170", "9410170", "9410170", "9410170", "8418150", "8447930", "8447930", "8447930")
stns <- unique(stns_orig)



# set up date parameters
end_date <- 2022
water_longterm_start <- 1900
water_19yr_start <- end_date - 18

# the latest station from NWLON seems to start in 1970
# maybe calculate that rate of change for all so it's comparable?


# download data files ----
# first check to see if current file exists

for(i in seq_along(stns)){
    stn <- stns[i]
    url_path <- paste0("https://tidesandcurrents.noaa.gov/sltrends/data/", stn, "_meantrend.csv")
    
    out_name <- paste0(stn, ".csv")
    out_path <- here::here("data", "NWLON", out_name)
    
    if(file.exists(out_path)){
        print(paste("File from NWLON station", stn, "already exists"))
    } else {
        try(
            download.file(url = url_path, 
                          destfile = out_path))
        if(file.exists(out_path)){
            print(paste0("Downloaded data from NWLON station ", stn))
        } else {
            print(paste0("Data from NWLON station ", stn, " is not available"))
        }
    }
    
    out_name2 <- paste0(stn, "_monthly_means.csv")
    out_path2 <- here::here("data", "NWLON", out_name2)
    
    if(file.exists(out_path2)){
        print(paste("Monthly means file from NWLON station", stn, "already exists"))
    } else {
        
        dat1 <- coops_search(begin_date = 20030101,
                            end_date = 20121231,
                            station_name = stn,
                            product = "monthly_mean",
                            datum = "MLLW")
        
        dat2 <- coops_search(begin_date = 20130101,
                             end_date = 20221231,
                             station_name = stn,
                             product = "monthly_mean",
                             datum = "MLLW")
        
        dat <- bind_rows(dat1$data, dat2$data)
        dat <- dat %>% 
            mutate(station = stn,
                   name = dat1$metadata$name,
                   lat = dat1$metadata$lat,
                   long = dat1$metadata$lon) %>% 
            relocate(station:long)
        
        write.csv(dat, out_path2, row.names = FALSE)
        
        if(file.exists(out_path2)){
            print(paste0("Downloaded monthly means file from NWLON station ", stn))
        } else {
            print(paste0("Monthly means file from NWLON station ", stn, " is not available"))
        }
    }
    
}


# calculate trends from each file ----
wl_trends <- list()
for(i in seq_along(stns)){
    stn <- stns[i]
    
    in_path <- here::here("data", "NWLON", paste0(stn, ".csv"))
    # make sure file exists
    
    
    # set up file
    
    # find the right rows to start on
    dat_head <- readLines(in_path)[1:100]
    start_row <- grep("^Year", dat_head)
    
    
    dat_raw <- read_csv(in_path, skip = start_row-1)
    dat <- dat_raw %>% 
        clean_names() %>%
        filter(year <= end_date) %>%
        select(year, month, monthly_msl) %>%
        mutate(date = ymd(paste0(year, month, "-01")),
               monthly_msl_mm = monthly_msl * 1000)
    
    
    # period of record
    ari_out <- arima(dat$monthly_msl_mm, order = c(1, 0, 0), xreg = dat$date)
    
    trend_lt <- round(ari_out$coef[3] * 365.25, 2)
    se_lt <- round(sqrt(ari_out$var.coef[3, 3]) * 365.25, 4)
    confint_lt <- round(confint(ari_out)[3, ] * 365.25, 2)
    setr_plus_minus <- round(1.96*se_lt, 2)
    
    # since 1970
    dat_sub1970 <- dat %>% 
        filter(year >= 1970)
    
    ari_out1970 <- arima(dat_sub1970$monthly_msl_mm, order = c(1, 0, 0), xreg = dat_sub1970$date)
    
    trend_1970 <- round(ari_out1970$coef[3] * 365.25, 2)
    se_1970 <- round(sqrt(ari_out1970$var.coef[3, 3]) * 365.25, 4)
    confint_1970 <- round(confint(ari_out1970)[3, ] * 365.25, 2)
    setr_plus_minus_1970 <- round(1.96*se_1970, 2)
    
    # 19-yrs
    dat_sub19 <- dat %>% 
        filter(year >= water_19yr_start)
    
    ari_out19 <- arima(dat_sub19$monthly_msl_mm, order = c(1, 0, 0), xreg = dat_sub19$date)
    
    trend_19 <- round(ari_out19$coef[3] * 365.25, 2)
    se_19 <- round(sqrt(ari_out19$var.coef[3, 3]) * 365.25, 4)
    confint_19 <- round(confint(ari_out19)[3, ] * 365.25, 2)
    setr_plus_minus_19 <- round(1.96*se_19, 2)
    
    # output
    wl_trends[[i]] <- data.frame("NWLON Station Number" = stn,
                          "SLR rate (mm/yr)" = trend_lt,
                          "+/- (95% CI)" = setr_plus_minus,
                          "SLR start" = lubridate::year(min(dat$date)),
                          "data end" = end_date,
                          "slr_since1970" = trend_1970,
                          "ci95_since1970" = setr_plus_minus_1970,
                          "slr_19yr" = trend_19,
                          "ci95_19yr" = setr_plus_minus_19,
                          "yr19_start" = water_19yr_start,
                          "yr19_end" = end_date,
                          check.names = FALSE,
                          row.names = NULL)
}



# tie all together ----
wl_trends_df <- bind_rows(wl_trends)

# write out full csv
write.csv(wl_trends_df,
          here::here("data", "NWLON", "00_allStations.csv"),
          row.names = FALSE,
          na = "")

# tie to order of NWLON stations in google sheet and write out for copying
for_spreadsheet <- data.frame("station" = stns_orig) %>% 
    left_join(wl_trends_df, by = c("station" = "NWLON Station Number")) %>% 
    select(station,
           "SLR rate (mm/yr)",
           "SLR start",
           slr_since1970,
           slr_19yr)
write.csv(for_spreadsheet,
          here::here("data", "NWLON", "00_allStations_forSpreadsheet.csv"),
          row.names = FALSE,
          na = "")

