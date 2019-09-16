library(pacman)
p_load(raster, ncdf4, rgdal, stringr, gsubfn, lubridate)

## Reading files and stacking days
late_files <- list.files('data/late_run',pattern='*.nc',full.names=TRUE)
late_stack <- raster::stack(late_files)

final_files <- list.files('data/final_run',pattern='*.nc',full.names=TRUE)
final_stack <- raster::stack(final_files)

# Geting dimensions from any file
e_nc <- nc_open(late_files[1])
lat <- ncvar_get(e_nc,'lat')
lon <- ncvar_get(e_nc,'lon')

# Setting the base date from the database
days <- ncvar_get(e_nc, 'time')
base_date <- unlist(ncatt_get(e_nc,'time')[1])
base_date <- strapplyc(base_date, "\\d+-\\d+-\\d+", simplify = TRUE)
base_date <- as.Date(base_date) + days


#### GETTING DATA RANGE AND DESCENTS ####
# get range of dates
first_date <- as.Date("2014-06-01")
final_date <- as.Date("2016-06-01")

# Grouping periods
groups <- as.character(seq(first_date, final_date, by="days"))

# Get starting value for descent (-1 to compensate flag set)
flag <- 0
new_flag <- 0
day = as.numeric(format(first_date, '%d'))
descent <- (as.numeric(format(first_date, '%m'))-1)*3
if (10 < day && day <= 20) descent = descent + 1
if (day > 20) descent = descent + 2 

# Group descents
for (i in 1:length(groups)) {
    date <- as.Date(groups[i])
    day = as.numeric(format(date, '%d'))
    if (day <= 10) { new_flag = 1 }
    else if (day <= 20) { new_flag = 2 }
    else { new_flag = 3 }

    if (flag != new_flag){
      flag = new_flag
      descent = descent + 1
      if (descent==37) { descent = 1 }
    }
    
    groups[i] = paste(format(date, '%Y'), descent)
}


# Calculating R²


# Calculating pearsons coeficient
