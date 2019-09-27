library(pacman)
p_load(raster, ncdf4, rgdal, stringr, gsubfn, lubridate, ggplot2)

late_files <- sort(list.files('.',pattern='*.nc',full.names=TRUE))
nc = nc_open(late_files[1])
