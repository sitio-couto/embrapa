library(pacman)
p_load(raster, ncdf4, rgdal, stringr, gsubfn, lubridate, ggplot2)

first_date <- as.Date("2014-06-01")
final_date <- as.Date("2016-06-01")
level = TRUE

  # Getting product files
  late_files <- sort(list.files('data/late_run',pattern='*.nc',full.names=TRUE))
  final_files <- sort(list.files('data/final_run',pattern='*.nc',full.names=TRUE))
  
  # Setting the base date from the files
  e_nc <- nc_open(late_files[1])
  days <- ncvar_get(e_nc, 'time')
  base_date <- unlist(ncatt_get(e_nc,'time')[1])
  base_date <- strapplyc(base_date, "\\d+-\\d+-\\d+", simplify = TRUE)
  base_date <- as.Date(base_date)
  nc_close(e_nc)
  
  # HANDLING DATE DIFFERENCES
  # Removes dates which have no pair in the 
  # other stack of files truncating the amount
  # of data to the size of the smallest stack.
  
  # Starting index and range to iterate
  i = 1
  timestamps = c()
  len = min(c(length(late_files), length(final_files)))
  
  # Remove files without pairs (if leveling is set)
  while (i <= len && level) {
    late = nc_open(late_files[i])
    final = nc_open(final_files[i])
    l = ncvar_get(late, 'time')
    f = ncvar_get(final, 'time')
    
    while (l != f) {
      if (l < f) {
        print("Final product missing date:")
        print(base_date + l)
        late_files = late_files[-i]
        late = nc_open(late_files[i])
        l = ncvar_get(late, 'time')
      } else if (l > f) {
        print("Late product missing date:")
        print(base_date + f)
        final_files = final_files[-i]
        final = nc_open(final_files[i])
        f = ncvar_get(final, 'time')
      }
      
      timestamps = timestamps[-i]
      len = min(length(late_files), length(final_files))
    }
    
    timestamps = append(timestamps, as.Date(base_date + l))  
    if (tail(timestamps, 1) == first_date) {start = i}
    if (tail(timestamps, 1) == final_date) {end = i}
    
    nc_close(late)
    nc_close(final)
    i = i + 1
  }
  
  # Slicing files for the desired range
  final_files = final_files[(start:end)]
  final_files = final_files[!is.na(final_files)]
  late_files = late_files[(start:end)]
  late_files = late_files[!is.na(late_files)]
  timestamps = timestamps[(start:end)]
  
  # Initialize stacks
  late_stack <- raster::stack(late_files)
  final_stack <- raster::stack(final_files)
  
  # Bind timestamps to raster layers
  late_stack = setZ(late_stack, timestamps, name='time') 
  final_stack = setZ(final_stack, timestamps, name='time')  
  
  out = list('base_date'=base_date,'late'=late_stack, 'final'=final_stack)

for (i in 1:length(final_files)) {
  if(getZ(final_stack[[i]]) != getZ(late_stack[[i]])) { print("ERRADO! (1)") }
  l = base_date + ncvar_get(nc_open(late_files[i]), 'time')
  f = base_date + ncvar_get(nc_open(final_files[i]), 'time')
  t = timestamps[i]
  if(l != f || l != t) {print("ERRADO!")}
}
