first_date <- as.Date("2014-06-01")
final_date <- as.Date("2014-08-01")

# Daily product files
late_files <- list.files('data/late_run',pattern='*.nc',full.names=TRUE)
# Final product files
final_files <- list.files('data/final_run',pattern='*.nc',full.names=TRUE)

# Setting the base date from the files
e_nc <- nc_open(late_files[1])
days <- ncvar_get(e_nc, 'time')
base_date <- unlist(ncatt_get(e_nc,'time')[1])
base_date <- strapplyc(base_date, "\\d+-\\d+-\\d+", simplify = TRUE)
base_date <- as.Date(base_date) + days
nc_close(e_nc)

# Set idexes for predefined date range
start <- as.numeric(first_date - base_date) + 1 # Does not start at 0
end <- as.numeric(final_date - base_date) + 1

# Slicing stacks for the desired range
final_files = final_files[(start:end)]
final_files = final_files[!is.na(final_files)]
late_files = late_files[(start:end)]
late_files = late_files[!is.na(late_files)]

# HANDLING DATE DIFFERENCES
# Removes dates which have no pair in the 
# other stack of files truncating the amount
# of data to the size of the smallest stack.
i = 1
timestamps = seq(first_date, final_date, by='days')
len = min(c(length(late_files), length(final_files)))

# Open range and return
late_stack <- raster::stack()
final_stack <- raster::stack()

# Remove files without pairs
while (i <= len) {
  late = nc_open(late_files[i])
  final = nc_open(final_files[i])
  l = ncvar_get(late, 'time')
  f = ncvar_get(final, 'time')
  
  # If theres no time pair in the stacks, remove the stack layer
  if ( l != f ) {
    if ( l > f ) { final_files = final_files[-i] }
    if ( l < f ) { late_files = late_files[-i] }
    timestamps = timestamps[-i]
    len = min(c(length(late_files), length(final_files)))
    i = i - 1
  } else {
    late_stack = addLayer(late_stack, late_files[i])
    final_stack = addLayer(final_stack, final_files[i]) 
  }
  
  nc_close(late)
  nc_close(final)
  i = i + 1
}

# Bind timestamps to raster layers
late_stack = setZ(late_stack, timestamps, name='time') 
final_stack = setZ(final_stack, timestamps, name='time')  