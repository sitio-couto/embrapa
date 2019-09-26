library(pacman)
p_load(raster, ncdf4, rgdal, stringr, gsubfn, lubridate, ggplot2)




#### READ AND LEVEL DATA ####
# Loads only a predefined range of layers to memory
# based on the given range. Also discards data which
# has no pair (p.e. theres a missing day in the late
# product, so the same day will be discarded in the 
# final product). 
# Returns raster stacks with the layer being days
# and each day with a its date binded to the layer
# by setZ
read_data <- function(first_date, last_date) {
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
  while (i <= len) {
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
  
  return(list('base_date'=base_date,'late'=late_stack, 'final'=final_stack))
}





#### FUNCTION TO GET DECENDS ####
# Given a starting and ending date, this function
# returns a Nx2 matrix where the first colunm is
# the legends for each decend and the second is the
# id for each descend to be used with stackApply() 
get_decends <- function (dates) {
  # Seetup varibles
  flag <- 0
  new_flag <- 0
  legend = as.character(dates)
  
  # Get starting value for decends
  day = as.numeric(format(dates[1], '%d'))
  decend <- (as.numeric(format(dates[1], '%m'))-1)*3
  if (10 < day && day <= 20) decend = decend + 1
  if (day > 20) decend = decend + 2 
  
  # Group decends
  for (i in 1:length(dates)) {
    date <- as.Date(dates[i])
    day = as.numeric(format(date, '%d'))
    if (day <= 10) { new_flag = 1 }
    else if (day <= 20) { new_flag = 2 }
    else { new_flag = 3 }
    
    if (flag != new_flag){
      flag = new_flag
      decend = decend + 1
      if (decend==37) { decend = 1 }
    }
    
    legend[i] = paste(format(date, '%Y'), decend)
  }
  
  # Casting groups to sequence
  indices = c()
  old = legend[1]
  count = 1
  for (i in 1:length(legend)) {
    if (old == legend[i]) {
      indices[i] = count
    } else {
      count = count +  1
      old = legend[i]
      indices[i] = count
    }
  }
  
  return(list("legend"=unique(legend), "mask"=indices))
}






#### FUNCTION TO GET MONTHS ####
# Given a sequence of dates, returns the sequence
# grouped by months and a mask for using with stack
# apply and grouping layers from the same month.
get_months = function(dates) {
  # Getting months mask
  old <- 0
  count = 0
  indexes = c()
  legends = c()
  
  for (i in 1:length(dates)) {
    legends[i] = format(dates[i], '%Y-%m')
    if (old == legends[i]) {
      indexes[i] = count 
    } else {
      old = legends[i]
      count = count + 1
      indexes[i] = count 
    }
  }
  legends = unique(legends)
  
  return(list("legend"=legends, "mask"=indexes))
}





#### EXECUTING SCRIPT (STARTING POINT) ####
# get range of dates
first_date <- as.Date("2015-06-01")
final_date <- as.Date("2016-06-01")
#======================================#

stacks = read_data(first_date, final_date)
decend = get_decends(getZ(stacks$late))
months = get_months(getZ(stacks$late))





#### RMSE PER DECEND ####
# Get decend precitation sums
real_sum <- stackApply(stacks$final, decend$mask, sum)
prev_sum <- stackApply(stacks$late, decend$mask, sum)
# Calculate rmse
N = length(stacks$late[[1]])
decend_rmse = c()
for (i in 1:dim(real_sum)[3]) {
  decend_rmse = c(decend_rmse, sqrt(sum(as.matrix((prev_sum[[i]]-real_sum[[i]])**2))/N))
}
# Plot RMSE
frame = data.frame(decend$legend, decend_rmse)
ggplot(frame, aes(x = decend.legend, y = decend_rmse)) + geom_point(size=2, shape=23)

#### RMSE PER MONTH ####
# Get months precitation sums
real_sum <- stackApply(stacks$final, months$mask, sum)
prev_sum <- stackApply(stacks$late, months$mask, sum)

# Get months rmse
N = length(stacks$late[[1]])
months_rmse = c()
for (i in 1:dim(real_sum)[3]) {
  months_rmse = c(months_rmse, sqrt(sum(as.matrix((prev_sum[[i]]-real_sum[[i]])**2))/N))
}

frame = data.frame(months$legend, months_rmse)
ggplot(frame, aes(x = months.legend, y = months_rmse)) + geom_point(size=2, shape=23)







#### RMSE PER CELL FOR EACH DECEND ####
l = dim(stacks$late)[3]
fun = function(x) { x[(1:l)] - x[(l+1:2*l)] }
diff = calc(stack(stacks$late, stacks$final), fun)
diff = stackApply(diff, decend$mask, sum)
N = length(stacks$late)
diff = sqrt((diff**2)/N)

