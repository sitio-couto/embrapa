library(pacman)
p_load(raster, ncdf4, rgdal, stringr, gsubfn, lubridate)



#### READING FILES AND STACKING DAYS ####
# Daily product
late_files <- list.files('data/late_run',pattern='*.nc',full.names=TRUE)
late_stack <- raster::stack(late_files)
# Final product files
final_files <- list.files('data/final_run',pattern='*.nc',full.names=TRUE)
final_stack <- raster::stack(final_files)

# Setting the base date from the files
e_nc <- nc_open(late_files[1])
days <- ncvar_get(e_nc, 'time')
base_date <- unlist(ncatt_get(e_nc,'time')[1])
base_date <- strapplyc(base_date, "\\d+-\\d+-\\d+", simplify = TRUE)
base_date <- as.Date(base_date) + days




#### GETTING DATA RANGE AND DECENDS ####
# get range of dates
first_date <- as.Date("2014-06-01")
final_date <- as.Date("2014-08-01")
#======================================#

# Slicing stacks for the desired range
start <- as.numeric(first_date - base_date) + 1 # Does not start at 0
end <- as.numeric(final_date - base_date) + 1
final_stack = final_stack[[start:end]]
late_stack = late_stack[[start:end]]

# Grouping periods
groups <- as.character(seq(first_date, final_date, by="days"))

# Get starting value for decends (-1 to compensate flag set)
flag <- 0
new_flag <- 0
day = as.numeric(format(first_date, '%d'))
decend <- (as.numeric(format(first_date, '%m'))-1)*3
if (10 < day && day <= 20) decend = decend + 1
if (day > 20) decend = decend + 2 

# Group decends
for (i in 1:length(groups)) {
  date <- as.Date(groups[i])
  day = as.numeric(format(date, '%d'))
  if (day <= 10) { new_flag = 1 }
  else if (day <= 20) { new_flag = 2 }
  else { new_flag = 3 }
  
  if (flag != new_flag){
    flag = new_flag
    decend = decend + 1
    if (decend==37) { decend = 1 }
  }
  
  #groups[i] = paste(format(date, '%Y'), decend)
  groups[i] = as.numeric(format(date, '%Y'))*100+ as.numeric(decend)
}





#### HANDLING DATE DIFFERENCES ####
# Removes dates which have no pair in the other stack truncating the amount
# of data to the size of the smallest stack.
len = min(c(dim(final_stack)[3], dim(late_stack)[3]))
for (i in 1:len) {
  late = ncvar_get(nc_open(late_files[i]), 'time')
  final = ncvar_get(nc_open(final_files[i]), 'time')
  
  while (late != final) {
    if ( late < final ) {
      late_stack = dropLayer(late_stack, i)
      late_files = late_files[-i]
      late = ncvar_get(nc_open(late_files[i]), 'time')
    } else if (final < late) {
      final_stack = dropLayer(final_stack, i)
      final_files = final_files[-i]
      final = ncvar_get(nc_open(final_files[i]), 'time')
    } 
    groups = groups[-i]
  } 
}

# Casting groups to sequence
indices = as.numeric(groups)
old = indices[1]
count = 1
for (i in 1:length(indices)) {
  if (old == indices[i]) {
    indices[i] = count
  } else {
    count = count +  1
    old = indices[i]
    indices[i] = count
  }
}

# Getting months mask
months = groups
for (i in 1:length(groups)) {
  months[i] =floor((as.numeric(months[i])%%100-1)/3)
}
months = as.numeric(months)

#### GETING INDEXES FOR DECENDS ####                                                                                                                 
# Get decend precitation sums
real_sum <- stackApply(final_stack, indices, sum)
prev_sum <- stackApply(late_stack, indices, sum)

# Get decend RMSE
N = length(late_stack[[1]])
decend_rmse = c()
for (i in 1:dim(real_sum)[3]) {
  decend_rmse = c(rmse, sqrt(sum(as.matrix((prev_sum[[i]]-real_sum[[i]])**2))/N))
}

# Get months precitation sums
real_sum <- stackApply(final_stack, months, sum)
prev_sum <- stackApply(late_stack, months, sum)

# Get months rmse
N = length(late_stack[[1]])
months_rmse = c()
for (i in 1:dim(real_sum)[3]) {
  months_rmse = c(months_rmse, sqrt(sum(as.matrix((prev_sum[[i]]-real_sum[[i]])**2))/N))
}

# Plot RMSE
plot(unique(groups), decend_rmse)
plot(unique(months), months_rmse)

# Calculating R²


# Calculating pearsons coeficient
