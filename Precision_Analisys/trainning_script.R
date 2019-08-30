library(pacman)
p_load(raster, RColorBrewer, rasterVis, ncdf4, maptools, rgdal, ggmap, ggplot2, reshape2, stringr, mapdata, mapview, gsubfn)

# Reading files and stacking days
early_files <- list.files('data/early_run',pattern='*.nc',full.names=TRUE)
early_stack <- raster::stack(early_files)

late_files <- list.files('data/late_run',pattern='*.nc',full.names=TRUE)
late_stack <- raster::stack(late_files)

final_files <- list.files('data/final_run',pattern='*.nc',full.names=TRUE)
final_stack <- raster::stack(final_files)

# Geting dimensions from any file
e_nc <- nc_open(early_files[1])
lat <- ncvar_get(e_nc,'lat')
lon <- ncvar_get(e_nc,'lon')

# Setting the base date from the database
days <- ncvar_get(e_nc, 'time')
base_date <- unlist(ncatt_get(e_nc,'time')[1],use.names=FALSE)
base_date <- strapplyc(base_date, "\\d+-\\d+-\\d+", simplify = TRUE)
base_date <- as.Date(base_date) + days

#### PLOTING RASTER DATA ####
# get range of dates
first_date <- "2014-06-01"
final_date <- "2014-06-01"

first_date <- as.Date(first_date)
final_date <- as.Date(final_date)

start <- as.numeric(first_date - base_date) + 1 # Does not start at 0
end <- as.numeric(final_date - base_date) + 1
days <- (end-(start-1))

# Getting averages
early_mean = 0
late_mean = 0
final_mean = 0
for (i in start:end){
  early_mean <- early_mean + early_stack[[i]]
  late_mean <- late_mean + late_stack[[i]]
  final_mean <- final_mean + final_stack[[i]]
} 
early_mean <- early_mean/days
late_mean <- late_mean/days
final_mean <- final_mean/days

# Ploting average from all runs 
par(mfrow=c(2,3))

image(lon, lat, as.matrix(flip(early_mean[[1]],'y')))
data(wrld_simpl)
plot(wrld_simpl,add=TRUE,phc=1)

image(lon, lat, as.matrix(flip(late_mean[[1]],'y')))
data(wrld_simpl)
plot(wrld_simpl,add=TRUE,phc=2)

image(lon, lat, as.matrix(flip(final_mean[[1]],'y')))
data(wrld_simpl)
plot(wrld_simpl,add=TRUE,phc=3)

# Ploting difference when compared to final run
early_diff <- early_mean - final_mean
image(lon, lat, as.matrix(flip(early_diff,'y')))
data(wrld_simpl)
plot(wrld_simpl,add=TRUE,phc=4)

late_diff <- late_mean - final_mean
image(lon, lat, as.matrix(flip(late_diff,'y')))
data(wrld_simpl)
plot(wrld_simpl,add=TRUE,phc=5)

#### CALCULATING RMSE ####
# get range of dates
first_date <- "2014-06-01"
final_date <- "2014-07-01"

first_date <- as.Date(first_date)
final_date <- as.Date(final_date)

start <- as.numeric(first_date - base_date) + 1 # Does not start at 0
end <- as.numeric(final_date - base_date) + 1
days <- (end-(start-1))

# Get early rmse for desired range
early_rmse = 0
for (i in start:end){
  early_rmse <- early_rmse + (early_stack[[i]] - final_stack[[i]])**2  
} 
early_rmse <- sqrt(early_rmse/(end-(start-1)))

# Plot early RMSE 
image(lon, lat, as.matrix(flip(early_rmse,'y')))
data(wrld_simpl)
plot(wrld_simpl,add=TRUE,phc=5)

# Get late rmse for desired range
late_rmse = 0
for (i in start:end){
  late_rmse <- late_rmse + (late_stack[[i]] - final_stack[[i]])**2  
} 
late_rmse <- sqrt(late_rmse/(end-(start-1)))

# Plot late  RMSE 
image(lon, lat, as.matrix(flip(late_rmse,'y')))
data(wrld_simpl)
plot(wrld_simpl,add=TRUE,phc=5)

#### PLOTING ERROR COVERAGE HISTOGRAM ####
par(mfrow=c(1,1))

ggplot(data = as.matrix(flip(late_diff,'y')), mapping = aes(x = longitude, y = latitude)) + 
geom_point(aes(col = median_price, size = transactions)) +
geom_text(aes(label = city), size = 2, nudge_y = 0.01) +
scale_color_distiller(palette = "YlOrRd", direction = 1)

# Plot graph indicating the percentage of a certain error in the map for early run
hist_data <- hist(early_diff[early_diff!=0], breaks = 2000)
hist_data$counts <- (hist_data$counts/length(early_diff[[1]]))*100
hist_data$breaks <- hist_data$breaks[-1]
x <- hist_data$breaks[hist_data$counts>0.1]
y <- hist_data$counts[hist_data$counts>0.1]
plot(x, y, type='s')

# Plot graph indicating the percentage of positive erros in the map late run
hist_data <- hist(late_diff[late_diff!=0], breaks = 2000)
hist_data$counts <- (hist_data$counts/length(early_diff[[1]]))*100
hist_data$breaks <- hist_data$breaks[-1]
x <- hist_data$breaks[hist_data$counts>0.1]
y <- hist_data$counts[hist_data$counts>0.1]
plot(x, y, type='s')

