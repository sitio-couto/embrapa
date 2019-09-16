library(pacman)
p_load(raster, RColorBrewer, rasterVis, ncdf4, maptools, rgdal, ggmap, ggplot2, reshape2, stringr, mapdata, gsubfn,rgeos)

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
late_mean = 0
final_mean = 0
for (i in start:end){
  late_mean <- late_mean + late_stack[[i]]
  final_mean <- final_mean + final_stack[[i]]
} 
late_mean <- late_mean/days
final_mean <- final_mean/days

# Ploting average from all runs 
par(mfrow=c(2,2))

image(lon, lat, as.matrix(flip(late_mean[[1]],'y')))
data(wrld_simpl)
plot(wrld_simpl,add=TRUE,phc=1)

image(lon, lat, as.matrix(flip(final_mean[[1]],'y')))
data(wrld_simpl)
plot(wrld_simpl,add=TRUE,phc=2)

# Ploting difference when compared to final run
late_diff <- abs(late_mean - final_mean)
image(lon, lat, as.matrix(flip(late_diff,'y')))
data(wrld_simpl)
plot(wrld_simpl,add=TRUE,phc=3)

#### CALCULATING RMSE ####
# Getting date inputs
first_date <- "2014-06-01"
final_date <- "2014-07-01"
# Casting
first_date <- as.Date(first_date)
final_date <- as.Date(final_date)
# Setting ranges
start <- as.numeric(first_date - base_date) + 1 # Does not start at 0
end <- as.numeric(final_date - base_date) + 1
days <- (end-(start-1))

# Get late RMSE for desired range
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

# Plot graph indicating the percentage of positive erros in the map late run
hist_data <- hist(late_diff[late_diff!=0], breaks = 2000)
hist_data$counts <- (hist_data$counts/length(late_diff[[1]]))*100
hist_data$breaks <- hist_data$breaks[-1]
x <- hist_data$breaks[hist_data$counts>0.1]
y <- hist_data$counts[hist_data$counts>0.1]
plot(x, y, type='s')

############ TEST AREA ##############

colr <- colorRampPalette(rev(brewer.pal(11, 'RdBu')))
mp = map('worldHires', xlim = range(lon), ylim = range(lat))
mp <- data.frame(lon=mp$x, lat=mp$y)

# Plotting diference between products
late_diff <- late_mean - final_mean
late_diff <- setMinMax(late_diff)
border <- ceiling(max(abs(minValue(late_diff)), maxValue(late_diff))) 
levelplot(as.matrix(flip(late_diff,'y')), 
          margin=FALSE,                       # suppress marginal graphics
          colorkey=list(
            space='bottom',                   # plot legend at bottom
            labels=list(at=c(-border,0,border), font=1)      # legend ticks and labels 
          ),    
          par.settings=list(
            axis.line=list(col='transparent') # suppress axes and legend outline
          ),
          scales=list(draw=FALSE),            # suppress axis labels
          col.regions=colr,                   # colour ramp
          at=seq(-border, border, len=12))    # colour ramp break
