library(pacman)
p_load(raster, ncdf4, maptools, rgdal, ggmap, ggplot2, reshape2, stringr, mapdata, mapview)


early_files <- list.files('data/early_run',pattern='*.nc',full.names=TRUE)
early_stack <- raster::stack(early_files)

late_files <- list.files('data/late_run',pattern='*.nc',full.names=TRUE)
late_stack <- raster::stack(late_files)

final_files <- list.files('data/final_run',pattern='*.nc',full.names=TRUE)
final_stack <- raster::stack(final_files)

e_nc <- nc_open(early_files[1])
lat <- ncvar_get(e_nc,'lat')
lon <- ncvar_get(e_nc,'lon')

par(mfrow=c(2,3))

image(lon, lat, as.matrix(flip(early_stack[[1]],'y')))
data(wrld_simpl)
plot(wrld_simpl,add=TRUE,phc=1)

image(lon, lat, as.matrix(flip(late_stack[[1]],'y')))
data(wrld_simpl)
plot(wrld_simpl,add=TRUE,phc=2)

image(lon, lat, as.matrix(flip(final_stack[[1]],'y')))
data(wrld_simpl)
plot(wrld_simpl,add=TRUE,phc=3)

early_diff <- early_stack[[1]]-final_stack[[1]]
image(lon, lat, as.matrix(flip(early_diff,'y')))
data(wrld_simpl)
plot(wrld_simpl,add=TRUE,phc=4)

late_diff <- late_stack[[1]]-final_stack[[1]]
image(lon, lat, as.matrix(flip(late_diff,'y')))
data(wrld_simpl)
plot(wrld_simpl,add=TRUE,phc=5)

s <- 1
e <- 30
diff_sum = 0
for (i in s:e){
  diff_sum = (diff_sum + (early_stack[[i]] - final_stack[[i]]))**2  
} 

test <- early_stack[[1]]
for (i in 1:length(lat)){
  for (j in 1:length(lon)){
    test[i,j] = 1
  }
} 

par(mfrow=c(1,1))

ggplot(data =  as.matrix(flip(late_diff,'y')), mapping = aes(x = longitude, y = latitude)) + 
geom_point(aes(col = median_price, size = transactions)) +
geom_text(aes(label = city), size = 2, nudge_y = 0.01) +
scale_color_distiller(palette = "YlOrRd", direction = 1)

# Plot graph indicating the percentage of a certain error in the map 
hist_data <- hist(early_diff[early_diff!=0], breaks = 2000)
hist_data$counts <- (hist_data$counts/length(early_diff[[1]]))*100
hist_data$breaks <- hist_data$breaks[-1]
x <- hist_data$breaks[hist_data$counts>0.1]
y <- hist_data$counts[hist_data$counts>0.1]
plot(x, y, type='s')

# Plot graph indicating the percentage of positive erros in the map
hist_data <- hist(early_diff[early_diff>0], breaks = 2000)
hist_data$counts <- (hist_data$counts/length(early_diff[[1]]))*100
hist_data$breaks <- hist_data$breaks[-1]
x <- hist_data$breaks[hist_data$counts>0.1]
y <- hist_data$counts[hist_data$counts>0.1]
plot(x, y, type='s')

