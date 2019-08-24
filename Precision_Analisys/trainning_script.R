library(pacman)
p_load(raster, ncdf4, maptools)

early_files <- list.files('data/early_run',pattern='*.nc',full.names=TRUE)
early_stack <- raster::stack(early_files)

late_files <- list.files('data/late_run',pattern='*.nc',full.names=TRUE)
late_stack <- raster::stack(late_files)

final_files <- list.files('data/final_run',pattern='*.nc',full.names=TRUE)
final_stack <- raster::stack(final_files)

nc <- nc_open(early_files[0])
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

early_diff <- abs(early_stack[[1]]-final_stack[[1]])
image(lon, lat, as.matrix(flip(early_diff,'y')))
data(wrld_simpl)
plot(wrld_simpl,add=TRUE,phc=4)

late_diff <- abs(late_stack[[1]]-final_stack[[1]])
image(lon, lat, as.matrix(flip(late_diff,'y')))
data(wrld_simpl)
plot(wrld_simpl,add=TRUE,phc=5)
