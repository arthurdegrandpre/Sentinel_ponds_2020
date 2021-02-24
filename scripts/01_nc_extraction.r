library(ncdf4)
library(raster)
library(sf)
library(leaflet) # optionnal, used for validation
library(tidyverse) # optionnal, used for validation
library(ggplot2) # optionnal, used for validation


rm(list=ls())

output_df = data.frame()

path_nc = "D:/nic_transfer/" # path to all .nc files
ncdir = dir(path_nc, pattern=".nc$")

path_poly = "./data/mask_nic_20191126/" # path to all ponds .shp files
polydir = dir(path_poly, pattern=".shp$")

# Assuming that all the .nc files will be structured in the same way, run this loop and voila :) 

for(i in 152:length(ncdir)){
# i=152
print(paste0("image ",i," out of ",length(ncdir)))
nc = nc_open(paste0(path_nc,ncdir[i]))
# nc1 # summary of the netcdf dataset

nc_proj4 = ncatt_get(nc,0, "proj4_string")$value
nc_res = ncatt_get(nc,0, "pixel_size")$value
nc_extent_x = ncatt_get(nc,0, "xrange")$value
nc_extent_y = ncatt_get(nc,0, "yrange")$value

var_names = attributes(nc$var)$names
bands = grep("^rhos",var_names, value=T)

r = raster()
for(b in 1:length(bands)){
  r = stack(r, raster(t(ncvar_get(nc, bands[b]))))
}

if(is.na(getValues(r[[1]])[1])==T){
  print(paste0("empty raster at ",i))
  next
}

r = brick(r)
extent(r) = c(nc_extent_x,nc_extent_y)
res(r) = nc_res
proj4string(r) = nc_proj4
names(r) = bands

rp = projectRaster(r, crs="+init=epsg:4326")

#  leaflet() %>% 
#    addTiles() %>% 
#    addRasterImage(rp$rhos_833) # visual validation of the raster

for(j in 1:length(polydir)){
  poly = st_read(paste0(path_poly,polydir[j]), quiet=T)
  
  cr = crop(rp,poly)
  mr = mask(cr, poly)
  
  dfmr = as.data.frame(mr, xy=T)
  dfmr = dfmr[complete.cases(dfmr),]
  dfmr$date = substr(ncdir[i],9,18)
  dfmr$satellite = substr(ncdir[i],1,3)
  dfmr$pond = gsub("_Polygon.shp","",polydir[j])
  colnames(dfmr) = colnames(output_df)
  output_df = rbind(output_df, dfmr)
}
}

write.csv(output_df, "./data/nc_extract.csv")

View(output_df)
View(dfmr)
### optionnal validation : number of pixels per pond per date

val_df = output_df %>% group_by(date,pond) %>% summarise(frequency = n()) %>% ungroup

ggplot(val_df, aes(x=date, y=frequency, group=pond)) +
  geom_point()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  facet_wrap(~pond)
