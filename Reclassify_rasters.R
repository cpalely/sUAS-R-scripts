##Reclassification script
library(terra)
library(sf)

###################
##List variables
##Directory to write files to
raster_directory = c("./Spectral Work/R Code output/Masked")

##Directory to write files to
out_directory = c("./Spectral Work/R Code output/Masked/Reclassified")


####################
##Functions

##Function to write to disk using lapply
write_raster_disk = function(raster, output_directory){
  filename = file.path(output_directory, paste0(names(raster),"ExGI.tif"))
  writeRaster(raster, filename, overwrite = T)
}

##Reclassify function
reclassify_function = function(raster){
  ifel(raster > 0, 1, 0)
}


##################
##Read in paths for image files
file_paths = list.files(raster_directory, pattern = "\\.tif$", full.names = T)
print(file_paths)

##read in rasters using for loop
#Empty list
raster_list = list()

#Loop to read
for(file_path in file_paths){
  raster = rast(file_path)
  raster_list[[basename(file_path)]] = raster
  rm(raster)
}

##################
##Reclassify Rasters
time1 = Sys.time()
reclass_rasters = lapply(raster_list, reclassify_function)
time2 = Sys.time()
print(time2-time1)


