library(terra)
library(sf)

###################
##List variables
##Directory
image_directory = c("./2022 UAS imagery")

##Directory to write files to
ExGI_mask_outpath = c("./Spectral Work/R Code output/Masked")

####################
##Functions
ExGI_fun = function(image){
  print("start of ")
  print(names(image))
  Red = image[[1]]
  Green = image[[2]]
  Blue = image[[3]]
  ExGI = (2*Green)- (Red + Blue)
  names(ExGI) = substr(names(ExGI),1, nchar(names(ExGI))-2)
  print(names(ExGI))
  print("End of")
  print(names(image))
  
  return (ExGI)
  
}

##Function to write to disk using lapply
write_raster_disk = function(raster, output_directory){
  filename = file.path(output_directory, paste0(names(raster),"_ExGI.tif"))
  writeRaster(raster, filename, overwrite = T)
}


##################
##Read in paths for image files
file_paths = list.files(image_directory, pattern = "\\.tif$", full.names = T)
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
##Calculate Excess Greenness index using lapply and function
time1 = Sys.time()
ExGI_list = lapply(raster_list, ExGI_fun)
time2 = Sys.time()
print(time2-time1)

##################
##Masked_function to set NA
time1 = Sys.time()
raster_mlist = lapply(1:length(ExGI_list), function(i){
  masked_layer = ifel(raster_list[[i]][[4]] == 0, NA, ExGI_list[[i]])
})
time2 = Sys.time()
print(time2-time1)


##################
##Write to disk
lapply(raster_mlist, write_raster_disk, ExGI_mask_outpath)


