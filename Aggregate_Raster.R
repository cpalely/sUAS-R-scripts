##Reclassify rasters
library(terra)
library(parallel)
library(sf)



###Read surface roughness raster
getwd()

SurRough_direct = "./Klamath Surface Roughness/2022 Surface Roughness/3Meter/"

file_paths = list.files(SurRough_direct, pattern = "\\.tif$", full.names = T)
print(file_paths)
raster_names = basename(file_paths)
site = "PC"

##Set up cluster for parallel process
num_cores = detectCores()
num_workers = num_cores - 3
cl = makeCluster(num_workers)

raster_list = list()
#Loop to read
for(file_path in file_paths){
  raster = rast(file_path)
  raster_list[[basename(file_path)]] = raster
  plot(raster)
  rm(raster)
}

############################
PC_Hi= raster_list[[4]]
alt = "Hi"
target_res = 1


aggregate_raster <- function(raster, SurRough_direct, site, alt, target_res, num_workers ) {
  cell_size <- res(PC_low)
  
  factor <- floor(target_res / cell_size[1])
  print(factor)
  
  starttime <- Sys.time()
  PC_agg <- terra::aggregate(raster, fact = factor, fun = mean, na.rm = TRUE, cores = num_workers)
  endtime <- Sys.time()
  print(endtime - starttime)
  
  outputname <- paste0(SurRough_direct, site, "_", alt, "_", target_res, "m_aggregated_raster.tif")
  
  # Save the aggregated raster
  writeRaster(PC_low_agg, filename = outputname, filetype = "GTiff", overwrite = TRUE)
}

aggregate_raster(PC_Hi, SurRough_direct, site, alt, target_res = target_res, num_workers = num_workers  )
