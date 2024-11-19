##Surface Roughness Toolbox
install.packages('terra', repos='https://rspatial.r-universe.dev')
library(terra)
library(sf)
library(fields)

##Get and set directory
getwd()

##Memory options
terraOptions(memfrac = 0.1, memmin = 0.1)

##Set temp directory to external drive
tempdir("D:/Temp")


###################
##List variables
##Directory
DEM_directory = c("./Klamath Surface Roughness/2022 DEMs for Roughness modeling/2022 UAS PC")

output_directory = c("./Klamath Surface Roughness/2022 Surface Roughness/1Meter/")
###################
##Functions

##Function to read in list of rasters
read_rasters <- function(file_paths) {
  # Initialize an empty list to store the rasters
  raster_list <- list()
  
  # Loop through each file path and read the raster
  for (file_path in file_paths) {
    raster <- rast(file_path)  # Read the raster
    raster_list[[basename(file_path)]] <- raster  # Store in the list with the basename as the key
    rm(raster)  # Optional: Remove the variable to free memory
  }
  
  return(raster_list)  # Return the list of rasters
}

##Function to write to disk using lapply
write_raster_disk = function(raster, output_directory){
  filename = file.path(output_directory, paste0(names(raster),".tif"))
  writeRaster(raster, filename, overwrite = T)
}


##Function to create relative elevation model using a list of raster files
##Function to create relative elevation model using a list of raster files
Surface_Roughness_function <- function(DEM_list, grid_size = 20, window = 3) {
  Rough_results <- list()  # Initialize list to store results
  
  for (dem_file in DEM_list) {
    # Load DEM raster
    plot(dem_file)
    print(summary(dem_file))
    
    # Extract base name for file saving
    dem_name <- names(dem_file)
    print(dem_name)
    
    # Check if DEM is a raster object
    if (!inherits(dem_file, "SpatRaster")) {
      stop("Each element in DEM_list must be a SpatRaster object.")
    }
    
    # Create fishnet grid
    DEM_ext = ext(dem_file)
    width = DEM_ext[2]- DEM_ext[1]
    height = DEM_ext[4]- DEM_ext[3]
    n_cols = ceiling(width/grid_size)
    n_rows =ceiling(height/grid_size)
    grid_poly = st_as_sfc(st_bbox(DEM_ext))
    dem_crs = crs(dem_file)
    fishnet <- st_make_grid(grid_poly, n = c(n_cols, n_rows), crs = dem_crs, square = TRUE) %>%
      st_as_sf()
    print("fishnet made")
    
    # Get fishnet centroids
    grid_centroids = st_centroid(fishnet)
    grid_centroids = st_set_crs(grid_centroids, terra::crs(dem_file))##Change to dem_crs
    print(crs(grid_centroids))
    print("Centroids made")
    
    # Zonal means
    mean_elev = zonal(dem_file, vect(fishnet), fun = "mean", as.raster = TRUE, as.polygons = TRUE, na.rm = TRUE)
    names(mean_elev) = "Mean_Elev"
    print("zonal stats calculated")
    
    # Extract mean elevation to centroids
    cent_elev = extract(mean_elev, grid_centroids)
    print("elevations extracted to centroids")
    
    # Convert cent_elev to a dataframe and join with centroids
    cent_elev_df <- as.data.frame(cent_elev)
    grid_centroids_sf <- st_as_sf(grid_centroids)
    grid_centroids_sf <- cbind(grid_centroids_sf, cent_elev_df)
    print("elevations joined to centroids")
    
    
    # Extract coordinates and elevation values for interpolation
    x_coords <- st_coordinates(grid_centroids_sf)[, 1]
    y_coords <- st_coordinates(grid_centroids_sf)[, 2]
    z_values <- grid_centroids_sf$Mean_Elev
    print("coords and elevations extracted for interpolation")
    
    
    # Combine into a data frame
    print("x and y coords joined")
    xy <- as.data.frame(cbind(x_coords, y_coords))
    
    
    # Check for NA values and remove them
    na_indices <- is.na(xy[, 1]) | is.na(xy[, 2]) | is.na(z_values)
    if (any(na_indices)) {
      xy <- xy[!na_indices, ]
      z_values <- z_values[!na_indices]
    }
    print("NA's removed")
    
    
    # Perform Thin Plate Spline interpolation
    print("starting spline model")
    interpolated_values <- Tps(xy, z_values)
    print("spline modeled")
    
    # Create interpolated raster
    interpolated_raster <- interpolate(rast(dem_file), interpolated_values)
    print("Spline interpolated onto raster")
    
    # Mask interpolated values with original DEM
    masked_raster <- mask(interpolated_raster, dem_file)
    print("spline raster masked by orginal DEM")
    rm(interpolated_raster)
    
    snapped_raster = resample(masked_raster, dem_file, method = "bilinear")
    print("spline raster snapped to original DEM")
    rm(masked_raster)
    
    # Calculate Relative Elevation Model
    REM = dem_file - snapped_raster
    print("interpolated raster subtracted from DEM")
    rm(snapped_raster)
    
    # Surface roughness 3x3 window of standard deviations
    print("Starting 3x3 moving window standard deviation")
    SurfaceRoughness <- focal(REM, w = matrix(1, nrow = window, ncol = window), fun = sd, na.rm = TRUE)
    
    Surface_Roughness <-  resample(SurfaceRoughness, dem_file, method = "bilinear")
    
    print("storing Surface Roughness raster in list")
    Rough_results[[paste("Surface_Roughness", length(Rough_results) + 1)]] <- SurfaceRoughness
    
    # Write grid centroids to disk
    st_write(grid_centroids_sf, paste0(output_directory, dem_name, "_grid_centroids.shp"), append = F)
    print("Grid centroids saved to disk")
    
    # Write fishnet grid to disk
    st_write(fishnet, paste0(output_directory, dem_name, "_fishnet_grid.shp"), append = F)
    print("fishnet saved to disk")
    
    
    # Write REM to disk
    writeRaster(REM, paste0(output_directory, dem_name, "_REM.tif"), overwrite = TRUE)
    print("Relative Elevation Model saved to disk")
    
    #Write Surface Roughness to disk
    writeRaster(Surface_Roughness, paste0(output_directory, dem_name,"_SurfaceRoughness.tif"), overwrite = T)
    
    
    
  }
  
  return(Rough_results)
  
}




#####################
##Read in paths for image files
file_paths = list.files(DEM_directory, pattern = "\\.tif$", full.names = T)
print(file_paths)

##Read in DEM raster
DEM_list = read_rasters(file_paths)


##Do REM function

startTime = Sys.time()
Rough_list = Surface_Roughness_function(DEM_list, grid_size = 1, window = 3)
endTime = Sys.time()

print(paste("Function run time is", endTime- startTime))

for(file in Rough_list){
  print(summary(file))

}

##write to disk
lapply(Rough_list, write_raster_disk, output_directory)
#


Rough_list[[1]]
