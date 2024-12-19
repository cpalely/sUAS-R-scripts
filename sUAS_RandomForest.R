library(terra)
library(randomForest)
library(parallel)

##Directories
raster_direct = "./Random Forest/PC/HiAlt/Rasters"
vect_direct = "./Random Forest/PC/HiAlt/"

out_direct = "./Random Forest/PC/HiAlt/Rasters/Agg"

##Functions
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

#######
##Set up cluster for parallel process
num_cores = detectCores()
num_workers = num_cores - 3
cl = makeCluster(num_workers)


#################################
##Read in rasters

file_paths = list.files(raster_direct, pattern = "\\.tif$", full.names = T)
print(file_paths)
raster_names = basename(file_paths)
raster_names

##Read in DEM raster
raster_list = read_rasters(file_paths)

for (raster_file in raster_list) {
  rast <- rast(raster_file)
  epsg_code <- crs(rast)
  
  if (!is.null(epsg_code)) {
    # Extracting EPSG code
    epsg <- gsub(".*:([0-9]+).*", "\\1", as.character(epsg_code))
    cat(paste("Raster:", raster_file, ", EPSG:", epsg, "\n"))
  } else {
    cat(paste("Raster:", raster_file, ", EPSG: Not available\n"))
  }
}


##Drop band 4 from RGB
#Set NO Data to null using band 4 first
# Set bands 1-3 to NA where band 4 == 0
rgb = raster_list[[1]]
rgb[[1]][rgb[[4]] == 0] <- NA
rgb[[2]][rgb[[4]] == 0] <- NA
rgb[[3]][rgb[[4]] == 0] <- NA

rgb_3band <- subset(rgb, 1:3)
raster_list[[1]] = rgb_3band
rm(rgb_3band)
plot(raster_list[[1]])

##Rename list elements
raster_names = c("RGB", "EXGI","GRVI","VARI","GCC","SurfRough_25cm","TRI")
names(raster_list) = raster_names

##Plot lyr name(s) and cell size
for (i in seq_along(raster_list)) {
  raster_file <- raster_list[[i]]
  raster_name <- names(raster_list)[i]
  
  print("Name:")
  print(raster_name)
  print("")
  
  print("cell size is:")
  print(res(raster_file))
  print("")
}





################################################################################
##Aggregate and resample rasters to common extent and cell size
# Step 1: Identify largest cell size
cell_sizes <- sapply(raster_list, res)  # Get cell sizes
largest_cell_size <- max(cell_sizes)     # Find the largest cell size

# Print the largest cell size
cat("Largest cell size:", largest_cell_size, "\n")



# Initialize variables to keep track of the largest extent
largest_extent <- NULL
largest_raster_name <- NULL

# Loop through the raster list to find the largest extent
for (i in seq_along(raster_list)) {
  raster_file <- raster_list[[i]]
  raster_name <- names(raster_list)[i]
  
  # Get the extent of the current raster
  current_extent <- ext(raster_file)
  
  # Calculate the area of the current extent
  current_area <- (current_extent$xmax - current_extent$xmin) * (current_extent$ymax - current_extent$ymin)
  
  if (is.null(largest_extent) || current_area > (largest_extent$xmax - largest_extent$xmin) * (largest_extent$ymax - largest_extent$ymin)) {
    largest_extent <- current_extent
    largest_raster_name <- raster_name
  }
}

# Print the name of the raster with the largest extent and the extent itself
cat("Raster with largest extent:", largest_raster_name, "\n")
print(largest_extent)


# Define a function to resample rasters to a common extent and resolution
resample_to_common_extent <- function(raster, common_raster) {
  return(resample(raster, common_raster, method = "bilinear"))
}

# Get the raster with the largest extent (you can change this to suit your needs)
common_raster <- raster_list[[1]]  # Start with the first raster

#Aggregate each raster and resample to the common extent
aggregated_rasters <- list()  # Initialize the list to store aggregated rasters

StartTime = Sys.time()
for (i in seq_along(raster_list)) {
  raster_file <- raster_list[[i]]
  raster_name <- names(raster_list)[i]
  
  # Aggregate the raster to the largest cell size
  aggregated_raster <- aggregate(raster_file, fact = c(largest_cell_size / res(raster_file)[1], 
                                                       largest_cell_size / res(raster_file)[2]), 
                                 fun = mean, cores = num_workers)  # Use mean function for aggregation
  
  # Resample to the common extent
  aggregated_raster_resampled <- resample_to_common_extent(aggregated_raster, common_raster)
  
  # Store the aggregated and resampled raster with the same name
  aggregated_rasters[[raster_name]] <- aggregated_raster_resampled
}
EndTime = Sys.time()

print(EndTime - StartTime)
# Check names of the aggregated rasters
print(names(aggregated_rasters))

#stack the aggregated rasters
stacked_rasters <- rast(aggregated_rasters)

# ##Write to disk
# out_direct
# # Write all aggregated rasters to disk
# lapply(names(aggregated_rasters), function(raster_name) {
#   write_raster_disk(aggregated_rasters[[raster_name]], out_direct)
# })
# 
# aggregated_rasters


###############################################################################
##Read in vector sample points
vect_file = list.files(vect_direct, pattern = "\\.gpkg$", full.names = T)
val_points =vect(vect_file)
plot(stacked_rasters)
plot(val_points, add = T)

# Assuming val_points is already loaded and is a SpatVector
# val_points$Vegetation <- ifelse(val_points$Random_Forest == 2, 1, 0)

# Verify the changes
summary(val_points)

#Extract values from the stacked rasters at the points defined in val_points
extracted_values <- as.data.frame(extract(stacked_rasters, val_points))
summary(extracted_values)

##Combine extracted values with val_points, make sure "Bareground is first column"
#Set Bareground first
##Change to Veg or Random Forest or Bareground
combined_data <- val_points[c("Random_Forest")]
# Add the extracted values
combined_data <- cbind(combined_data, extracted_values)
combined_data = combined_data[,-2]
print(head(combined_data))

combined_data$Random_Forest = as.factor(combined_data$Random_Forest)

## Prepare the data for Random Forest
# Make sure to reference Bareground as the target variable
set.seed(123)
rf_model <- randomForest(Random_Forest ~ ., data = combined_data, importance = TRUE,ntree = 1000,
                         mtry = sqrt(ncol(combined_data) - 1),
                         na.action = na.omit )

print(rf_model)

## Evaluate Model Performance
# Make predictions on the training data
predictions_df <- predict(rf_model, combined_data)

# Create a confusion matrix (to evaluate performance)
confusion_matrix <- table(combined_data$Random_Forest, predictions_df)

# Print the confusion matrix
print(confusion_matrix)

# Calculate accuracy
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
cat("Accuracy:", accuracy, "\n")

# Check variable importance
importance(rf_model)
varImpPlot(rf_model)
gc()


###############################################################################
##predict onto raster
# Extract raster values to a data frame
raster_values <- as.data.frame(terra::values(stacked_rasters))

# Step 3: Use the model to predict on raster values
set.seed(456)
predictions_rast <- predict(rf_model, raster_values)

# Step 4: Convert predictions to a SpatRaster
# Create an empty SpatRaster for predictions with the same dimensions as stacked_rasters
predicted_raster <- rast(nrows = nrow(stacked_rasters), 
                         ncols = ncol(stacked_rasters), 
                         ext = ext(stacked_rasters))

# Fill the predicted raster with the prediction values
# Ensure to match the number of values in the prediction output to those in raster
predicted_raster[] <- predictions_rast

# Step 5: Set the same crs (coordinate reference system) as the original raster if necessary
crs(predicted_raster) <- crs(stacked_rasters)

# Step 6: Save the predicted raster to a file
writeRaster(predicted_raster, "predicted_RF_classes_original_Classes.tif", overwrite = TRUE)

# Optional: Plot the predicted raster
plot(predicted_raster, main = "Four Class Model ")
gc()
