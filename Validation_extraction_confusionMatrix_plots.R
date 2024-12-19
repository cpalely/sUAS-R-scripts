##Read in list of EXGI rasters, random point vector file, extract raster values to vector and write to disk as geojson or geopackage
library(terra)    # For raster operations
library(sf)       # For handling spatial data
library(geojsonio) # For writing GeoJSON


##Define folder path
getwd()

##Folders that need to be changed
Site = "BB"

val_direct = paste0("./Spectral/Normalized_ExGI/Validation/",Site)

##Read files in folder
file_paths = list.files(val_direct, pattern = "\\.tif$", full.names = T)
print(file_paths)
raster_names = basename(file_paths)

##read in rasters using for loop
#Empty list
raster_list = list()

##Read in vector sample points
vect_file = list.files(val_direct, pattern = "\\.gpkg$", full.names = T)
val_points =vect(vect_file)

#Loop to read
for(file_path in file_paths){
  raster = rast(file_path)
  raster_list[[basename(file_path)]] = raster
  plot(raster)
  plot(val_points, add = T)
  rm(raster)
}

print(names(raster_list))

# Extract raster values to val_points
for (i in seq_along(raster_list)) {
  # Extract raster values for each raster
  print("Extracting values from:")
  print(names(raster_list[[i]]))
  values = extract(raster_list[[i]], val_points)
  
  # Check the extracted values
  if (is.null(values) || nrow(values) == 0) {
    warning(paste("No values extracted from raster", i, "for the given vector points."))
    next
  } else {
    # Print the values for debugging
    print(values)
  }
  
  # Create a new column name based on the raster file name
  col_name = paste0("Threshold_", i)
  
  # Add extracted values as a new column in val_points
  val_points[[col_name]] = values$BB_62m_Ortho
}

# Optionally print the updated val_points to check the result
print(summary((val_points)))


#################################################################################
###Create confusion matrix

# Load dplyr
library(dplyr)

#create df of points for using dplyr
val_points.df = as.data.frame(val_points)

# Create confusion matrices for each index column using dplyr
index_cols <- grep("^Threshold_", names(val_points.df), value = TRUE)

# Initialize confusion summary
confusion_summary <- data.frame(Index = character(), TP = integer(), FP = integer(), FN = integer(), TN = integer(), stringsAsFactors = FALSE)

# Loop through each Index column
for (index_col in index_cols) {
  
  # Calculate True Positive, False Positive, False Negative, True Negative
  TP <- sum(val_points.df[[index_col]] == 1 & val_points.df$Vegetation == 1, na.rm = TRUE)
  FP <- sum(val_points.df[[index_col]] == 1 & val_points.df$Vegetation == 0, na.rm = TRUE)
  FN <- sum(val_points.df[[index_col]] == 0 & val_points.df$Vegetation == 1, na.rm = TRUE)
  TN <- sum(val_points.df[[index_col]] == 0 & val_points.df$Vegetation == 0, na.rm = TRUE)
  
  # Append results to summary
  confusion_summary <- rbind(confusion_summary, data.frame(Index = index_col, TP = TP, FP = FP, FN = FN, TN = TN))
}

# Print confusion summary
print(confusion_summary)

# Check how many rows meet conditions for each Index column
for (index_col in index_cols) {
  cat("\nRows for FP (Vegetation == 0 &", index_col, "== 1):\n")
  print(val_points.df[val_points.df$Vegetation == 0 & val_points.df[[index_col]] == 1, ])
  cat("\nRows for TP (Vegetation == 1 &", index_col, "== 1):\n")
  print(val_points.df[val_points.df$Vegetation == 1 & val_points.df[[index_col]] == 1, ])
  cat("\nRows for FN (Vegetation == 1 &", index_col, "== 0):\n")
  print(val_points.df[val_points.df$Vegetation == 1 & val_points.df[[index_col]] == 0, ])
  cat("\nRows for TN (Vegetation == 0 &", index_col, "== 0):\n")
  print(val_points.df[val_points.df$Vegetation == 0 & val_points.df[[index_col]] == 0, ])
}
################################################################################
################################################################################
##Heat map
# Load necessary libraries
library(ggplot2)
library(wesanderson)
library(reshape2)

# Assuming confusion_summary is already defined, we convert it to long format
confusion_long <- melt(confusion_summary, id.vars = "Index")

# Extract Zissou1 colors
zissou_colors <- wes_palette("Zissou1")
# Create the heatmap with Zissou1 colors
ggplot(confusion_long, aes(x = variable, y = Index, fill = value)) +
  geom_tile() +
  scale_fill_gradientn(colors = zissou_colors) +  # Use scale_fill_gradientn for multiple colors
  labs(title = "Confusion Matrix Heatmap",
       x = "Outcome",
       y = "Threshold Level",
       fill = "Count") +
  theme_minimal()

###############################################################################
##Bar Plot
# Extract Zissou1 colors
fox_colors <- wes_palette("FantasticFox1")[c(4, 2, 3, 5)]

# Plotting the bar plot using Zissou1 colors
ggplot(confusion_long, aes(x = Index, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "Confusion Matrix Counts by threshold",
       x = "Index Level",
       y = "Count",
       fill = "Outcome") +
  theme_minimal() +
  scale_fill_manual(values = fox_colors)  # Applying Zissou1 colors


#################################################################################
# Calculate additional performance metrics
# Create a function to calculate metrics for each class
calculate_metrics <- function(df) {
  # Initialize metrics dataframe to store results
  metrics <- data.frame(Index = df$Index,
                        Accuracy = numeric(nrow(df)),
                        Precision = numeric(nrow(df)),
                        Recall = numeric(nrow(df)),
                        F1_Score = numeric(nrow(df)))
                        
  
  for (i in 1:nrow(df)) {
    TP <- df$TP[i]
    FP <- df$FP[i]
    FN <- df$FN[i]
    TN <- df$TN[i]
    
    # Calculate precision, recall, F1 score for the current index
    precision <- ifelse(TP + FP == 0, 0, TP / (TP + FP))
    recall <- ifelse(TP + FN == 0, 0, TP / (TP + FN))
    f1_score <- ifelse(precision + recall == 0, 0, 2 * (precision * recall) / (precision + recall))
    
    # Calculate accuracy for the current index
    accuracy <- (TP + TN) / (TP + FP + FN + TN)
    
    # Store the calculated metrics
    metrics$Precision[i] <- precision
    metrics$Recall[i] <- recall
    metrics$F1_Score[i] <- f1_score
    metrics$Accuracy[i] <- accuracy  # Store accuracy for each index
  }
  
  return(metrics)
}

# Calculate metrics for the confusion matrix
metrics_result <- calculate_metrics(confusion_summary)

# Print results
print(metrics_result)

metrics_long <- melt(metrics_result, id.vars = "Index", variable.name = "Metric", value.name = "Value")
print(metrics_long)

# Plotting the metrics
ggplot(metrics_long, aes(x = Index, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "Performance Metrics by threshold level",
       x = "Index Level",
       y = "Score",
       fill = "Metric") +
  theme_minimal() +
  scale_fill_manual(values = fox_colors)  # You can choose a palette or colors you like



