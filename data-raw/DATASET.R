# Load necessary libraries
library(raster)
library(sf)

# Read raster files
raster_2010 <- raster("D:/MSc_EAGLE/RPackage/chngTrackR/data_raw/X0063_Y0029_2010_Classification.tif")
raster_2020 <- raster("D:/MSc_EAGLE/RPackage/chngTrackR/data_raw/X0063_Y0029_2020_Classification.tif")

# Stack rasters if they have multiple bands
stack_2010 <- stack(raster_2010)
stack_2020 <- stack(raster_2020)

# Read validation points
validation_points <- read.csv("D:/MSc_EAGLE/RPackage/chngTrackR/data_raw/Combined_X0063_Y0029_2020_Validations.csv")

# Save processed data into the package
usethis::use_data(stack_2010, stack_2020, validation_points, overwrite = TRUE)
