# ------------------------------------------------------------
# ONSPD CSV Filter and Export to Parquet
# ------------------------------------------------------------
# This script:
# - Loads a large ONSPD CSV file
# - Filters rows where 'doterm' is empty or NA
# - Keeps only 'pcd' and 'oa21' columns (renaming 'oa21' to 'oa21cd')
# - Removes spaces from the 'pcd' values
# - Outputs the result as a Parquet file to a specified directory
# 
# Version: 0.0.7
# ------------------------------------------------------------

# Load required libraries
library(data.table)
library(arrow)

# Set file paths
input_file <- "/Users/charlesbrewer/Downloads/ONSPD_MAY_2025 2/Data/ONSPD_MAY_2025_UK.csv"
output_dir <- "/Users/charlesbrewer/Downloads/ONSPD_MAY2025_rev_072025"
output_file <- file.path(output_dir, "ONSPD_MAY_2025_filtered.parquet")

# Create output directory if it doesn't exist
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Read only necessary columns
cols_to_read <- c("pcd", "oa21", "doterm")

message("Reading file...")
dt <- fread(input_file, select = cols_to_read)

# Filter where doterm is empty or NA
message("Filtering rows...")
dt_filtered <- dt[is.na(doterm) | doterm == ""]

# Remove spaces in pcd
dt_filtered[, pcd := gsub(" ", "", pcd)]

# Rename oa21 to oa21cd
setnames(dt_filtered, "oa21", "oa21cd")

# Drop doterm
dt_filtered[, doterm := NULL]

# Write to .parquet using arrow
message("Writing to Parquet: ", output_file)
write_parquet(dt_filtered, output_file)

message("✅ Done. Parquet file saved at: ", output_file)