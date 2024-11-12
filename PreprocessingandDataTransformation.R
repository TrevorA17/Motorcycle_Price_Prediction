# Load dataset with specified column classes
vehicle_data <- read.csv("data/BIKE_DETAILS.csv", colClasses = c(
  name = "character",
  year = "integer",
  seller_type = "factor",
  owner = "factor",
  km_driven = "integer",
  ex_showroom_price = "numeric",
  selling_price = "numeric"
))

# Verify the structure to ensure column types are correct
str(vehicle_data)

# Display the first few rows to confirm data loading
head(vehicle_data)
View(vehicle_data)

# Check for missing values in each column
missing_values <- sapply(vehicle_data, function(x) sum(is.na(x)))
print(missing_values)

# Summary of total missing values in the dataset
total_missing <- sum(missing_values)
cat("Total missing values in the dataset:", total_missing, "\n")

# Display columns with missing values, if any
if (total_missing > 0) {
  cat("Columns with missing values:\n")
  print(missing_values[missing_values > 0])
} else {
  cat("No missing values found in the dataset.\n")
}

# Impute missing values in ex_showroom_price with the mean
vehicle_data$ex_showroom_price[is.na(vehicle_data$ex_showroom_price)] <- mean(vehicle_data$ex_showroom_price, na.rm = TRUE)

View(vehicle_data)

# Remove the 'X' column from the dataset
vehicle_data <- vehicle_data[, !(names(vehicle_data) %in% "X")]

# Verify the column has been removed
print(names(vehicle_data))

View(vehicle_data)

