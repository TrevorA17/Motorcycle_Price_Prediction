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
