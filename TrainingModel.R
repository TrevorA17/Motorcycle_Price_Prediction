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

install.packages("caret")  # For cross-validation and data splitting
library(caret)

# Set seed for reproducibility
set.seed(123)

# Split the data into training (70%) and testing (30%) sets
train_index <- createDataPartition(vehicle_data$selling_price, p = 0.7, list = FALSE)
train_data <- vehicle_data[train_index, ]
test_data <- vehicle_data[-train_index, ]

# Check dimensions of the splits
cat("Training set dimensions:", dim(train_data), "\n")
cat("Testing set dimensions:", dim(test_data), "\n")

# Bootstrapping with 100 samples
boot_samples <- 100
boot_results <- list()

for (i in 1:boot_samples) {
  # Resample the training data with replacement
  boot_sample <- train_data[sample(1:nrow(train_data), replace = TRUE), ]
  
  # Fit a model to the bootstrap sample (e.g., linear regression)
  boot_model <- lm(selling_price ~ km_driven + ex_showroom_price, data = boot_sample)
  
  # Store the model results
  boot_results[[i]] <- summary(boot_model)$coefficients
}

# Display results from a sample bootstrapped model
print(boot_results[[1]])

# K-Fold Cross-Validation with 5 folds
train_control <- trainControl(method = "cv", number = 5)
model_cv <- train(selling_price ~ km_driven + ex_showroom_price,
                  data = train_data,
                  method = "lm",
                  trControl = train_control)

# Summary of cross-validation results
print(model_cv)
