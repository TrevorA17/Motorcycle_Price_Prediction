# Saving the model
saveRDS(random_forest_model, "./models/saved_random_forest_model.rds")

# Load the saved model
loaded_rf_model <- readRDS("./models/saved_random_forest_model.rds")

# New data for prediction (adjust the values based on your input)
new_data <- data.frame(
  km_driven = 15000,
  ex_showroom_price = 100000
)

# Use the loaded model to make predictions
predictions_loaded_model <- predict(loaded_rf_model, newdata = new_data)

# Print predictions
print(predictions_loaded_model)
