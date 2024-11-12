# Load the saved RandomForest model
loaded_rf_model <- readRDS("./models/saved_random_forest_model.rds")

#* @apiTitle Used Vehicle Price Prediction API

#* @apiDescription This API predicts the selling price of used vehicles based on km_driven and ex_showroom_price.

#* @param km_driven Number of kilometers driven by the vehicle
#* @param ex_showroom_price The ex-showroom price of the vehicle
#* @get /predict_price

predict_price <- 
  function(km_driven, ex_showroom_price) {
    
    # Convert the parameters to numeric in case they are passed as characters
    km_driven <- as.numeric(km_driven)
    ex_showroom_price <- as.numeric(ex_showroom_price)
    
    # Create a data frame using the input parameters
    to_be_predicted <- data.frame(
      km_driven = km_driven,
      ex_showroom_price = ex_showroom_price
    )
    
    # Use the loaded model to make predictions
    prediction <- predict(loaded_rf_model, newdata = to_be_predicted)
    
    # Return the predicted selling price
    return(list(predicted_selling_price = prediction))
  }
