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

# Load necessary library for summary statistics
library(dplyr)

# Frequency of seller type
seller_type_freq <- table(vehicle_data$seller_type)
print(seller_type_freq)

# Frequency of owner type
owner_freq <- table(vehicle_data$owner)
print(owner_freq)

# Mean, median, and mode for numerical columns

# Mean
mean_year <- mean(vehicle_data$year, na.rm = TRUE)
mean_km_driven <- mean(vehicle_data$km_driven, na.rm = TRUE)
mean_ex_showroom_price <- mean(vehicle_data$ex_showroom_price, na.rm = TRUE)
mean_selling_price <- mean(vehicle_data$selling_price, na.rm = TRUE)

# Median
median_year <- median(vehicle_data$year, na.rm = TRUE)
median_km_driven <- median(vehicle_data$km_driven, na.rm = TRUE)
median_ex_showroom_price <- median(vehicle_data$ex_showroom_price, na.rm = TRUE)
median_selling_price <- median(vehicle_data$selling_price, na.rm = TRUE)

# Mode function
get_mode <- function(v) {
  uniq_v <- unique(v)
  uniq_v[which.max(tabulate(match(v, uniq_v)))]
}

mode_owner <- get_mode(vehicle_data$owner)
mode_seller_type <- get_mode(vehicle_data$seller_type)

print(list(
  mean_year = mean_year, mean_km_driven = mean_km_driven,
  mean_ex_showroom_price = mean_ex_showroom_price, mean_selling_price = mean_selling_price,
  median_year = median_year, median_km_driven = median_km_driven,
  median_ex_showroom_price = median_ex_showroom_price, median_selling_price = median_selling_price,
  mode_owner = mode_owner, mode_seller_type = mode_seller_type
))

# Variance and standard deviation
variance_km_driven <- var(vehicle_data$km_driven, na.rm = TRUE)
sd_km_driven <- sd(vehicle_data$km_driven, na.rm = TRUE)

variance_ex_showroom_price <- var(vehicle_data$ex_showroom_price, na.rm = TRUE)
sd_ex_showroom_price <- sd(vehicle_data$ex_showroom_price, na.rm = TRUE)

variance_selling_price <- var(vehicle_data$selling_price, na.rm = TRUE)
sd_selling_price <- sd(vehicle_data$selling_price, na.rm = TRUE)

# Range
range_year <- range(vehicle_data$year, na.rm = TRUE)
range_km_driven <- range(vehicle_data$km_driven, na.rm = TRUE)
range_ex_showroom_price <- range(vehicle_data$ex_showroom_price, na.rm = TRUE)
range_selling_price <- range(vehicle_data$selling_price, na.rm = TRUE)

# Range
range_year <- range(vehicle_data$year, na.rm = TRUE)
range_km_driven <- range(vehicle_data$km_driven, na.rm = TRUE)
range_ex_showroom_price <- range(vehicle_data$ex_showroom_price, na.rm = TRUE)
range_selling_price <- range(vehicle_data$selling_price, na.rm = TRUE)

# Quantiles
quantiles_km_driven <- quantile(vehicle_data$km_driven, na.rm = TRUE)
quantiles_ex_showroom_price <- quantile(vehicle_data$ex_showroom_price, na.rm = TRUE)
quantiles_selling_price <- quantile(vehicle_data$selling_price, na.rm = TRUE)

# Interquartile range (IQR)
iqr_km_driven <- IQR(vehicle_data$km_driven, na.rm = TRUE)
iqr_ex_showroom_price <- IQR(vehicle_data$ex_showroom_price, na.rm = TRUE)
iqr_selling_price <- IQR(vehicle_data$selling_price, na.rm = TRUE)

print(list(
  variance_km_driven = variance_km_driven, sd_km_driven = sd_km_driven,
  variance_ex_showroom_price = variance_ex_showroom_price, sd_ex_showroom_price = sd_ex_showroom_price,
  variance_selling_price = variance_selling_price, sd_selling_price = sd_selling_price,
  range_year = range_year, range_km_driven = range_km_driven,
  range_ex_showroom_price = range_ex_showroom_price, range_selling_price = range_selling_price,
  quantiles_km_driven = quantiles_km_driven, quantiles_ex_showroom_price = quantiles_ex_showroom_price,
  quantiles_selling_price = quantiles_selling_price,
  iqr_km_driven = iqr_km_driven, iqr_ex_showroom_price = iqr_ex_showroom_price,
  iqr_selling_price = iqr_selling_price
))

# Correlation matrix for numerical variables
cor_matrix <- cor(vehicle_data %>% select(year, km_driven, ex_showroom_price, selling_price), use = "complete.obs")
print(cor_matrix)

# Covariance matrix
cov_matrix <- cov(vehicle_data %>% select(year, km_driven, ex_showroom_price, selling_price), use = "complete.obs")
print(cov_matrix)

## ANOVA test for selling price across different seller types
anova_seller_type <- aov(selling_price ~ seller_type, data = vehicle_data)
summary(anova_seller_type)

# ANOVA test for selling price across different ownership categories
anova_owner <- aov(selling_price ~ owner, data = vehicle_data)
summary(anova_owner)

# Perform Tukey's HSD post-hoc test if ANOVA is significant
tukey_seller_type <- TukeyHSD(anova_seller_type)
print(tukey_seller_type)

tukey_owner <- TukeyHSD(anova_owner)
print(tukey_owner)

install.packages("ggplot2")
library(ggplot2)

# Histogram of selling prices
ggplot(vehicle_data, aes(x = selling_price)) +
  geom_histogram(binwidth = 10000, fill = "steelblue", color = "black") +
  labs(title = "Distribution of Selling Prices", x = "Selling Price", y = "Frequency")

# Bar plot for seller type
ggplot(vehicle_data, aes(x = seller_type)) +
  geom_bar(fill = "purple") +
  labs(title = "Frequency of Seller Types", x = "Seller Type", y = "Count")

# Bar plot for owner type
ggplot(vehicle_data, aes(x = owner)) +
  geom_bar(fill = "orange") +
  labs(title = "Frequency of Ownership Types", x = "Owner Type", y = "Count")

# Boxplot for kilometers driven
ggplot(vehicle_data, aes(y = km_driven)) +
  geom_boxplot(fill = "lightgreen") +
  labs(title = "Boxplot of Kilometers Driven", y = "Kilometers Driven")

# Scatter plot of kilometers driven vs selling price
ggplot(vehicle_data, aes(x = km_driven, y = selling_price)) +
  geom_point(color = "blue", alpha = 0.6) +
  labs(title = "Selling Price vs Kilometers Driven", x = "Kilometers Driven", y = "Selling Price")

# Boxplot of selling price by seller type
ggplot(vehicle_data, aes(x = seller_type, y = selling_price)) +
  geom_boxplot(fill = "salmon") +
  labs(title = "Selling Price by Seller Type", x = "Seller Type", y = "Selling Price")

# Boxplot of selling price by owner
ggplot(vehicle_data, aes(x = owner, y = selling_price)) +
  geom_boxplot(fill = "cyan") +
  labs(title = "Selling Price by Ownership Type", x = "Owner Type", y = "Selling Price")

# Faceted scatter plot of km driven vs selling price by seller type
ggplot(vehicle_data, aes(x = km_driven, y = selling_price)) +
  geom_point(alpha = 0.5) +
  facet_wrap(~ seller_type) +
  labs(title = "Selling Price vs Kilometers Driven by Seller Type", x = "Kilometers Driven", y = "Selling Price")

install.packages("GGally")
library(GGally)

# Pairwise plot for selected variables
ggpairs(vehicle_data, columns = c("year", "km_driven", "ex_showroom_price", "selling_price"))


