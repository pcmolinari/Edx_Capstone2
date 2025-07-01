## ----setup, include=FALSE------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ----read-and-preview, echo=TRUE-----------------------------------------------------------------
# Read the file
data <- read.csv("https://raw.githubusercontent.com/pcmolinari/Edx_Capstone2/main/data/open-meteo-52.13N4.31E7m.csv", sep = ",", header = TRUE, skip=3)

# Display first 5 records to inspect the data read, headers
knitr::kable(head(data, 5), caption = "First 5 Records of My Data")



## ----install_packages_silent, include=TRUE, message=FALSE, warning=FALSE-------------------------
quiet_load <- function(pkg) {
  if (!suppressMessages(require(pkg, character.only = TRUE))) {
    suppressMessages(suppressWarnings(
      install.packages(pkg, repos = "http://cran.us.r-project.org")
    ))
    suppressMessages(require(pkg, character.only = TRUE))
  }
}

# List of packages
packages <- c("tidyverse", "caret", "data.table", "lubridate", "stringr",
              "ggplot2", "knitr", "kableExtra", "scales", "dplyr", "lubridate",
              "forecast", "reshape2", "RColorBrewer", "Metrics", "glue", "prophet",
              "xgboost", "scales", "DALEX")

# Silently install and load
invisible(lapply(packages, quiet_load))


## ----rename_columns, echo=TRUE-------------------------------------------------------------------
# Rename the columns
colnames(data) <- c("time", "temperature", "humidity", "rain_mm", "snow_fall", "precipitation_mm", "cloud_cover")
knitr::kable(head(data, 5), caption = "First 5 Records of My Data")



## ----check-missing-values, message=FALSE, warning=FALSE------------------------------------------
# Summarize NA and blank string counts for each column
missing_summary <- data.frame(
  Column = names(data),
  NA_Count = sapply(data, function(col) sum(is.na(col))),
  Blank_Count = sapply(data, function(col) sum(col == "", na.rm = TRUE))
)

# Display the result
kable(missing_summary, caption = "Missing and Blank Value Check")



## ----enrich_time_date, message=FALSE, warning=FALSE----------------------------------------------
## to enrich dinamics some transofrmations
# I make the time column in POSIXct format
data$time <- ymd_hm(data$time)  

# Add 'date' and 'clock_time' columns
data <- data %>%
  mutate(
    date = as.Date(time),              # just the date part
    clock_time = format(time, "%H:%M") # formatted time string, e.g. "14:30"
  )
knitr::kable(head(data, 5), caption = "First 5 Records of My Data")


## ----beautiful-summary, message=FALSE, warning=FALSE---------------------------------------------
# Create summary
summary_df <- summary(data)




## ----TS_lineplot, message=FALSE, warning=FALSE---------------------------------------------------
### time series line plot
library(ggplot2)

ggplot(data, aes(x = time, y = temperature)) +
  geom_line(color = "steelblue") +
  labs(title = "Temperature Over Time", x = "Time", y = "Temperature")



## ----decomposition, message=FALSE, warning=FALSE-------------------------------------------------
#Decomposition Plot
#Breaks the time series into trend, seasonal, and residual components:
library(forecast)

temp_ts <- ts(data$temperature, frequency = 24 * 365)  # hourly data, yearly seasonality
decomp <- stl(temp_ts, s.window = "periodic")
plot(decomp)


## ----histogram-kde-temp, message=FALSE, warning=FALSE--------------------------------------------
# Load necessary libraries
library(ggplot2)
library(dplyr)

#Histogram
ggplot(data, aes(x = temperature)) +
  geom_histogram(aes(y = ..density..), 
                 binwidth = 1, 
                 fill = "#69b3a2", 
                 color = "white", 
                 alpha = 0.7) +
  geom_density(color = "#1c3faa", size = 1.2) +
  labs(
    title = " Distribution of Daily Temperatures",
    x = "Temperature (°C)",
    y = "Density"
  ) +
  theme_minimal()


## ----correlation-heatmap, message=FALSE, warning=FALSE-------------------------------------------

# Select only numeric columns for correlation
numeric_data <- data %>%
  select(where(is.numeric))

# Compute correlation matrix
cor_matrix <- round(cor(numeric_data, use = "complete.obs"), 2)

# Convert to long format for ggplot
melted_cor <- melt(cor_matrix)

# Plot heatmap
ggplot(data = melted_cor, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "#67a9cf", mid = "white", high = "#ef8a62", 
                       midpoint = 0, limit = c(-1, 1), space = "Lab", 
                       name = "Correlation") +
  geom_text(aes(label = value), color = "black", size = 3) +
  theme_minimal() +
  labs(title = "Heatmap of Variable Correlations",
       x = "", y = "") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))


## ----acf-temperature, message=FALSE, warning=FALSE-----------------------------------------------
# Convert time column to proper DateTime if not already
data$time <- as.POSIXct(data$time)


# We'll use the full time series here (adjust if aggregating):
ts_temp <- ts(data$temperature, frequency = 24)  # 24 if hourly, 1 if daily

# Plot autocorrelation
acf(ts_temp, lag.max = 30, main = "📈 ACF: Autocorrelation of Temperature")


## ----tst_train_Split, message=FALSE, warning=FALSE-----------------------------------------------
train <- data %>% filter(year(date) != 2024)

test  <- data %>% filter(year(date) == 2024)
## to enrich dinamics some transofrmations

train <- train %>%
  mutate(hour = hour(hm(clock_time)),
         day = yday(date))

test <- test %>%
  mutate(hour = hour(hm(clock_time)),
         day = yday(date))

h <- nrow(test)
actual <- test$temperature


## ----naivef, message=FALSE, warning=FALSE--------------------------------------------------------



naive_model <- naive(ts(train$temperature), h = h)
pred_naive <- as.numeric(naive_model$mean)
rmse_naive <- Metrics::rmse(actual, pred_naive)

cat(glue("RMSE - Naive Forecast: {round(rmse_naive, 2)}\n\n"))


## ----naives, message=FALSE, warning=FALSE--------------------------------------------------------
snaive_model <- snaive(ts(train$temperature, frequency = 24), h = h)
pred_snaive <- as.numeric(snaive_model$mean)
rmse_snaive <- Metrics::rmse(actual, pred_snaive)

cat(glue("RMSE - Seasonal Naive: {round(rmse_snaive, 2)}\n\n"))


## ----etsm, message=FALSE, warning=FALSE----------------------------------------------------------
ets_model <- ets(ts(train$temperature, frequency = 24))
pred_ets <- forecast(ets_model, h = h)$mean
rmse_ets <- Metrics::rmse(actual, as.numeric(pred_ets))

cat(glue("RMSE - ETS Model: {round(rmse_ets, 2)}\n\n"))


## ----arimam, message=FALSE, warning=FALSE--------------------------------------------------------
arima_model <- auto.arima(train$temperature)
pred_arima <- forecast(arima_model, h = h)$mean
rmse_arima <- Metrics::rmse(actual, as.numeric(pred_arima))

cat(glue("RMSE - ARIMA Model: {round(rmse_arima, 2)}\n\n"))


## ----arimaxa, message=FALSE, warning=FALSE-------------------------------------------------------
xreg_train <- as.matrix(train[, c("humidity", "cloud_cover")])
xreg_test <- as.matrix(test[, c("humidity", "cloud_cover")])
arimax_model <- auto.arima(train$temperature, xreg = xreg_train)
pred_arimax <- forecast(arimax_model, xreg = xreg_test)$mean
rmse_arimax <- Metrics::rmse(actual, as.numeric(pred_arimax))

cat(glue("RMSE - ARIMAX Model Automatic: {round(rmse_arimax, 2)}\n\n"))


## ----arimaxmm, message=FALSE, warning=FALSE------------------------------------------------------
# Define the order: (p = 5, d = 0, q = 5)
arimax_model <- Arima(train$temperature, order = c(5, 0, 5), xreg = xreg_train)
# Forecast using future xreg values
pred_arimax <- forecast(arimax_model, xreg = xreg_test)$mean
# Calculate RMSE
rmse_arimax <- Metrics::rmse(test$temperature, as.numeric(pred_arimax))

cat(glue("🔁 RMSE - ARIMAX Model Manual: {round(rmse_arimax, 2)}\n\n"))


## ----prophetm, message=FALSE, warning=FALSE------------------------------------------------------
# Rename required columns
df_prophet <- train %>%
  mutate(ds = as.POSIXct(paste(date, clock_time)),  # combine date + clock_time
         y = temperature) %>%
  select(ds, y)

model <- prophet(df_prophet)
future <- test %>%
  mutate(ds = as.POSIXct(paste(date, clock_time))) %>%
  select(ds)
## generate forecast
forecast <- predict(model, future)
predicted_prophet <- forecast$yhat
### RMSE calculation
rmse_prophet <- rmse(test$temperature, predicted_prophet)
print(rmse_prophet)

cat(glue("RMSE - Prophet: {round(rmse_prophet, 2)}\n\n"))


## ----xgboostmodel, message=FALSE, warning=FALSE--------------------------------------------------
# Prepare data
X_train <- as.matrix(train[, c("hour", "day", "humidity", "cloud_cover")])
X_test  <- as.matrix(test[,  c("hour", "day", "humidity", "cloud_cover")])
y_train <- train$temperature
dtrain <- xgb.DMatrix(data = X_train, label = y_train)
dtest  <- xgb.DMatrix(data = X_test)
xgb_model <- xgboost(data = dtrain, nrounds = 100, objective = "reg:squarederror", verbose = 0)
xgb_pred <- predict(xgb_model, newdata = dtest)
rmse_xgb <- Metrics::rmse(test$temperature, xgb_pred)

cat(glue("RMSE - XGBoost : {round(rmse_xgb, 2)}\n\n"))


## ----nnmodel, message=FALSE, warning=FALSE-------------------------------------------------------
# Step 1: Prepare features
# Choose top correlated features (e.g., humidity and cloud_cover)
xreg_train <- scale(train[, c("humidity", "cloud_cover")])
xreg_test  <- scale(test[, c("humidity", "cloud_cover")],
                    center = attr(xreg_train, "scaled:center"),
                    scale  = attr(xreg_train, "scaled:scale"))

# Step 2: Build Neural Network
nnet_model <- nnetar(
  y = train$temperature,
  xreg = xreg_train,
  p = 12,       # use 12 lags (~half-day if hourly data)
  size = 3,     # 3 hidden nodes to avoid overfitting
  repeats = 5   # increase stability by averaging multiple runs
)

# Step 3: Forecast
forecast_nnet <- forecast(nnet_model, xreg = xreg_test, h = nrow(test))
# Step 4: Evaluate
rmse_nnet <- rmse(test$temperature, as.numeric(forecast_nnet$mean))
cat(glue("RMSE - Neural Network : {round(rmse_nnet, 2)}\n\n"))



## ----xgb_peeking, message=FALSE, warning=FALSE---------------------------------------------------
##### View feature imporantce of XG BOOST
importance_matrix <- xgb.importance(model = xgb_model)
print(importance_matrix)

# Plot it
xgb.plot.importance(importance_matrix, top_n = 10, rel_to_first = TRUE)


#Fature importance
importance_matrix <- xgb.importance(feature_names = colnames(X_train), model = xgb_model)
xgb.plot.importance(importance_matrix, top_n = 10)


#Gain shows the average improvement in accuracy brought by splits using that feature.
#Cover indicates how many samples the feature split covered.
#Frequency counts how often the feature was used in trees.


### Partially dependence plots
explainer <- explain(model = xgb_model, data = X_train, y = y_train)
pdp <- model_profile(explainer, variables = c("humidity", "cloud_cover", "hour"))
plot(pdp)

### I will look at the trees

xgb.plot.tree(model = xgb_model, trees = 0)  # shows Tree 0 visually

xgb_model_text <- xgb.dump(xgb_model, with_stats = TRUE)
cat(xgb_model_text[1:20], sep = "\n")  # look at the root decision logic

### Finally, I Will train a surrogate model like a simple linear model to mimic it (because it is easier to explain)
### This gives me interpretable coefficients per variable— as a human-friendly proxy.

xgb_pred_train <- predict(xgb_model, newdata = X_train)
lm_surrogate <- lm(xgb_pred_train ~ X_train)
summary(lm_surrogate)



### lest see if surrogate model is really representative of the XGBOOS it is trying to mimic
### Now that I’ve built a linear model (lm_surrogate) to approximate my XGBoost predictions, here’s how to ## ##compare those predictions to my actual observed values:


### get surrogate predictions
surrogate_pred <- predict(lm_surrogate, newdata = as.data.frame(X_train))

### compare real to observerd targets
plot(y_train, surrogate_pred,
     xlab = "Actual Temperature",
     ylab = "Surrogate Prediction",
     main = "Surrogate Model vs. Actual",
     col = "steelblue", pch = 16)
abline(0, 1, col = "red", lwd = 2)  # perfect fit line


### evaluate rmse and r squared for both
library(Metrics)

rmse_surrogate <- rmse(y_train, surrogate_pred)
r2_surrogate <- summary(lm(y_train ~ surrogate_pred))$r.squared

rmse_surrogate
r2_surrogate

#This indicates the surogate is very poor to use to explain the model

###Finally I Want to compare this side-by-side with XGBoost’s accuracy or visualize residuals?

### First i generate teh predicitions
# XGBoost predictions (already available)
xgb_pred_train <- predict(xgb_model, newdata = X_train)

# Surrogate model predictions
surrogate_pred <- predict(lm_surrogate, newdata = as.data.frame(X_train))


# I compare teh residuals
resid_xgb <- y_train - xgb_pred_train
resid_surrogate <- y_train - surrogate_pred


## And i plot the residuals side by side
par(mfrow = c(1, 2))

plot(resid_xgb, main = "XGBoost Residuals", ylab = "Residual", xlab = "Index",
     col = "steelblue", pch = 16)
abline(h = 0, col = "red")

plot(resid_surrogate, main = "Surrogate LM Residuals", ylab = "Residual", xlab = "Index",
     col = "darkorange", pch = 16)
abline(h = 0, col = "red")

#This will show how “off” each model is across my training data—tight clouds around 0 are what I want.

### RMSE  and r squared for both

rmse_xgb <- rmse(y_train, xgb_pred_train)
rmse_surrogate <- rmse(y_train, surrogate_pred)

r2_xgb <- summary(lm(y_train ~ xgb_pred_train))$r.squared
r2_surrogate <- summary(lm(y_train ~ surrogate_pred))$r.squared

data.frame(
  Model = c("XGBoost", "Surrogate LM"),
  RMSE = c(rmse_xgb, rmse_surrogate),
  R_Squared = c(r2_xgb, r2_surrogate)
)

###Visualize actual vs predicted
par(mfrow = c(1, 2))

plot(y_train, xgb_pred_train,
     xlab = "Actual Temp", ylab = "Predicted Temp",
     main = "XGBoost", col = "steelblue", pch = 16)
abline(0, 1, col = "red", lwd = 2)

plot(y_train, surrogate_pred,
     xlab = "Actual Temp", ylab = "Predicted Temp",
     main = "Surrogate Linear Model", col = "darkorange", pch = 16)
abline(0, 1, col = "red", lwd = 2)



## ----xgb_visualize predictions, message=FALSE, warning=FALSE-------------------------------------

##### Visualize the predictions of best model - XGBoost


# Combine date and clock_time into a proper datetime
test$datetime <- as.POSIXct(paste(test$date, test$clock_time), tz = "UTC")

# Build comparison dataframe
plot_df <- data.frame(
  datetime = test$datetime,
  actual = test$temperature,
  predicted = xgb_pred
)

# Plot
ggplot(plot_df, aes(x = datetime)) +
  geom_line(aes(y = actual), color = "black", size = 1, alpha = 0.7) +
  geom_line(aes(y = predicted), color = "dodgerblue", size = 1, linetype = "dashed") +
  labs(
    title = "XGBoost Forecast vs Actual Temperature",
    x = "Datetime",
    y = "Temperature"
  ) +
  theme_minimal()



## This clearly shows that XGBOOST is the best model I can use to predict


## ------------------------------------------------------------------------------------------------


