# Load necessary libraries
library(readr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(lmtest)
library(corrplot)
library(tidyr)
library(RColorBrewer)
library(knitr)
library(reshape2)
library(tseries)
library(forecast)
library(gbm)
library(mgcv)
library(Metrics)
library(caret)
library(scales)
library(prophet)
library(mgcv)
library(splines)
library(prophet)
library(stats)
library(e1071)
library(zoo)
library(BASS)
library(xts)


# Load the data
companies <- read_csv("sp500_companies.csv")
index <- read_csv("sp500_index.csv")
stocks <- read_csv("sp500_stocks.csv")
vix <- read_csv("VIX_History.csv")
bitcoin <- read_csv("Bitcoin_Historical_Data.csv")


############################################## DATA PREPARATION
# Convert Date column to Date type
index$Date <- as.Date(index$Date, format="%Y-%m-%d")
stocks$Date <- as.Date(stocks$Date, format="%Y-%m-%d")
vix$DATE <- as.Date(vix$DATE, format="%m/%d/%Y")
bitcoin$Date <- as.Date(bitcoin$Date, format="%m/%d/%Y")

# Only take data from after 2015 (that because NVIDIA value was very very low before)
start_date <- as.Date("2015-01-01")
index <- index[index$Date >= start_date, ]
stocks <- stocks[stocks$Date >= start_date, ]
vix <- vix[vix$DATE >= start_date, ]
bitcoin <- bitcoin[bitcoin$Date >= start_date, ]

          
# Rename the colnames of VIX and BTC to differentiate it from stocks
colnames(vix) <- c("Date", "OpenVix", "HighVix", "LowVix", "CloseVix")
colnames(bitcoin) <- c("Date", "CloseBTC", "OpenBTC", "HighBTC", "LowBTC", "VolBTC", "ChangeBTC")

# ################ IMPORTANT: We need to add the weekend values. Saturday and Sundays will be Friday's values
### INDEX
date_range <- seq(min(index$Date), max(index$Date), by = "day")

# Create a new data frame with all dates
index_df_full <- data.frame(Date = date_range)

# Join with original data
index_df_full <- left_join(index_df_full, index, by = "Date")

# Fill missing values (weekends) with the last non-NA value (Friday's value)
index <- index_df_full %>%
  arrange(Date) %>%
  mutate(`S&P500` = ifelse(is.na(`S&P500`), NA, as.numeric(`S&P500`))) %>%
  fill(`S&P500`)

# STOCKS
stocks <- stocks %>%
  mutate(Date = ymd(Date))

# Create a sequence of dates and a vector of unique symbols
all_dates <- seq.Date(min(stocks$Date), max(stocks$Date), by = "day")
all_symbols <- unique(stocks$Symbol)

# Create a complete grid of dates and symbols
complete_grid <- expand.grid(Date = all_dates, Symbol = all_symbols)

# Left join the original DataFrame with the complete grid
stocks <- left_join(complete_grid, stocks, by = c("Date", "Symbol"))

# Fill missing values with NA
stocks <- stocks %>%
  fill("Adj Close", Close, High, Low, Open, Volume, .direction = "down")


# VIX
# Create a complete sequence of dates
date_range <- seq(min(vix$Date), max(vix$Date), by = "day")

# Create a new data frame with all dates
vix_df_full <- data.frame(Date = date_range)

# Join with original data
vix_df_full <- left_join(vix_df_full, vix, by = "Date")

# Fill missing values (weekends) with the last non-NA value (Friday's value) for all numeric columns
vix<- vix_df_full %>%
  arrange(Date) %>%
  mutate(across(where(is.numeric), ~as.numeric(.))) %>%
  fill(where(is.numeric))

# (BTC doesn't need it)
########################

# Join the stocks and companies data to enrich stock data with sector info
stocks <- stocks %>% left_join(companies, by = "Symbol")

# Join the stocks with the Vix info
# stocks <- stocks %>% left_join(vix, by = "Date")

# Join the stocks with the BTC info
# stocks <- stocks %>% left_join(bitcoin, by = "Date")

# Check for missing values
sum(is.na(index))    # Check for missing values in index
sum(is.na(stocks))   # Check for missing values in stocks
sum(is.na(companies)) # Check for missing values in companies
sum(is.na(vix)) # Check for missing values in companies
sum(is.na(bitcoin)) # Check for missing values in companies


#Remove rows with missing values
stocks_clean <- na.omit(stocks)
companies_clean <- na.omit(companies)


# Check for missing values in cleaned data
sum(is.na(index))    # Check for missing values in index
sum(is.na(stocks_clean))   # Check for missing values in stocks cleaned
sum(is.na(companies_clean)) # Check for missing values in companies cleaned


# Plot the S&P 500 index over time
ggplot(index, aes(x = Date, y = `S&P500`)) +
  geom_line(color = "blue") +
  labs(title = "S&P 500 Index Over Time", x = "Date", y = "S&P 500 Index") +
  theme_minimal()

# Filter data for a specific stock (e.g., Apple Inc.)
AMD_data <- stocks_clean %>%
  filter(Symbol == "AMD")

# Plot the stock prices over time
ggplot(AMD_data, aes(x = Date)) +
  geom_line(aes(y = `Adj Close`, color = "Adjusted Close")) +
  geom_line(aes(y = Close, color = "Close")) +
  labs(title = "AMD Inc. Stock Prices Over Time", x = "Date", y = "Price") +
  scale_color_manual(values = c("Adjusted Close" = "red", "Close" = "green")) +
  theme_minimal()

# Aggregate stock data by sector
sector_summary <- stocks_clean %>%
  group_by(Sector, Date) %>%
  summarize(Sector_Close = mean(Close, na.rm = TRUE))

# Plot sector performance over time
ggplot(sector_summary, aes(x = Date, y = Sector_Close, color = Sector)) +
  geom_line(size = 1) +  # Increase line thickness for better visibility
  labs(
    title = "Sector-wise Performance Over Time",
    subtitle = "Average Closing Prices of S&P 500 Stocks",
    x = "Date", 
    y = "Average Closing Price (USD)"
  ) +
  scale_y_continuous(labels = scales::dollar) +  # Format y-axis as dollars
  scale_color_brewer(palette = "Set3") +  # Use colorblind-friendly palette
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),  # Center and bold title
    plot.subtitle = element_text(hjust = 0.5, size = 12),  # Add a subtitle
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    legend.title = element_blank(),  # Remove legend title
    legend.position = "right",  # Position legend to the right
    panel.grid.major = element_line(size = 0.5, color = "gray80")  # Subtle grid lines
  )
# Get tech data
tech_data <- sector_summary %>%
  filter(Sector == "Technology")

# Aggregate stock data by Industry
industry_summary <- stocks_clean %>%
  group_by(Industry, Date) %>%
  summarize(Industry_Close = mean(Close, na.rm = TRUE))

# Select 10 industries to plot
selected_industries <- c(
  "Semiconductors", "Medical Devices", "Internet Retail", 
  "Software - Application", "Biotechnology", 
  "Aerospace & Defense", "Insurance - Life", 
  "Pharmaceutical Retailers", "Capital Markets", 
  "Auto Manufacturers"
)

# Filter the dataset to only include the selected industries
industry_summary_selected <- industry_summary %>%
  filter(Industry %in% selected_industries)

# Plot sector performance over time
ggplot(industry_summary_selected, aes(x = Date, y = Industry_Close, color = Industry)) +
  geom_line(size = 1) +  # Increase line thickness for better visibility
  labs(
    title = "Performance of Selected Industries Over Time",
    subtitle = "Average Closing Prices of S&P 500 Stocks",
    x = "Date", 
    y = "Average Closing Price (USD)"
  ) +
  scale_y_continuous(labels = scales::dollar) +  # Format y-axis as dollars
  scale_color_brewer(palette = "Set3") +  # Use colorblind-friendly palette
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),  # Center and bold title
    plot.subtitle = element_text(hjust = 0.5, size = 12),  # Add a subtitle
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    panel.grid.major = element_line(size = 0.5, color = "gray80")  # Subtle grid lines
  )

# Get semic data
semic_data <- industry_summary %>%
  filter(Industry == "Semiconductors")

# Plot sector performance over time
ggplot(semic_data, aes(x = Date, y = Industry_Close)) +
  geom_line() +
  labs(title = "Semiconductor Industry Performance (Average Closing Prices)", x = "Date", y = "Avg Close Price")

# Plot VIX index over time
ggplot(vix, aes(x = Date, y = CloseVix)) +
  geom_line(color = "#1f77b4", size = 1) +  # Change line color and thickness
  labs(title = "VIX Close Price Evolution",
       x = "Date",
       y = "Close Price") +
  scale_y_continuous(labels = scales::dollar) +  # Format y-axis as dollars
  theme_minimal() +  # Use a minimal theme for a cleaner look
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        panel.grid.major = element_line(color = "gray80"),
        panel.grid.minor = element_blank())  # Customize grid lines


# Plot BTC index over time
ggplot(bitcoin, aes(x = Date, y = CloseBTC)) +
  geom_line(color = "#1f77b4", size = 1) +  # Change line color and thickness
  labs(title = "BTC Close Price Evolution",,
       x = "Date",
       y = "Close Price") +
  scale_y_continuous(labels = scales::dollar) +  # Format y-axis as dollars
  theme_minimal() +  # Use a minimal theme for a cleaner look
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        panel.grid.major = element_line(color = "gray80"),
        panel.grid.minor = element_blank())  # Customize grid lines

######################################## NVIDIA 

# Filter data for NVIDIA
NVIDIA_data <- stocks_clean %>%
  filter(Symbol == "NVDA")

NVIDIA_data <- NVIDIA_data %>% left_join(vix %>% select(Date, CloseVix), by = "Date")
NVIDIA_data <- NVIDIA_data %>% left_join(bitcoin %>% select(Date, CloseBTC), by = "Date")

# Create a dataframe for the annotations with additional events
annotations <- data.frame(
  Date = as.Date(c("2016-03-15", "2020-03-11", "2020-05-14", "2021-11-19", "2022-11-30", "2023-05-25")),
  label = c("AlphaGo", "COVID-19 Pandemic", "A100 GPU", "Stock Split", "ChatGPT Release", "$1T market cap")
)

# Match the Close prices from your data
annotations$Close <- sapply(annotations$Date, function(d) {
  NVIDIA_data$Close[which.min(abs(NVIDIA_data$Date - d))]
})

# Plot the stock prices over time with important dates
ggplot(NVIDIA_data, aes(x = Date)) +
  geom_ribbon(aes(ymin = Low, ymax = High), fill = "lightblue", alpha = 0.4) +
  geom_line(aes(y = Close, color = "Close"), size = 1) +
  geom_point(data = annotations, aes(y = Close), color = "red", size = 2) +
  geom_text(data = annotations, aes(y = Close, label = label), 
            hjust = -0.1, vjust = -0.5, size = 3, check_overlap = TRUE) +
  geom_segment(data = annotations, 
               aes(xend = Date, yend = Close, x = Date, y = Close * 1.1),
               arrow = arrow(length = unit(0.2, "cm"), ends = "first", type = "closed"),
               color = "red") +
  labs(title = "NVIDIA Stock Prices Over Time", 
       x = "Date", y = "Price") +
  scale_color_manual(values = c("Close" = "green")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10)) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(labels = scales::dollar_format())

# Add annotation for the 2021 price drop
price_drop_date <- as.Date("2021-11-19")
price_drop_price <- NVIDIA_data$Close[which.min(abs(NVIDIA_data$Date - price_drop_date))]

last_plot() +
  annotate("text", x = price_drop_date, y = price_drop_price, 
           label = "Stock Split & Crypto Decline", 
           hjust = -0.1, vjust = -0.5, size = 3, color = "blue") +
  geom_segment(aes(x = price_drop_date, y = price_drop_price * 1.1, 
                   xend = price_drop_date, yend = price_drop_price),
               arrow = arrow(length = unit(0.2, "cm"), ends = "first", type = "closed"),
               color = "blue")

# THIS WASN'T A VERY COOL PLOT HERE, BUT ITS INTERESTING TO HAVE IT TO PLOT IT BETTER IN THE PRESENTATION 

# Create the plot
scale_factor <- 10000 
ggplot(NVIDIA_data, aes(x = Date)) +
  geom_line(aes(y = Close, color = "Close"), size = 1) +
  geom_line(aes(y = CloseVix, color = "Vix"), size = 1) +
  geom_line(aes(y = CloseBTC / scale_factor, color = "BTC"), size = 1) + # scale BTC for better visualization
  scale_y_continuous(name = "Price", 
                     sec.axis = sec_axis(~ .* scale_factor, name = "BTC")) + # create secondary axis for BTC
  scale_color_manual(values = c("BTC" = "red", "Close" = "green", "Vix" = "blue")) +
  labs(title = "NVIDIA Stock Prices Over Time (with High-Low Ribbon)", x = "Date") +
  theme_minimal()

# Plot the nvidia stock prices over time 
ggplot(NVIDIA_data, aes(x = Date)) +
  geom_line(aes(y = Close), color = "green", size = 1) +
  #geom_vline(xintercept = as.Date("2015-01-01"), linetype = "dashed", color = "red", size = 0.8) +  # Add vertical line at 2015
  labs(title = "NVIDIA Stock Prices Over Time", x = "Date", y = "Price (USD)") +
  scale_y_continuous(labels = scales::dollar) +  # Format y-axis as dollars
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),  # Center title
    legend.position = "none"  # Remove legend
  )

# Improved plot for NVIDIA stock prices over time
ggplot(NVIDIA_data, aes(x = Date)) +
  geom_line(aes(y = Close), color = "#2ca02c", size = 1) +  # Change line color and thickness 
  labs(title = "NVIDIA Stock Prices Over Time", x = "Date", y = "Close Price (USD)") +
  scale_y_continuous(labels = dollar_format()) +  # Format y-axis as dollars
  theme_minimal(base_size = 14) +  # Use a minimal theme with larger base font size
  theme(
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),  # Center title, larger and bold
    axis.title = element_text(size = 14),  # Larger axis titles
    axis.text = element_text(size = 12),  # Larger axis text
    panel.grid.major = element_line(color = "gray85"),  # Lighten major grid lines
    panel.grid.minor = element_blank()  # Remove minor grid lines
  )

# Combined plot for NVIDIA stock prices and VIX index
scale_factor=1
ggplot() +
  geom_line(data = NVIDIA_data, aes(x = Date, y = Close), color = "#2ca02c", size = 1, alpha = 0.7, linetype = "solid") +  # NVIDIA stock prices
  geom_line(data = vix, aes(x = Date, y = CloseVix * scale_factor), color = "#1f77b4", size = 1, alpha = 0.7, linetype = "solid") +  # VIX index
  labs(title = "NVIDIA Stock Prices and VIX Index Over Time", x = "Date", y = "Price (USD)") +
  scale_y_continuous(labels = dollar_format(), sec.axis = sec_axis(~./scale_factor, name = "VIX Index")) +  # Format y-axis and add secondary axis
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    panel.grid.major = element_line(color = "gray85"),
    panel.grid.minor = element_blank()
  )


# Combined plot for NVIDIA stock prices and BTC Close Price
scale_factor=0.001
ggplot() +
  geom_line(data = NVIDIA_data, aes(x = Date, y = Close), color = "#2ca02c", size = 1, alpha = 0.7, linetype = "solid") +  # NVIDIA stock prices
  geom_line(data = bitcoin, aes(x = Date, y = CloseBTC * scale_factor), color = "#1f77b4", size = 1, alpha = 0.7, linetype = "solid") +  # VIX index
  labs(title = "NVIDIA Stock Prices and BTC Price Over Time", x = "Date", y = "Price (USD)") +
  scale_y_continuous(labels = dollar_format(), sec.axis = sec_axis(~./scale_factor, name = "BTC Price")) +  # Format y-axis and add secondary axis
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    panel.grid.major = element_line(color = "gray85"),
    panel.grid.minor = element_blank()
  )


# Plot the stock prices over the last year (Open vs Close)
NVIDIA_data_last_year <- NVIDIA_data %>%
  filter(Date >= as.Date(Sys.Date()) - 365)
ggplot(NVIDIA_data_last_year, aes(x = Date)) +
geom_ribbon(aes(ymin = Low, ymax = High), fill = "lightblue", alpha = 0.4) +
geom_line(aes(y = Open, color = "Open"), size = 1) +
geom_line(aes(y = Close, color = "Close"), size = 1) +
labs(title = "NVIDIA Stock Prices Over the Last Year (with High-Low Ribbon)", 
     x = "Date", y = "Price") +
scale_color_manual(values = c("Open" = "red", "Close" = "green")) +
theme_minimal()


# Plot the stock prices over time (Adjusted Close vs Close)
ggplot(NVIDIA_data, aes(x = Date)) +
  geom_line(aes(y = `Adj Close`, color = "Adjusted Close")) +
  geom_line(aes(y = Close, color = "Close")) +
  labs(title = "NVIDIA Stock Prices Over Time", x = "Date", y = "Price") +
  scale_color_manual(values = c("Adjusted Close" = "red", "Close" = "green")) +
  theme_minimal()

# Plot the Volume data over the last year
ggplot(NVIDIA_data, aes(x = Date, y = Volume)) +
  geom_bar(stat = "identity", fill = "blue", alpha = 0.7) +
  labs(title = "NVIDIA Stock Volume Over the Last Year", 
       x = "Date", y = "Volume") +
  theme_minimal()

# Summary statistics
summary(NVIDIA_data$Close)

# Skewness and Kurtosis
skewness(NVIDIA_data$Close)
kurtosis(NVIDIA_data$Close)

# Plot the distribution of the 'Close' prices
ggplot(NVIDIA_data, aes(x = Close)) +
  geom_histogram(binwidth = 0.05, fill = "steelblue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of 'Close' Prices",
       x = "Close Price",
       y = "Frequency") +
  theme_minimal()

NVIDIA_data$Close_log <- log(NVIDIA_data$Close)

skewness(NVIDIA_data$Close_log)
kurtosis(NVIDIA_data$Close_log)

# Plot the distribution of the log-transformed 'Close' prices
ggplot(NVIDIA_data, aes(x = Close_log)) +
  geom_histogram(binwidth = 0.05, fill = "steelblue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Log-Transformed 'Close' Prices",
       x = "Log of Close Price",
       y = "Frequency") +
  theme_minimal()

# I don't know how useful would be to work with logs

# Correlation matrix for numeric variables
cor_matrix <- cor(cbind(NVIDIA_data %>% select(Open, High, Low, Close, Volume, `Adj Close`, CloseVix, CloseBTC), 
                        tech_data$Sector_Close, 
                        semic_data$Industry_Close))
corrplot(cor_matrix, method = "number")
corrplot(cor_matrix, 
         method = "color",                     # Use color to fill cells
         col = brewer.pal(n = 11, name = "RdYlBu"), # Color palette (red to blue)
         addCoef.col = "black",                # Add correlation coefficients in black
         number.cex = 0.8,                     # Adjust size of correlation numbers
         tl.col = "black",                     # Text label color
         tl.srt = 45,                          # Rotate x-axis labels for readability
         tl.cex = 0.8,                         # Adjust text label size
         title = "Enhanced Correlation Heatmap",# Add a title
         mar = c(0, 0, 2, 0),                  # Margins around the plot
         diag = TRUE)                         # Hide diagonal values

print(cor_matrix)

# How many stock days per year
# Extract the year from each date
years <- format(NVIDIA_data$Date, "%Y")

# Count the number of rows for each year
year_counts <- table(years)

# Print the counts for each year
print(year_counts)

# Convert NVIDIA_data$Close into time series, 
start_year <- 2015
start_period <- as.numeric(format(as.Date("2015-01-01"), "%j"))
finish_year <- 2024
finish_period <- as.numeric(format(as.Date("2024-08-29"), "%j"))

# Create time series object with correct start and end parameters
NVIDIA_ts <- ts(NVIDIA_data$Close, start = c(start_year, start_period), end = c(finish_year, finish_period), frequency = 365)
autoplot(NVIDIA_ts)

# ADF Test for stationary
adf_test <- adf.test(NVIDIA_ts, alternative = "stationary")
print(adf_test)
kpss_test_result <- kpss.test(NVIDIA_ts)
print(kpss_test_result)

#based on the ADF test data is non-stationary.  
#Since the p-value (0.99) is much greater than the
#typical significance level of 0.05, you fail to reject
#the null hypothesis (data is non-stationary)
#we can confirm by plotting the ACF and the PACF
# the KPSS test also supports this. 

# Decomposition of NVIDIA close value
decomposed_index <- stl(NVIDIA_ts, s.window = "periodic")
plot(decomposed_index)

# Huge trend. Also seasonality.

# ACF and PACF plots 
acf(NVIDIA_data$Close, main = "Autocorrelation of NVIDIA Close price") # exponential decaying
pacf(NVIDIA_data$Close, main = "Partial Autocorrelation of NVIDIA Close price") # huge spike at lag 1

#The ACF and PACF plots together suggest that the close price 
#is likely non-stationary (which you can confirm with the Augmented 
#Dickey-Fuller (ADF) test).
#The PACF indicates that an AR(1) process could be suitable, 
#meaning the current value of the index is mostly influenced by 
#its immediate past value.
#To address the non-stationarity, differencing the series might 
#be required before further analysis such as ARIMA modeling.

# First difference of the close value
NVIDIA_ts_d1 <- diff(NVIDIA_ts, differences = 1)
NVIDIA_data_d1 <- diff(NVIDIA_data$Close, differences = 1)

# ADF test on the differentiated series
adf_test_diff <- adf.test(NVIDIA_ts_d1, alternative = "stationary")
print(adf_test_diff)
kpss_test_result <- kpss.test(NVIDIA_ts_d1)
print(kpss_test_result)

# The ADF tells us that the data is likely stationary but the KPSS test do not!

# Second difference of the close value
NVIDIA_ts_d2 <- diff(NVIDIA_ts, differences = 2)
NVIDIA_data_d2 <- diff(NVIDIA_data$Close, differences = 2)

# ADF test on the differentiated series
adf_test_diff <- adf.test(NVIDIA_ts_d2, alternative = "stationary")
print(adf_test_diff)
kpss_test_result <- kpss.test(NVIDIA_ts_d2)
print(kpss_test_result)

# Now both ADF ans KPSS tells us that the data is likely stationary!

# ACF and PACF plots on first differentiated data
autoplot(NVIDIA_ts_d1)
acf(NVIDIA_data_d1, main = "ACF of First Differenced NVIDIA Close Price")
pacf(NVIDIA_data_d1, main = "PACF of First Differenced NVIDIA Close Price")

# ACF and PACF plots on second differentiated data
autoplot(NVIDIA_ts_d2)
acf(NVIDIA_data_d2, main = "ACF of Secong Differenced NVIDIA Close Price")
pacf(NVIDIA_data_d2, main = "PACF of Second Differenced NVIDIA Close Price")

# Results of the ADF Test, KPSS test, and the plots (PACF, ACF) shows that 
# the differentiated series is now stationary!!

######################### Modeling Part ##############################

# Splitting the data into training and testing sets (test = last three months
train_size <- length(NVIDIA_ts) - 90
train_data <- window(NVIDIA_ts, end = c(time(NVIDIA_ts)[train_size]))
test_data <- window(NVIDIA_ts, start = c(time(NVIDIA_ts)[train_size + 1]))

####################### LOESS (only for modelling) ########################

# Convert time series to data frame for easier manipulation
train_df <- data.frame(
  date = time(train_data),
  value = as.numeric(train_data)
)

# Fit LOESS model
loess_model <- loess(value ~ as.numeric(date), data = train_df, span = 0.2)

summary(loess_model)

# Generate fitted values
fitted_values_loess <- predict(loess_model)

# Plot the fitted LOESS model over the real data
ggplot(train_df, aes(x = date, y = value)) +
  geom_line(color = "black") +
  geom_line(data = data.frame(date = train_df$date, value = fitted_values_loess), 
            aes(x = date, y = value), color = "green") +
  ggtitle("LOESS Fitted Values vs Actual") +
  xlab("Time") + ylab("NVIDIA Close Price") +
  theme_minimal()

# Calculate metrics
rmse_loess <- sqrt(mean((train_df$value - fitted_values_loess)^2))
mape_loess <- mean(abs((train_df$value - fitted_values_loess) / train_df$value)) * 100

cat("RMSE:", rmse_loess, "\n")
cat("MAPE:", mape_loess, "%\n")

# Calculate residuals
residuals_loess <- train_df$value - fitted_values_loess

# Plot residuals
ggplot(data.frame(date = train_df$date, residuals = residuals_loess), aes(x = date, y = residuals_loess)) +
  geom_line() +
  ggtitle("Residuals of LOESS Model") +
  xlab("Time") + ylab("Residuals") +
  theme_minimal()

# ACF of residuals
acf_residuals_loess <- acf(residuals_loess, plot = FALSE)
plot(acf_residuals_loess, main = "ACF of Residuals")

# PACF of residuals
pacf_residuals_loess <- pacf(residuals_loess, plot = FALSE)
plot(pacf_residuals_loess, main = "PACF of Residuals")

# Perform Box-Pierce test for autocorrelation in residuals
# The BP test is based on the null hypothesis (H₀) that there is no autocorrelation up to a specified lag k
# The alternative hypothesis (H₁) is that there is autocorrelation in the residuals.
# If the p-value is small (e.g., < 0.05): Reject the null hypothesis, indicating that autocorrelation is present in the residuals.
# If the p-value is large (e.g., > 0.05): Fail to reject the null hypothesis, meaning there is no significant autocorrelation.
bp_test <- Box.test(residuals_loess, lag = 20, type = "Box-Pierce")
print(bp_test)

# Dublin-Watson test
# The Durbin-Watson statistic ranges from 0 to 4:
# DW ≈ 2: No autocorrelation.
# DW < 2: Positive autocorrelation.
# DW > 2: Negative autocorrelation.

# Perform the Durbin-Watson test on residuals
dw_test <- dwtest(residuals_loess ~ 1)
print(dw_test)

# There is positive autocorrelation in the residuals.

# Check for normality of residuals
hist(residuals_loess, breaks = 30, main = "Histogram of Residuals", xlab = "Residuals")
# Shapiro-Wilk normality test
shapiro_test <- shapiro.test(residuals_loess)
print(shapiro_test)


# So, residuals doesn't look like White Noise so they are correlated!


####################### SPLINE REGRESSION (for modeling) ########################

# Convert time series to data frame for easier manipulation (if not already in data frame)
train_df <- data.frame(
  date = time(train_data),  # Assuming train_data is a time series object
  value = as.numeric(train_data)
)

# Fit Spline Regression model 
# degree specifies the degree of the polynomial (cubic splines by default)
# knots defines where the spline pieces are joined
spline_r_model <- lm(value ~ bs(date, degree = 3, knots = quantile(train_df$date, probs = c(0.25, 0.5, 0.75))), data = train_df)

# Summary of the model
summary(spline_r_model)

# Generate fitted values
fitted_values_spline_r <- predict(spline_r_model, newdata = train_df)

# Plot the fitted Spline Regression model over the real data
ggplot(train_df, aes(x = date, y = value)) +
  geom_line(color = "black") +
  geom_line(data = data.frame(date = train_df$date, value = fitted_values_spline_r), 
            aes(x = date, y = value), color = "purple") +
  ggtitle("Spline Regression Fitted Values vs Actual") +
  xlab("Time") + ylab("NVIDIA Close Price") +
  theme_minimal()

# Calculate metrics
rmse_spline_r <- sqrt(mean((train_df$value - fitted_values_spline_r)^2))
mape_spline_r <- mean(abs((train_df$value - fitted_values_spline_r) / train_df$value)) * 100

cat("RMSE:", rmse_spline_r, "\n")
cat("MAPE:", mape_spline_r, "%\n")

# Calculate residuals
residuals_spline_r <- train_df$value - fitted_values_spline_r

# Plot residuals
ggplot(data.frame(date = train_df$date, residuals = residuals_spline_r), aes(x = date, y = residuals_spline_r)) +
  geom_line() +
  ggtitle("Residuals of Spline Regression Model") +
  xlab("Time") + ylab("Residuals") +
  theme_minimal()

# ACF of residuals
acf_residuals_spline_r <- acf(residuals_spline_r, plot = FALSE)
plot(acf_residuals_spline_r, main = "ACF of Residuals")

# PACF of residuals
pacf_residuals_spline_r <- pacf(residuals_spline_r, plot = FALSE)
plot(pacf_residuals_spline_r, main = "PACF of Residuals")

# Perform Box-Pierce test for autocorrelation in residuals
bp_test_spline_r <- Box.test(residuals_spline_r, lag = 20, type = "Box-Pierce")
print(bp_test_spline_r)

# Perform the Durbin-Watson test on residuals
dw_test_spline_r <- dwtest(residuals_spline_r ~ 1)
print(dw_test_spline_r)

# Check for normality of residuals
hist(residuals_spline_r, breaks = 30, main = "Histogram of Residuals", xlab = "Residuals")

# Shapiro-Wilk normality test
shapiro_test_spline_r <- shapiro.test(residuals_spline_r)
print(shapiro_test_spline_r)

####################### SMOOTHING SPLINES (for modeling) ########################

# Convert time series to data frame for easier manipulation (if not already in data frame)
train_df <- data.frame(
  date = time(train_data),  # Assuming train_data is a time series object
  value = as.numeric(train_data)
)

# Fit Smoothing Spline model
# spar controls the smoothness of the spline (similar to span in LOESS); it ranges from 0 to 1
smoothing_spline_model <- smooth.spline(train_df$date, train_df$value, spar = 0.7)

# Generate fitted values
fitted_values_spline <- predict(smoothing_spline_model, train_df$date)$y

# Plot the fitted Smoothing Spline model over the real data
ggplot(train_df, aes(x = date, y = value)) +
  geom_line(color = "black") +
  geom_line(data = data.frame(date = train_df$date, value = fitted_values_spline), 
            aes(x = date, y = value), color = "orange") +
  ggtitle("Smoothing Spline Fitted Values vs Actual") +
  xlab("Time") + ylab("NVIDIA Close Price") +
  theme_minimal()

# Calculate metrics
rmse_spline <- sqrt(mean((train_df$value - fitted_values_spline)^2))
mape_spline <- mean(abs((train_df$value - fitted_values_spline) / train_df$value)) * 100

cat("RMSE:", rmse_spline, "\n")
cat("MAPE:", mape_spline, "%\n")

# Calculate residuals
residuals_spline <- train_df$value - fitted_values_spline

# Plot residuals
ggplot(data.frame(date = train_df$date, residuals = residuals_spline), aes(x = date, y = residuals)) +
  geom_line() +
  ggtitle("Residuals of Smoothing Spline Model") +
  xlab("Time") + ylab("Residuals") +
  theme_minimal()

# ACF of residuals
acf_residuals_spline <- acf(residuals_spline, plot = FALSE)
plot(acf_residuals_spline, main = "ACF of Residuals")

# PACF of residuals
pacf_residuals_spline <- pacf(residuals_spline, plot = FALSE)
plot(pacf_residuals_spline, main = "PACF of Residuals")

# Perform Box-Pierce test for autocorrelation in residuals
bp_test_spline <- Box.test(residuals_spline, lag = 20, type = "Box-Pierce")
print(bp_test_spline)

# Perform the Durbin-Watson test on residuals
dw_test_spline <- dwtest(residuals_spline ~ 1)
print(dw_test_spline)

# Check for normality of residuals
hist(residuals_spline, breaks = 30, main = "Histogram of Residuals", xlab = "Residuals")

# Shapiro-Wilk normality test
shapiro_test_spline <- shapiro.test(residuals_spline)
print(shapiro_test_spline)

######################## GAM with Tech and Semiconductors data #####################################

# Convert time series to data frame for easier manipulation (if not already in data frame)
train_df <- data.frame(
  date = time(train_data),  # Assuming train_data is a time series object
  value = as.numeric(train_data)
)

# Create the exog data
full_exog_data <- semic_data %>% left_join(tech_data, by = "Date")

# The years in train_df are in decimal representation
convert_to_decimal_year <- function(date_str) {
  # Convert to Date object
  date <- as.Date(date_str)
  
  # Extract year, month, and day
  year <- year(date)
  
  # Calculate the fraction of the year
  days_in_year <- ifelse(leap_year(year), 366, 365)
  fraction_of_year <- (yday(date) - 1) / days_in_year
  
  # Combine year and fraction
  decimal_year <- year + fraction_of_year
  
  return(decimal_year)
}

# Ensure that the 'Date' column in full_exog_data is of Date type
full_exog_data$Date <- as.Date(full_exog_data$Date)

# Convert the 'Date' column to decimal year format
full_exog_data <- full_exog_data %>%
  mutate(DecimalYear = sapply(Date, convert_to_decimal_year))

# Define the date range from train_df
min_date <- min(train_df$date)
max_date <- max(train_df$date)

# Filter the exogenous data to include only dates within the range
exog_df <- full_exog_data[full_exog_data$DecimalYear >= min_date & full_exog_data$DecimalYear <= max_date, ]

combined_df <- bind_cols(train_df[-c(1), ], exog_df[, c(3, 5)])

# Plot it
ggplot(combined_df, aes(x = date)) +
  geom_line(aes(y = value, color = "Value")) +
  geom_line(aes(y = Industry_Close, color = "Industry Close")) +
  geom_line(aes(y = Sector_Close, color = "Sector Close")) +
  labs(title = "Evolution of Value, Industry Close, and Sector Close",
       x = "Date",
       y = "Value",
       color = "Legend") +
  theme_minimal()

# Fit GAM model
gam_model <- gam(value ~ s(date) + Industry_Close + Sector_Close, data = combined_df)

# Summary of the GAM model
summary(gam_model)

# Generate fitted values
fitted_values_gam <- fitted(gam_model)

# Plot the fitted Smoothing gam model over the real data
ggplot(combined_df, aes(x = date, y = value)) +
  geom_line(color = "black") +
  geom_line(aes(y = fitted_values_gam), color = "green") +
  ggtitle("GAM Fitted Values vs Actual with Predictors") +
  xlab("Time") + ylab("Value") +
  theme_minimal()


# Calculate metrics
aic_gam <- AIC(gam_model)
rmse_gam <- sqrt(mean((combined_df$value - fitted_values_gam)^2))
mape_gam <- mean(abs((combined_df$value - fitted_values_gam) / combined_df$value)) * 100

cat("AIC:", aic_gam, "\n")
cat("RMSE:", rmse_gam, "\n")
cat("MAPE:", mape_gam, "%\n")

# Calculate residuals
residuals_gam <- combined_df$value - fitted_values_gam

# Plot residuals
ggplot(data.frame(date = combined_df$date, residuals = residuals_gam), aes(x = date, y = residuals)) +
  geom_line() +
  ggtitle("Residuals of GAM Model") +
  xlab("Time") + ylab("Residuals") +
  theme_minimal()

# ACF of residuals
acf_residuals_gam <- acf(residuals_gam, plot = FALSE)
plot(acf_residuals_gam, main = "ACF of Residuals")

# PACF of residuals
pacf_residuals_gam <- pacf(residuals_gam, plot = FALSE)
plot(pacf_residuals_gam, main = "PACF of Residuals")

# Perform Box-Pierce test for autocorrelation in residuals
bp_test_gam <- Box.test(residuals_gam, lag = 20, type = "Box-Pierce")
print(bp_test_gam)

# Perform the Durbin-Watson test on residuals
dw_test_gam <- dwtest(residuals_gam ~ 1)
print(dw_test_gam)

# Check for normality of residuals
hist(residuals_gam, breaks = 30, main = "Histogram of Residuals", xlab = "Residuals")

# Shapiro-Wilk normality test
shapiro_test_gam <- shapiro.test(residuals_gam)
print(shapiro_test_gam)


######################## GAM with Tech and Semiconductors data and VIX index #####################################

# Convert time series to data frame for easier manipulation (if not already in data frame)
train_df <- data.frame(
  date = time(train_data),  # Assuming train_data is a time series object
  value = as.numeric(train_data)
)

# Create the exog data
full_exog_data <- semic_data %>% left_join(tech_data, by = "Date") # add Tech and Semiconductors
full_exog_data <- full_exog_data %>% left_join(vix %>% select(Date, CloseVix), by = "Date") # Add VIX Close Index

# The years in train_df are in decimal representation
convert_to_decimal_year <- function(date_str) {
  # Convert to Date object
  date <- as.Date(date_str)
  
  # Extract year, month, and day
  year <- year(date)
  
  # Calculate the fraction of the year
  days_in_year <- ifelse(leap_year(year), 366, 365)
  fraction_of_year <- (yday(date) - 1) / days_in_year
  
  # Combine year and fraction
  decimal_year <- year + fraction_of_year
  
  return(decimal_year)
}

# Ensure that the 'Date' column in full_exog_data is of Date type
full_exog_data$Date <- as.Date(full_exog_data$Date)

# Convert the 'Date' column to decimal year format
full_exog_data <- full_exog_data %>%
  mutate(DecimalYear = sapply(Date, convert_to_decimal_year))

# Define the date range from train_df
min_date <- min(train_df$date)
max_date <- max(train_df$date)

# Filter the exogenous data to include only dates within the range
exog_df <- full_exog_data[full_exog_data$DecimalYear >= min_date & full_exog_data$DecimalYear <= max_date, ]

combined_df <- bind_cols(train_df[-c(1), ], exog_df[, c(3, 5, 6)])

# Fit GAM model
gam_model <- gam(value ~ s(date) + Industry_Close + Sector_Close + CloseVix, data = combined_df)

# Summary of the GAM model
summary(gam_model)

# Generate fitted values
fitted_values_gam <- fitted(gam_model)

# Plot the fitted Smoothing gam model over the real data
ggplot(combined_df, aes(x = date, y = value)) +
  geom_line(color = "black") +
  geom_line(aes(y = fitted_values_gam), color = "green") +
  ggtitle("GAM Fitted Values vs Actual with Predictors") +
  xlab("Time") + ylab("Value") +
  theme_minimal()


# Calculate metrics
aic_gam_2 <- AIC(gam_model)
rmse_gam_2 <- sqrt(mean((combined_df$value - fitted_values_gam)^2))
mape_gam_2 <- mean(abs((combined_df$value - fitted_values_gam) / combined_df$value)) * 100

cat("AIC:", aic_gam_2, "\n")
cat("RMSE:", rmse_gam_2, "\n")
cat("MAPE:", mape_gam_2, "%\n")

# Calculate residuals
residuals_gam <- combined_df$value - fitted_values_gam

# Plot residuals
ggplot(data.frame(date = combined_df$date, residuals = residuals_gam), aes(x = date, y = residuals)) +
  geom_line() +
  ggtitle("Residuals of GAM Model") +
  xlab("Time") + ylab("Residuals") +
  theme_minimal()

# ACF of residuals
acf_residuals_gam <- acf(residuals_gam, plot = FALSE)
plot(acf_residuals_gam, main = "ACF of Residuals")

# PACF of residuals
pacf_residuals_gam <- pacf(residuals_gam, plot = FALSE)
plot(pacf_residuals_gam, main = "PACF of Residuals")

# Perform Box-Pierce test for autocorrelation in residuals
bp_test_gam <- Box.test(residuals_gam, lag = 20, type = "Box-Pierce")
print(bp_test_gam)

# Perform the Durbin-Watson test on residuals
dw_test_gam <- dwtest(residuals_gam ~ 1)
print(dw_test_gam)

# Check for normality of residuals
hist(residuals_gam, breaks = 30, main = "Histogram of Residuals", xlab = "Residuals")

# Shapiro-Wilk normality test
shapiro_test_gam <- shapiro.test(residuals_gam)
print(shapiro_test_gam)

######################## GAM with Tech and Semiconductors data, VIX index and BTC price #####################################

# Convert time series to data frame for easier manipulation (if not already in data frame)
train_df <- data.frame(
  date = time(train_data),  # Assuming train_data is a time series object
  value = as.numeric(train_data)
)

# Create the exog data
full_exog_data <- semic_data %>% left_join(tech_data, by = "Date") # add Tech and Semiconductors
full_exog_data <- full_exog_data %>% left_join(bitcoin %>% select(Date, CloseBTC), by = "Date") # Add BTC Close Value
full_exog_data <- full_exog_data %>% left_join(NVIDIA_data %>% select(Date, CloseVix), by = "Date") # Add VIX Close Index

# The years in train_df are in decimal representation
convert_to_decimal_year <- function(date_str) {
  # Convert to Date object
  date <- as.Date(date_str)
  
  # Extract year, month, and day
  year <- year(date)
  
  # Calculate the fraction of the year
  days_in_year <- ifelse(leap_year(year), 366, 365)
  fraction_of_year <- (yday(date) - 1) / days_in_year
  
  # Combine year and fraction
  decimal_year <- year + fraction_of_year
  
  return(decimal_year)
}

# Ensure that the 'Date' column in full_exog_data is of Date type
full_exog_data$Date <- as.Date(full_exog_data$Date)

# Convert the 'Date' column to decimal year format
full_exog_data <- full_exog_data %>%
  mutate(DecimalYear = sapply(Date, convert_to_decimal_year))

# Define the date range from train_df
min_date <- min(train_df$date)
max_date <- max(train_df$date)

# Filter the exogenous data to include only dates within the range
exog_df <- full_exog_data[full_exog_data$DecimalYear >= min_date & full_exog_data$DecimalYear <= max_date, ]

combined_df <- bind_cols(train_df[-c(1), ], exog_df[, c(3, 5, 6, 7)])

# Fit GAM model
gam_model <- gam(value ~ s(date) + Industry_Close + Sector_Close + CloseVix + CloseBTC, data = combined_df)

# Summary of the GAM model
summary(gam_model)

# Generate fitted values
fitted_values_gam <- fitted(gam_model)

# Plot the fitted Smoothing gam model over the real data
ggplot(combined_df, aes(x = date, y = value)) +
  geom_line(color = "black") +
  geom_line(aes(y = fitted_values_gam), color = "green") +
  ggtitle("GAM Fitted Values vs Actual with Predictors") +
  xlab("Time") + ylab("Value") +
  theme_minimal()


# Calculate metrics
aic_gam_3 <- AIC(gam_model)
rmse_gam_3 <- sqrt(mean((combined_df$value - fitted_values_gam)^2))
mape_gam_3 <- mean(abs((combined_df$value - fitted_values_gam) / combined_df$value)) * 100

cat("AIC:", aic_gam_3, "\n")
cat("RMSE:", rmse_gam_3, "\n")
cat("MAPE:", mape_gam_3, "%\n")

# Calculate residuals
residuals_gam <- combined_df$value - fitted_values_gam

# Plot residuals
ggplot(data.frame(date = combined_df$date, residuals = residuals_gam), aes(x = date, y = residuals)) +
  geom_line() +
  ggtitle("Residuals of GAM Model") +
  xlab("Time") + ylab("Residuals") +
  theme_minimal()

# ACF of residuals
acf_residuals_gam <- acf(residuals_gam, plot = FALSE)
plot(acf_residuals_gam, main = "ACF of Residuals")

# PACF of residuals
pacf_residuals_gam <- pacf(residuals_gam, plot = FALSE)
plot(pacf_residuals_gam, main = "PACF of Residuals")

# Perform Box-Pierce test for autocorrelation in residuals
bp_test_gam <- Box.test(residuals_gam, lag = 20, type = "Box-Pierce")
print(bp_test_gam)

# Perform the Durbin-Watson test on residuals
dw_test_gam <- dwtest(residuals_gam ~ 1)
print(dw_test_gam)

# Check for normality of residuals
hist(residuals_gam, breaks = 30, main = "Histogram of Residuals", xlab = "Residuals")

# Shapiro-Wilk normality test
shapiro_test_gam <- shapiro.test(residuals_gam)
print(shapiro_test_gam)
####################### Gradient Boosting  ########################

# Prepare data for Gradient Boosting
lag_order <- 10  # Number of lags to use as features
X <- embed(train_df$value, lag_order + 1)
y <- X[,1]  # Current values
X <- X[,-1]  # Lagged values

# Fit Gradient Boosting Model
gb_model <- gbm(y ~ ., data = data.frame(y = y, X), 
                distribution = "gaussian", 
                n.trees = 1000, 
                interaction.depth = 3, 
                shrinkage = 0.1, 
                cv.folds = 5)

# Find optimal number of trees
best_iter <- gbm.perf(gb_model, method = "cv")

# Generate fitted values
fitted_values <- predict(gb_model, n.trees = best_iter)
fitted_values <- c(rep(NA, lag_order), fitted_values)  # Add NAs for the first 'lag_order' observations

# Plot the fitted Gradient Boosting model over the real data
ggplot(train_df, aes(x = date, y = value)) +
  geom_line(color = "black") +
  geom_line(data = data.frame(date = train_df$date, value = fitted_values), 
            aes(x = date, y = value), color = "green") +
  ggtitle("Gradient Boosting Fitted Values vs Actual") +
  xlab("Time") + ylab("NVIDIA Close Value") +
  theme_minimal()

# Calculate metrics for training data
rmse_gb <- sqrt(mean((train_df$value[-c(1:lag_order)] - fitted_values[-c(1:lag_order)])^2, na.rm = TRUE))
mape_gb <- mean(abs((train_df$value[-c(1:lag_order)] - fitted_values[-c(1:lag_order)]) / train_df$value[-c(1:lag_order)]), na.rm = TRUE) * 100

cat("RMSE (training):", rmse_gb, "\n")
cat("MAPE (training):", mape_gb, "%\n")

# Analyze residuals
residuals <- train_df$value - fitted_values

# Plot residuals
ggplot(data.frame(date = train_df$date, residuals = residuals), aes(x = date, y = residuals)) +
  geom_line() +
  ggtitle("Residuals of Gradient Boosting Model") +
  xlab("Time") + ylab("Residuals") +
  theme_minimal()

# ACF and PACF of residuals
acf(residuals, na.action = na.pass, main = "ACF of Residuals")
pacf(residuals, na.action = na.pass, main = "PACF of Residuals")

# Box-Pierce test
lb_test <- Box.test(residuals, lag = 20, type = "Box-Pierce")
print(lb_test)

# Perform the Durbin-Watson test on residuals
dwtest(residuals ~ 1)

# Normality check
hist(residuals, breaks = 30, main = "Histogram of Residuals", xlab = "Residuals")
shapiro_test <- shapiro.test(residuals)
print(shapiro_test)


######################### Holt - Winter's Exponential Smoothing Method ####################

# Holt-Winters’ method is suitable when both trend and seasonality are present in the data

hw_model <- HoltWinters(train_data, seasonal = "additive") # maybe try "multiplicative"

# NOTE: The hw() function automatically selects optimal smoothing parameters (alpha, beta, gamma) 
# using maximum likelihood estimation.

# Summary of the model
summary(hw_model)

# Generate in-sample predictions
fitted_values_hw <- hw_model$fitted[,1]

# Plot the fitted Holt-Winters model over the real data
autoplot(train_data, series = "Actual") + 
  autolayer(fitted_values_hw, series = "Fitted") + 
  scale_color_manual(values = c("black", "green")) +
  ggtitle("Holt-Winters Fitted Values vs Actual") +
  xlab("Time") + ylab("NVIDIA Close Price") +
  theme_minimal()

# Metrics of the modeling
#aic <- AIC(hw_model)
rmse_hw <- sqrt(mean((train_data - fitted_values_hw)^2))
mape_hw <- mean(abs((train_data - fitted_values_hw) / train_data)) * 100

#cat("AIC:", aic, "\n")
cat("RMSE:", rmse_hw, "\n")
cat("MAPE:", mape_hw, "%\n")

# NOTE:
# The the AIC value of the ARIMA model can be compared with other ARIMA models
# but the ETS (exponential smoothing methods= should not be compared directly with the
# ARIMA AIC value because they are different model classes and likelihood is computed in different ways.
# For this reason, the models will be compared outside of these values but they are still useful in creating the best model within each class.

# STUDY THE RESIDUALS
# Plot residuals
autoplot(residuals(hw_model)) +
  ggtitle("Residuals of Holt-Winters Model") +
  xlab("Time") + ylab("Residuals") +
  theme_minimal()

# Check the autocorrelation of residuals (should be like white noise)
Acf(residuals(hw_model), main = "ACF of Residuals")

# Perform a "Box-Pierce" test for autocorrelation in residuals
Box.test(residuals(hw_model), lag = 1, type = "Box-Pierce") 

# Perform the Durbin-Watson test on residuals
dwtest(residuals(auto_armax_model) ~ 1)

# Check for normality of residuals (e.g., using a histogram or Shapiro-Wilk test)
hist(residuals(hw_model), breaks = 20, main = "Histogram of Residuals", col = "lightblue")
shapiro.test(residuals(hw_model))  # Normality test

# FORECAST on the test data
forecast_hw <- forecast(hw_model, h = length(test_data))
print(forecast_hw)

# Plot forecast vs actual
autoplot(forecast_hw) + 
  autolayer(test_data, series = "Actual") + 
  scale_color_manual(values = c("black", "red")) +
  ggtitle("Holt-Winters Forecast vs Actual") +
  xlab("Time") + ylab("NVIDIA Close Price") +
  coord_cartesian(xlim = c(time(NVIDIA_ts)[length(NVIDIA_ts) - 252], time(NVIDIA_ts)[length(NVIDIA_ts)])) +
  theme_minimal()

# Metrics of the forecast
rmse_forecast_hw <- sqrt(mean((test_data - forecast_hw$mean)^2))
mape_forecast_hw <- mean(abs((test_data - forecast_hw$mean) / test_data)) * 100

# Print metrics
#cat("AIC:", aic, "\n")
cat("RMSE (forecast):", rmse_forecast_hw, "\n")
cat("MAPE (forecast):", mape_forecast_hw, "%\n")

######################### ARIMA (1,1,1) ##############################
########### We use ARIMAS related models for forecasting because they produce the best fitting scores (also is the easiest)!

# ARIMA(p,d,q): q = 1 since we took the first difference

# Run ACF test to select AR term or the p term
Acf(NVIDIA_ts,lag.max = 252) 
Acf(NVIDIA_ts_d1,lag.max = 252)
Acf(NVIDIA_ts_d2,lag.max = 252)

# Run PACF test to select MA term or the q term
Pacf(NVIDIA_ts, lag.max = 252)
Pacf(NVIDIA_ts_d1, lag.max = 252)
Pacf(NVIDIA_ts_d2, lag.max = 252)

# Choose the parameters
p = 1 # ACF exponentially or sinusoidal decaying and PACF spike at lag 1
d = 1 # first differentiation
q = 1 # for example

# Fit ARIMA(1,1,1) model
arima_model_111 <- Arima(train_data, order = c(1, 1, 1))

# Summary of the model
summary(arima_model_111)

# Generate in-sample predictions
fitted_values_111 <- fitted(arima_model_111)

# Plot the fitted ARIMA model over the real data
autoplot(train_data, series = "Actual") + 
  autolayer(fitted_values_111, series = "Fitted") + 
  scale_color_manual(values = c("black", "green")) +  # Different colors for actual vs fitted
  ggtitle("ARIMA(1,1,1) Fitted Values vs Actual") +
  xlab("Time") + ylab("NVIDIA Close Price") +
  theme_minimal()  # A cleaner theme for better visualization

# Metrics of the modeling
aic_111 <- AIC(arima_model_111)
rmse_111 <- sqrt(mean((train_data - fitted_values_111)^2))
mape_111 <- mean(abs((train_data - fitted_values_111) / train_data)) * 100

cat("AIC:", aic_111, "\n")
cat("RMSE:", rmse_111, "\n")
cat("MAPE:", mape_111, "%\n")

# Plot diagnostics
tsdiag(arima_model_111)

# Plot residuals
autoplot(residuals(arima_model_111)) +
  ggtitle("Residuals of ARIMA(1,1,1) Model") +
  xlab("Time") + ylab("Residuals") +
  theme_minimal()

# Check the autocorrelation of residuals (should be like white noise)
Acf(residuals(arima_model_111), main = "ACF of Residuals")

# Perform a "Box-Pierce" test for autocorrelation in residuals
Box.test(residuals(arima_model_111), lag = 20, type = "Box-Pierce")

# Perform the Durbin-Watson test on residuals
dwtest(residuals(arima_model_111) ~ 1)

# Check for normality of residuals (e.g., using a histogram or Shapiro-Wilk test)
hist(residuals(arima_model_111), breaks = 20, main = "Histogram of Residuals", col = "lightblue")
shapiro.test(residuals(arima_model_111))  # Normality test

# Both tests suggest that the current ARIMA(1,1,1) model might not be ideal! However we forecast (just to check)!

# FORECAST on the test data
forecast_arima_111 <- forecast(arima_model_111, h = length(test_data))

print(forecast_arima_111)

# Plot only the last part of the data to focus on forecast and actual
autoplot(forecast_arima_111) + 
  autolayer(test_data, series = "Actual") + 
  scale_color_manual(values = c("black", "red")) +  # Different colors for forecast vs actual
  ggtitle("ARIMA(1,1,1) Forecast vs Actual") +
  xlab("Time") + ylab("NVIDIA Close Price") +
  coord_cartesian(xlim = c(time(NVIDIA_ts)[length(NVIDIA_ts) - 365], time(NVIDIA_ts)[length(NVIDIA_ts)])) +  # Restrict to the last year
  theme_minimal()  # A cleaner theme for better visualization

# Metrics of the forecast
aic_111 <- AIC(arima_model_111)
rmse_forecast_111 <- sqrt(mean((test_data - forecast_arima_111$mean)^2))
mape_forecast_111 <- mean(abs((test_data - forecast_arima_111$mean) / test_data)) * 100

# Print metrics
cat("AIC:", aic_111, "\n")
cat("RMSE:", rmse_forecast_111, "\n")
cat("MAPE:", mape_forecast_111, "%\n")

######################### ARIMA (1,2,1) ##############################

# ARIMA(p,d,q): q = 2 since we took the first difference

# Choose the parameters
p = 1 # ACF exponentially or sinusoidal decaying and PACF spike at lag 1
d = 2 # Second differentiation
q = 1 # for example

# Fit ARIMA(1,2,1) model
arima_model_121 <- Arima(train_data, order = c(1, 2, 1))

# Summary of the model
summary(arima_model_121)

# Generate in-sample predictions
fitted_values_121 <- fitted(arima_model_121)

# Plot the fitted ARIMA model over the real data
autoplot(train_data, series = "Actual") + 
  autolayer(fitted_values_121, series = "Fitted") + 
  scale_color_manual(values = c("black", "green")) +  # Different colors for actual vs fitted
  ggtitle("ARIMA(1,1,1) Fitted Values vs Actual") +
  xlab("Time") + ylab("NVIDIA Close Price") +
  theme_minimal()  # A cleaner theme for better visualization

# Metrics of the modeling
aic_121 <- AIC(arima_model_121)
rmse_121 <- sqrt(mean((train_data - fitted_values_121)^2))
mape_121 <- mean(abs((train_data - fitted_values_121) / train_data)) * 100

cat("AIC:", aic_121, "\n")
cat("RMSE:", rmse_121, "\n")
cat("MAPE:", mape_121, "%\n")

# Plot diagnostics
tsdiag(arima_model_121)

# Plot residuals
autoplot(residuals(arima_model_121)) +
  ggtitle("Residuals of ARIMA(1,2,1) Model") +
  xlab("Time") + ylab("Residuals") +
  theme_minimal()

# Check the autocorrelation of residuals (should be like white noise)
Acf(residuals(arima_model_121), main = "ACF of Residuals")

# Perform a "Box-Pierce" test for autocorrelation in residuals
Box.test(residuals(arima_model_121), lag = 20, type = "Box-Pierce")

# Perform the Durbin-Watson test on residuals
dwtest(residuals(arima_model_121) ~ 1)

# Check for normality of residuals (e.g., using a histogram or Shapiro-Wilk test)
hist(residuals(arima_model_121), breaks = 20, main = "Histogram of Residuals", col = "lightblue")
shapiro.test(residuals(arima_model_121))  # Normality test

# Based on DW  you do not have strong evidence of positive autocorrelation in your residuals.

# FORECAST on the test data
forecast_arima_121 <- forecast(arima_model_121, h = length(test_data))

print(forecast_arima_121)

# Plot only the last part of the data to focus on forecast and actual
autoplot(forecast_arima_121) + 
  autolayer(test_data, series = "Actual") + 
  scale_color_manual(values = c("black", "red")) +  # Different colors for forecast vs actual
  ggtitle("ARIMA(1,1,1) Forecast vs Actual") +
  xlab("Time") + ylab("NVIDIA Close Price") +
  coord_cartesian(xlim = c(time(NVIDIA_ts)[length(NVIDIA_ts) - 252], time(NVIDIA_ts)[length(NVIDIA_ts)])) +  # Restrict to the last year
  theme_minimal()  # A cleaner theme for better visualization

# Metrics of the forecast
aic_121 <- AIC(arima_model_121)
rmse_forecast_121 <- sqrt(mean((test_data - forecast_arima_121$mean)^2))
mape_forecast_121 <- mean(abs((test_data - forecast_arima_121$mean) / test_data)) * 100

# Print metrics
cat("AIC:", aic_121, "\n")
cat("RMSE:", rmse_121, "\n")
cat("MAPE:", mape_121, "%\n")

######################### AUTO ARIMA ##############################

# Fit Auto ARIMA model
auto_arima_model <- auto.arima(train_data, ic = c("aicc", "aic", "bic"), trace=TRUE)

# Summary of the model
summary(auto_arima_model)

# Generate in-sample predictions
fitted_values_autoarima <- fitted(auto_arima_model)

# Plot the fitted ARIMA model over the real data
autoplot(train_data, series = "Actual") + 
  autolayer(fitted_values_autoarima, series = "Fitted") + 
  scale_color_manual(values = c("black", "green")) +  # Different colors for actual vs fitted
  ggtitle("Auto.Arima(0,1,1) with drift Fitted Values vs Actual") +
  xlab("Time") + ylab("NVIDIA Close Price") +
  theme_minimal()  # A cleaner theme for better visualization

# Metrics of the modeling
aic_autoarima <- AIC(auto_arima_model)
rmse_autoarima <- sqrt(mean((train_data - fitted_values_autoarima)^2))
mape_autoarima <- mean(abs((train_data - fitted_values_autoarima) / train_data)) * 100

cat("AIC:", aic_autoarima, "\n")
cat("RMSE:", rmse_autoarima, "\n")
cat("MAPE:", mape_autoarima, "%\n")

# Plot diagnostics
tsdiag(auto_arima_model)

# STUDY THE RESIDUALS
# Plot residuals
autoplot(residuals(auto_arima_model)) +
  ggtitle("Residuals of Auto.ARIMA(0,1,1) with drift Model") +
  xlab("Time") + ylab("Residuals") +
  theme_minimal()

# Check the autocorrelation of residuals (should be like white noise)
Acf(residuals(auto_arima_model), main = "ACF of Residuals")

# Perform a "Box-Pierce" test for autocorrelation in residuals
Box.test(residuals(auto_arima_model), lag = 1, type = "Box-Pierce") 

# Perform the Durbin-Watson test on residuals
dwtest(residuals(auto_arima_model) ~ 1)

# Check for normality of residuals (e.g., using a histogram or Shapiro-Wilk test)
hist(residuals(auto_arima_model), breaks = 20, main = "Histogram of Residuals", col = "lightblue")
shapiro.test(residuals(auto_arima_model))  # Normality test

# Based on DW  you do not have strong evidence of positive autocorrelation in your residuals.

# FORECAST on the test data
forecast_auto_arima <- forecast(auto_arima_model, h = length(test_data))

print(forecast_auto_arima)

# Plot only the last part of the data to focus on forecast and actual
autoplot(forecast_auto_arima) + 
  autolayer(test_data, series = "Actual") + 
  scale_color_manual(values = c("black", "red")) +  # Different colors for forecast vs actual
  ggtitle("ARIMA(5,2,1) Forecast vs Actual") +
  xlab("Time") + ylab("S&P500") +
  coord_cartesian(xlim = c(time(NVIDIA_ts)[length(NVIDIA_ts) - 252], time(NVIDIA_ts)[length(NVIDIA_ts)])) +  # Restrict to the last year
  theme_minimal()  # A cleaner theme for better visualization

# Metrics of the forecast
aic_autoarima <- AIC(auto_arima_model)
rmse_fore_autoarima <- sqrt(mean((test_data - forecast_auto_arima$mean)^2))
mape_fore_autoarima <- mean(abs((test_data - forecast_auto_arima$mean) / test_data)) * 100

# Print metrics
cat("AIC:", aic_autoarima, "\n")
cat("RMSE:", rmse_fore_autoarima, "\n")
cat("MAPE:", mape_fore_autoarima, "%\n")

######################### AUTO ARMAX with Tech data ##############################
# Auto:ARIMA with the Sector: Technologies close values as exogenous variables

exog_ts <- ts(tech_data$Sector_Close, start = start(NVIDIA_ts), end = end(NVIDIA_ts), frequency = 365)

exog_train_data <- window(exog_ts, end = c(time(NVIDIA_ts)[train_size]))
exog_test_data <- window(exog_ts, start = c(time(NVIDIA_ts)[train_size + 1]))

# Check the alignment
NVIDIA_df <- data.frame(Date = time(NVIDIA_ts), Close = as.numeric(NVIDIA_ts))
exog_df <- data.frame(Date = time(exog_ts), Exog = as.numeric(exog_ts))
ggplot() +
  geom_line(data = NVIDIA_df, aes(x = Date, y = Close, color = "NVIDIA Close Price")) +
  geom_line(data = exog_df, aes(x = Date, y = Exog * max(NVIDIA_df$Close) / max(exog_df$Exog), color = "Exogenous Variable")) +
  scale_color_manual(values = c("NVIDIA Close Price" = "red", "Exogenous Variable" = "blue")) +
  labs(title = "NVIDIA Close Price and Exogenous Variable", x = "Time") +
  theme_minimal() +
  theme(axis.title.y.right = element_text(color = "red"),
        axis.text.y.right = element_text(color = "red"))

# Fit Auto ARMAX model
auto_armax_model <- auto.arima(train_data, xreg = exog_train_data, max.p=2, ic = c("aicc", "aic", "bic"), trace=TRUE )

# Summary of the model
summary(auto_armax_model)

# Generate in-sample predictions
fitted_values_autoarmax <- fitted(auto_armax_model)

# Plot the fitted ARIMA model over the real data
autoplot(train_data, series = "Actual") + 
  autolayer(fitted_values_autoarmax, series = "Fitted") + 
  scale_color_manual(values = c("black", "green")) +  # Different colors for actual vs fitted
  ggtitle("ARMAX (0,1,0) Fitted Values vs Actual") +
  xlab("Time") + ylab("NVIDIA Close Price") +
  theme_minimal()  # A cleaner theme for better visualization

# Metrics of the modeling
aic_autoarmax_tech <- AIC(auto_armax_model)
rmse_autoarmax_tech <- sqrt(mean((train_data - fitted_values_autoarmax)^2))
mape_autoarmax_tech <- mean(abs((train_data - fitted_values_autoarmax) / train_data)) * 100

cat("AIC:", aic_autoarmax_tech, "\n")
cat("RMSE:", rmse_autoarmax_tech, "\n")
cat("MAPE:", mape_autoarmax_tech, "%\n")

# Plot diagnostics
tsdiag(auto_armax_model)

# STUDY THE RESIDUALS
# Plot residuals
autoplot(residuals(auto_armax_model)) +
  ggtitle("Residuals of Auto.armax(0,1,1) with drift Model") +
  xlab("Time") + ylab("Residuals") +
  theme_minimal()

# Check the autocorrelation of residuals (should be like white noise)
Acf(residuals(auto_armax_model), main = "ACF of Residuals")

# Perform a "Box-Pierce" test for autocorrelation in residuals
Box.test(residuals(auto_armax_model), lag = 1, type = "Box-Pierce") 

# Perform the Durbin-Watson test on residuals
dwtest(residuals(auto_armax_model) ~ 1)

# Check for normality of residuals (e.g., using a histogram or Shapiro-Wilk test)
hist(residuals(auto_armax_model), breaks = 20, main = "Histogram of Residuals", col = "lightblue")
shapiro.test(residuals(auto_armax_model))  # Normality test

# Based on DW  you do not have strong evidence of positive autocorrelation in your residuals.

# FORECAST on the test data
forecast_auto_armax <- forecast(auto_armax_model, xreg = exog_test_data, h = length(test_data))

print(forecast_auto_armax)

# Plot only the last part of the data to focus on forecast and actual
autoplot(forecast_auto_armax) + 
  autolayer(test_data, series = "Actual") + 
  scale_color_manual(values = c("black", "red")) +  # Different colors for forecast vs actual
  ggtitle("armax(0,1,0) Forecast vs Actual") +
  xlab("Time") + ylab("NVIDIA Close Price") +
  coord_cartesian(xlim = c(time(NVIDIA_ts)[length(NVIDIA_ts) - 252], time(NVIDIA_ts)[length(NVIDIA_ts)])) +  # Restrict to the last year
  theme_minimal()  # A cleaner theme for better visualization

# Metrics of the forecast
aic_autoarmax_tech <- AIC(auto_armax_model)
rmse_fore_autoarmax_tech <- sqrt(mean((test_data - forecast_auto_armax$mean)^2))
mape_fore_autoarmax_tech <- mean(abs((test_data - forecast_auto_armax$mean) / test_data)) * 100

# Print metrics
cat("AIC:", aic_autoarmax_tech, "\n")
cat("RMSE:", rmse_fore_autoarmax_tech, "\n")
cat("MAPE:", mape_fore_autoarmax_tech, "%\n")

######################### AUTO ARMAX with VIX Close Index ##############################

exog_ts <- ts(NVIDIA_data$CloseVix, start = start(NVIDIA_ts), end = end(NVIDIA_ts), frequency = 365)

exog_train_data <- window(exog_ts, end = c(time(NVIDIA_ts)[train_size]))
exog_test_data <- window(exog_ts, start = c(time(NVIDIA_ts)[train_size + 1]))

# Check the alignment
NVIDIA_df <- data.frame(Date = time(NVIDIA_ts), Close = as.numeric(NVIDIA_ts))
exog_df <- data.frame(Date = time(exog_ts), Exog = as.numeric(exog_ts))
ggplot() +
  geom_line(data = NVIDIA_df, aes(x = Date, y = Close, color = "NVIDIA Close Price")) +
  geom_line(data = exog_df, aes(x = Date, y = Exog * max(NVIDIA_df$Close) / max(exog_df$Exog), color = "Exogenous Variable")) +
  scale_color_manual(values = c("NVIDIA Close Price" = "red", "Exogenous Variable" = "blue")) +
  labs(title = "NVIDIA Close Price and Exogenous Variable", x = "Time") +
  theme_minimal() +
  theme(axis.title.y.right = element_text(color = "red"),
        axis.text.y.right = element_text(color = "red"))

# Fit Auto ARMAX model
auto_armax_model <- auto.arima(train_data, xreg = exog_train_data, ic = c("aicc", "aic", "bic"), trace=TRUE )

# Summary of the model
summary(auto_armax_model)

# Generate in-sample predictions
fitted_values_autoarmax <- fitted(auto_armax_model)

# Plot the fitted ARIMA model over the real data
autoplot(train_data, series = "Actual") + 
  autolayer(fitted_values_autoarmax, series = "Fitted") + 
  scale_color_manual(values = c("black", "green")) +  # Different colors for actual vs fitted
  ggtitle("ARMAX (0,1,0) Fitted Values vs Actual") +
  xlab("Time") + ylab("NVIDIA Close Price") +
  theme_minimal()  # A cleaner theme for better visualization

# Metrics of the modeling
aic_autoarmax_vix <- AIC(auto_armax_model)
rmse_autoarmax_vix <- sqrt(mean((train_data - fitted_values_autoarmax)^2))
mape_autoarmax_vix <- mean(abs((train_data - fitted_values_autoarmax) / train_data)) * 100

cat("AIC:", aic_autoarmax_vix, "\n")
cat("RMSE:", rmse_autoarmax_vix, "\n")
cat("MAPE:", mape_autoarmax_vix, "%\n")

# Plot diagnostics
tsdiag(auto_armax_model)

# STUDY THE RESIDUALS
# Plot residuals
autoplot(residuals(auto_armax_model)) +
  ggtitle("Residuals of Auto.armax(0,1,1) with drift Model") +
  xlab("Time") + ylab("Residuals") +
  theme_minimal()

# Check the autocorrelation of residuals (should be like white noise)
Acf(residuals(auto_armax_model), main = "ACF of Residuals")

# Perform a "Box-Pierce" test for autocorrelation in residuals
Box.test(residuals(auto_armax_model), lag = 1, type = "Box-Pierce") 

# Perform the Durbin-Watson test on residuals
dwtest(residuals(auto_armax_model) ~ 1)

# Check for normality of residuals (e.g., using a histogram or Shapiro-Wilk test)
hist(residuals(auto_armax_model), breaks = 20, main = "Histogram of Residuals", col = "lightblue")
shapiro.test(residuals(auto_armax_model))  # Normality test

# Based on DW  you do not have strong evidence of positive autocorrelation in your residuals.

# FORECAST on the test data
forecast_auto_armax <- forecast(auto_armax_model, xreg = exog_test_data, h = length(test_data))

print(forecast_auto_armax)

# Plot only the last part of the data to focus on forecast and actual
autoplot(forecast_auto_armax) + 
  autolayer(test_data, series = "Actual") + 
  scale_color_manual(values = c("black", "red")) +  # Different colors for forecast vs actual
  ggtitle("ARMAX(0,1,0) with Vix Close Index Forecast vs Actual") +
  xlab("Time") + ylab("NVIDIA Close Price") +
  coord_cartesian(xlim = c(time(NVIDIA_ts)[length(NVIDIA_ts) - 365], time(NVIDIA_ts)[length(NVIDIA_ts)])) +  # Restrict to the last year
  theme_minimal()  # A cleaner theme for better visualization

# Metrics of the forecast
aic_autoarmax_vic <- AIC(auto_armax_model)
rmse_fore_autoarmax_vix <- sqrt(mean((test_data - forecast_auto_armax$mean)^2))
mape_fore_autoarmax_vix <- mean(abs((test_data - forecast_auto_armax$mean) / test_data)) * 100

# Print metrics
cat("AIC:", aic_autoarmax_vix, "\n")
cat("RMSE:", rmse_fore_autoarmax_vix, "\n")
cat("MAPE:", mape_fore_autoarmax_vix, "%\n")

####################### AUTO ARMAX with BTC Close Index ##############################

# WRONG!!! 

exog_ts <- ts(NVIDIA_data$CloseBTC, start = start(NVIDIA_ts), end = end(NVIDIA_ts), frequency = 365)

exog_train_data <- window(exog_ts, end = c(time(NVIDIA_ts)[train_size]))
exog_test_data <- window(exog_ts, start = c(time(NVIDIA_ts)[train_size + 1]))

# Check the alignment
NVIDIA_df <- data.frame(Date = time(NVIDIA_ts), Close = as.numeric(NVIDIA_ts))
exog_df <- data.frame(Date = time(exog_ts), Exog = as.numeric(exog_ts))
ggplot() +
  geom_line(data = NVIDIA_df, aes(x = Date, y = Close, color = "NVIDIA Close Price")) +
  geom_line(data = exog_df, aes(x = Date, y = Exog * max(NVIDIA_df$Close) / max(exog_df$Exog), color = "Exogenous Variable")) +
  scale_color_manual(values = c("NVIDIA Close Price" = "red", "Exogenous Variable" = "blue")) +
  labs(title = "NVIDIA Close Price and Exogenous Variable", x = "Time") +
  theme_minimal() +
  theme(axis.title.y.right = element_text(color = "red"),
        axis.text.y.right = element_text(color = "red"))

# Fit Auto ARMAX model
auto_armax_model <- auto.arima(train_data, xreg = exog_train_data, ic = c("aicc", "aic", "bic"), trace=TRUE )

# Summary of the model
summary(auto_armax_model)

# Generate in-sample predictions
fitted_values_autoarmax <- fitted(auto_armax_model)

# Plot the fitted ARIMA model over the real data
autoplot(train_data, series = "Actual") + 
  autolayer(fitted_values_autoarmax, series = "Fitted") + 
  scale_color_manual(values = c("black", "green")) +  # Different colors for actual vs fitted
  ggtitle("ARMAX (0,1,0) Fitted Values vs Actual") +
  xlab("Time") + ylab("NVIDIA Close Price") +
  theme_minimal()  # A cleaner theme for better visualization

# Metrics of the modeling
aic_autoarmax_BTC <- AIC(auto_armax_model)
rmse_autoarmax_BTC <- sqrt(mean((train_data - fitted_values_autoarmax)^2))
mape_autoarmax_BTC <- mean(abs((train_data - fitted_values_autoarmax) / train_data)) * 100

cat("AIC:", aic_autoarmax_BTC, "\n")
cat("RMSE:", rmse_autoarmax_BTC, "\n")
cat("MAPE:", mape_autoarmax_BTC, "%\n")

# Plot diagnostics
tsdiag(auto_armax_model)

# STUDY THE RESIDUALS
# Plot residuals
autoplot(residuals(auto_armax_model)) +
  ggtitle("Residuals of Auto.armax(0,1,1) with drift Model") +
  xlab("Time") + ylab("Residuals") +
  theme_minimal()

# Check the autocorrelation of residuals (should be like white noise)
Acf(residuals(auto_armax_model), main = "ACF of Residuals")

# Perform a "Box-Pierce" test for autocorrelation in residuals
Box.test(residuals(auto_armax_model), lag = 1, type = "Box-Pierce") 

# Perform the Durbin-Watson test on residuals
dwtest(residuals(auto_armax_model) ~ 1)

# Check for normality of residuals (e.g., using a histogram or Shapiro-Wilk test)
hist(residuals(auto_armax_model), breaks = 20, main = "Histogram of Residuals", col = "lightblue")
shapiro.test(residuals(auto_armax_model))  # Normality test

# Based on DW  you do not have strong evidence of positive autocorrelation in your residuals.

# FORECAST on the test data
forecast_auto_armax <- forecast(auto_armax_model, xreg = exog_test_data, h = length(test_data))

print(forecast_auto_armax)

# Plot only the last part of the data to focus on forecast and actual
autoplot(forecast_auto_armax) + 
  autolayer(test_data, series = "Actual") + 
  scale_color_manual(values = c("black", "red")) +  # Different colors for forecast vs actual
  ggtitle("ARMAX(0,1,0) with Vix Close Index Forecast vs Actual") +
  xlab("Time") + ylab("NVIDIA Close Price") +
  coord_cartesian(xlim = c(time(NVIDIA_ts)[length(NVIDIA_ts) - 365], time(NVIDIA_ts)[length(NVIDIA_ts)])) +  # Restrict to the last year
  theme_minimal()  # A cleaner theme for better visualization

# Metrics of the forecast
aic_autoarmax_BTC <- AIC(auto_armax_model)
rmse_fore_autoarmax_BTC <- sqrt(mean((test_data - forecast_auto_armax$mean)^2))
mape_fore_autoarmax_BTC <- mean(abs((test_data - forecast_auto_armax$mean) / test_data)) * 100

# Print metrics
cat("AIC:", aic_autoarmax_BTC, "\n")
cat("RMSE:", rmse_fore_autoarmax_BTC, "\n")
cat("MAPE:", mape_fore_autoarmax_BTC, "%\n")


##################################### cOMPARISION OF THE PERFORMANCE

# Modelling table
modelling_data <- data.frame(
  Model = c("Loess", "Spline Regression", "Smoothing Spline", "GAM w/TechSemi", 
            "GAM w/TechSemiVix", "GAM w/TechSemiVixBTC", "Gradient Boosting", "Holt-Winters", 
            "ARIMA (1,1,1)", "ARIMA (1,2,1)", "AutoARIMA", 
            "ARMAX w/Tech", "ARMAX w/Vix", "ARMAX w/BTC"),
  AIC = c(NA, NA, NA, aic_gam, aic_gam_2, aic_gam_3, NA, NA, 
          aic_111, aic_121, aic_autoarima, 
          aic_autoarmax_tech, aic_autoarmax_vix, aic_autoarmax_BTC),
  RMSE = c(rmse_loess, rmse_spline_r, rmse_spline, rmse_gam, 
           rmse_gam_2, rmse_gam_3, rmse_gb, rmse_hw, rmse_111, rmse_121, 
           rmse_autoarima, rmse_autoarmax_tech, rmse_autoarmax_vix, rmse_autoarmax_BTC),
  MAPE = c(mape_loess, mape_spline_r, mape_spline, mape_gam, 
           mape_gam_2, mape_gam_3, mape_gb, mape_hw, mape_111, mape_121, 
           mape_autoarima, mape_autoarmax_tech, mape_autoarmax_vix, mape_autoarmax_BTC)
)

# Create and print modelling table
kable(modelling_data, caption = "Modelling Results", align = c('l', 'c', 'c', 'c'))


# Forecasting table
forecasting_data <- data.frame(
  Model = c("Holt-Winters", "ARIMA (1,1,1)", "ARIMA (1,2,1)", 
            "AutoARIMA", "ARMAX w/Tech log", "ARMAX w/Vix", "ARMAX w/BTC"),
  RMSE = c(rmse_forecast_hw, rmse_forecast_111, rmse_forecast_121, 
           rmse_fore_autoarima, rmse_fore_autoarmax_tech, rmse_fore_autoarmax_vix, rmse_fore_autoarmax_BTC),
  MAPE = c(mape_forecast_hw, mape_forecast_111, mape_forecast_121, 
           mape_fore_autoarima, mape_fore_autoarmax_tech, mape_fore_autoarmax_vix, mape_fore_autoarmax_vix)
)

# Create and print forecasting table
kable(forecasting_data, caption = "Forecasting Results", align = c('l', 'c', 'c'))
