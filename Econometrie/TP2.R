# ---- Create Output Directory ----
dir.create("outputs_tp2", showWarnings = FALSE)

# ---- 1. Load and Clean Data ----

# Load UK Interest Rates
uk_interest_rates <- scan("UKinterestrates.dat.txt", what = numeric(), quiet = TRUE)

# Load Simulation Data
simulation_data <- scan("simulation.dat.txt", what = numeric(), quiet = TRUE)

# Load Serie1 and Serie2
serie1 <- scan("serie1.dat.txt", what = numeric(), quiet = TRUE)
serie2 <- scan("serie2.dat.txt", what = numeric(), quiet = TRUE)

# Load and Clean San Francisco Data
sanfran_lines <- readLines("sanfran.dat.txt")
sanfran_data <- as.numeric(unlist(strsplit(sanfran_lines, "\\s+")))
sanfran_data <- sanfran_data[!is.na(sanfran_data)]  # Remove NA values caused by non-numeric strings

# Debugging: Print first few values of each dataset
print("UK Interest Rates:")
print(head(uk_interest_rates))

print("Simulation Data:")
print(head(simulation_data))

print("Serie1:")
print(head(serie1))

print("Serie2:")
print(head(serie2))

print("San Francisco Data:")
print(head(sanfran_data))

# ---- 2. Convert to Time Series ----

uk_ts <- ts(uk_interest_rates, start = c(1930, 1), frequency = 12)
simulation_ts <- ts(simulation_data)
serie1_ts <- ts(serie1)
serie2_ts <- ts(serie2)
sanfran_ts <- ts(sanfran_data, start = c(1932, 1), frequency = 12)

# ---- 3. Visualizations and Summary Statistics ----

# 3.1 UK Interest Rates
png("outputs_tp2/uk_interest_rates.png", width = 800, height = 600)
plot(uk_ts, main = "UK Interest Rates (1930+)", ylab = "Interest Rate", col = "blue")
dev.off()
summary_uk <- summary(uk_ts)
write.csv(as.data.frame(t(summary_uk)), "outputs_tp2/summary_uk_interest_rates.csv")

# 3.2 Simulation Data
if (length(simulation_data) > 0) {
  png("outputs_tp2/simulation_data.png", width = 800, height = 600)
  plot(simulation_ts, main = "Simulation Data", col = "darkgreen")
  dev.off()
  summary_simulation <- summary(simulation_ts)
  write.csv(as.data.frame(t(summary_simulation)), "outputs_tp2/summary_simulation_data.csv")
}

# 3.3 Serie1
if (length(serie1) > 0) {
  png("outputs_tp2/serie1.png", width = 800, height = 600)
  plot(serie1_ts, main = "Serie1 Data", col = "purple")
  dev.off()
  summary_serie1 <- summary(serie1_ts)
  write.csv(as.data.frame(t(summary_serie1)), "outputs_tp2/summary_serie1.csv")
}

# 3.4 Serie2
if (length(serie2) > 0) {
  png("outputs_tp2/serie2.png", width = 800, height = 600)
  plot(serie2_ts, main = "Serie2 Data", col = "brown")
  dev.off()
  summary_serie2 <- summary(serie2_ts)
  write.csv(as.data.frame(t(summary_serie2)), "outputs_tp2/summary_serie2.csv")
}

# 3.5 San Francisco Precipitation
png("outputs_tp2/sanfran_precipitation.png", width = 800, height = 600)
plot(sanfran_ts, main = "San Francisco Monthly Precipitation (1932-1966)", col = "cyan")
dev.off()
summary_sanfran <- summary(sanfran_ts)
write.csv(as.data.frame(t(summary_sanfran)), "outputs_tp2/summary_sanfran_precipitation.csv")

# ---- 4. Advanced Analysis: Trends and Seasonality ----

# 4.1 Decompose San Francisco Precipitation
sanfran_decomp <- decompose(sanfran_ts)
png("outputs_tp2/sanfran_decomposition.png", width = 800, height = 600)
plot(sanfran_decomp)
dev.off()
write.csv(data.frame(Trend = sanfran_decomp$trend, 
                     Seasonal = sanfran_decomp$seasonal, 
                     Random = sanfran_decomp$random), 
          "outputs_tp2/sanfran_decomposition.csv")

# 4.2 Holt-Winters Smoothing for UK Interest Rates
uk_hw <- HoltWinters(uk_ts)
png("outputs_tp2/uk_holtwinters.png", width = 800, height = 600)
plot(uk_hw, main = "Holt-Winters Forecast for UK Interest Rates")
dev.off()
uk_forecast <- data.frame(Forecast = tail(uk_hw$fitted[, 1], 12))
write.csv(uk_forecast, "outputs_tp2/uk_forecast.csv")

# ---- 5. Auto-Correlation and Seasonality Analysis ----

# 5.1 Auto-correlation for UK Interest Rates
png("outputs_tp2/uk_acf.png", width = 800, height = 600)
acf(uk_ts, main = "Auto-Correlation: UK Interest Rates")
dev.off()

# 5.2 Auto-correlation for San Francisco Precipitation
png("outputs_tp2/sanfran_acf.png", width = 800, height = 600)
acf(sanfran_ts, main = "Auto-Correlation: San Francisco Precipitation")
dev.off()

# ---- 6. Save Outputs ----
cat("All outputs have been saved in the 'outputs_tp2' directory.\n")
