# =====================================================================
#  ROAD ACCIDENT DATA ANALYZER  (Mini Project in R)
#  ---------------------------------------------------------------
#  - Uses ONLY base R (no packages to install)
#  - If "road_accidents.csv" exists in the working folder, it is used.
#    Otherwise a realistic sample dataset is generated automatically.
#  - Outputs: console report + PNG charts + CSV summaries in /output
#
#  Required CSV columns (if you use your own data):
#    Date (YYYY-MM-DD), Time (HH:MM), City, Weather, Road_Condition,
#    Vehicle_Type, Severity (Minor/Serious/Fatal), Casualties, Speed_Limit
#
#  Run:  Rscript road_accident_analyzer.R
#        (or open in RStudio and click "Source")
# =====================================================================

set.seed(42)
out_dir <- "output"
if (!dir.exists(out_dir)) dir.create(out_dir)

# ---------------------------------------------------------------------
# 1. LOAD DATA (or generate sample data)
# ---------------------------------------------------------------------
generate_sample_data <- function(n = 3000) {
  cities   <- c("Bengaluru", "Tumkur", "Mysuru", "Hubballi", "Mangaluru", "Belagavi")
  weathers <- c("Clear", "Rain", "Fog", "Cloudy")
  roads    <- c("Dry", "Wet", "Damaged", "Under Construction")
  vehicles <- c("Two-Wheeler", "Car", "Truck", "Bus", "Auto", "Bicycle")

  dates <- sample(seq(as.Date("2023-01-01"), as.Date("2025-12-31"), by = "day"),
                  n, replace = TRUE)
  # more accidents in rush hours
  hours <- sample(0:23, n, replace = TRUE,
                  prob = c(2,1,1,1,2,3,5,8,9,6,5,5,5,5,5,6,8,10,10,8,6,4,3,2))
  mins  <- sample(0:59, n, replace = TRUE)

  weather <- sample(weathers, n, replace = TRUE, prob = c(.55, .25, .08, .12))
  road    <- ifelse(weather == "Rain",
                    sample(roads, n, TRUE, prob = c(.1, .75, .1, .05)),
                    sample(roads, n, TRUE, prob = c(.65, .05, .2, .1)))
  vehicle <- sample(vehicles, n, replace = TRUE, prob = c(.38, .25, .12, .07, .12, .06))
  city    <- sample(cities, n, replace = TRUE, prob = c(.35, .1, .15, .15, .13, .12))
  speed   <- sample(c(30, 40, 50, 60, 80, 100), n, replace = TRUE,
                    prob = c(.15, .2, .25, .2, .12, .08))

  # Severity depends on speed, weather and vehicle (simulated risk)
  risk <- 0.012 * speed +
          ifelse(weather == "Fog", 0.6, 0) +
          ifelse(weather == "Rain", 0.3, 0) +
          ifelse(vehicle %in% c("Two-Wheeler", "Bicycle"), 0.5, 0) +
          ifelse(vehicle == "Truck", 0.4, 0) +
          ifelse(hours %in% c(22, 23, 0, 1, 2, 3), 0.4, 0) +
          rnorm(n, 0, 0.5)
  severity <- cut(risk, breaks = c(-Inf, 1.0, 1.6, Inf),
                  labels = c("Minor", "Serious", "Fatal"))
  casualties <- ifelse(severity == "Minor",   rpois(n, 0.8) + 1,
                ifelse(severity == "Serious", rpois(n, 1.5) + 1,
                                              rpois(n, 2.0) + 1))

  data.frame(
    Date = dates,
    Time = sprintf("%02d:%02d", hours, mins),
    City = city, Weather = weather, Road_Condition = road,
    Vehicle_Type = vehicle, Severity = as.character(severity),
    Casualties = casualties, Speed_Limit = speed,
    stringsAsFactors = FALSE
  )
}

if (file.exists("road_accidents.csv")) {
  cat(">> Loading dataset: road_accidents.csv\n")
  df <- read.csv("road_accidents.csv", stringsAsFactors = FALSE)
} else {
  cat(">> road_accidents.csv not found. Generating sample dataset...\n")
  df <- generate_sample_data()
  write.csv(df, "road_accidents.csv", row.names = FALSE)
  cat(">> Sample data saved as road_accidents.csv\n")
}

# ---------------------------------------------------------------------
# 2. DATA CLEANING & FEATURE ENGINEERING
# ---------------------------------------------------------------------
cat("\n================ DATA CLEANING ================\n")
cat("Rows before cleaning:", nrow(df), "\n")
cat("Missing values per column:\n")
print(colSums(is.na(df)))

df$Date <- as.Date(df$Date)
df <- df[!is.na(df$Date), ]
df <- df[!duplicated(df), ]

df$Casualties[is.na(df$Casualties)] <- median(df$Casualties, na.rm = TRUE)
df$Severity <- factor(df$Severity, levels = c("Minor", "Serious", "Fatal"))
df <- df[!is.na(df$Severity), ]

df$Hour     <- as.integer(substr(df$Time, 1, 2))
df$Year     <- as.integer(format(df$Date, "%Y"))
df$Month    <- factor(months(df$Date, abbreviate = TRUE), levels = month.abb)
df$Weekday  <- factor(weekdays(df$Date),
                      levels = c("Monday", "Tuesday", "Wednesday", "Thursday",
                                 "Friday", "Saturday", "Sunday"))
df$TimeOfDay <- cut(df$Hour, breaks = c(-1, 5, 11, 16, 20, 23),
                    labels = c("Night (0-5)", "Morning (6-11)", "Afternoon (12-16)",
                               "Evening (17-20)", "Late Night (21-23)"))
df$IsFatal <- ifelse(df$Severity == "Fatal", 1, 0)

cat("Rows after cleaning:", nrow(df), "\n")

# ---------------------------------------------------------------------
# 3. OVERALL SUMMARY
# ---------------------------------------------------------------------
cat("\n================ OVERALL SUMMARY ================\n")
cat("Period            :", format(min(df$Date)), "to", format(max(df$Date)), "\n")
cat("Total accidents   :", nrow(df), "\n")
cat("Total casualties  :", sum(df$Casualties), "\n")
cat("Avg casualties/acc:", round(mean(df$Casualties), 2), "\n")
cat("Fatal accidents   :", sum(df$IsFatal),
    sprintf("(%.1f%%)\n", 100 * mean(df$IsFatal)))

cat("\nSeverity distribution:\n")
print(table(df$Severity))

# Helper: count + percentage + fatal rate for any grouping column
group_summary <- function(data, col) {
  tab <- aggregate(cbind(Accidents = 1, Casualties = data$Casualties,
                         Fatal = data$IsFatal) ~ data[[col]],
                   data = data, FUN = sum)
  names(tab)[1] <- col
  tab$Fatal_Rate_Pct <- round(100 * tab$Fatal / tab$Accidents, 1)
  tab[order(-tab$Accidents), ]
}

by_city    <- group_summary(df, "City")
by_weather <- group_summary(df, "Weather")
by_road    <- group_summary(df, "Road_Condition")
by_vehicle <- group_summary(df, "Vehicle_Type")
by_tod     <- group_summary(df, "TimeOfDay")

cat("\n--- Accidents by City ---\n");            print(by_city,    row.names = FALSE)
cat("\n--- Accidents by Weather ---\n");         print(by_weather, row.names = FALSE)
cat("\n--- Accidents by Road Condition ---\n");  print(by_road,    row.names = FALSE)
cat("\n--- Accidents by Vehicle Type ---\n");    print(by_vehicle, row.names = FALSE)
cat("\n--- Accidents by Time of Day ---\n");     print(by_tod,     row.names = FALSE)

write.csv(by_city,    file.path(out_dir, "summary_by_city.csv"),    row.names = FALSE)
write.csv(by_weather, file.path(out_dir, "summary_by_weather.csv"), row.names = FALSE)
write.csv(by_road,    file.path(out_dir, "summary_by_road.csv"),    row.names = FALSE)
write.csv(by_vehicle, file.path(out_dir, "summary_by_vehicle.csv"), row.names = FALSE)

# ---------------------------------------------------------------------
# 4. TIME-BASED ANALYSIS
# ---------------------------------------------------------------------
hour_tab    <- table(factor(df$Hour, levels = 0:23))
weekday_tab <- table(df$Weekday)
month_tab   <- table(df$Month)
year_tab    <- table(df$Year)

peak_hour <- as.integer(names(which.max(hour_tab)))
cat("\n================ TIME ANALYSIS ================\n")
cat("Peak accident hour :", sprintf("%02d:00 - %02d:59", peak_hour, peak_hour), "\n")
cat("Most dangerous day :", names(which.max(weekday_tab)), "\n")
cat("Worst month        :", names(which.max(month_tab)), "\n")
cat("Yearly trend:\n"); print(year_tab)

# ---------------------------------------------------------------------
# 5. STATISTICAL TESTS & MODEL
# ---------------------------------------------------------------------
cat("\n================ STATISTICAL ANALYSIS ================\n")

# (a) Is severity related to weather? (Chi-square test)
chi <- suppressWarnings(chisq.test(table(df$Weather, df$Severity)))
cat("Chi-square test: Weather vs Severity\n")
cat(sprintf("  X-squared = %.2f, df = %d, p-value = %.4g\n",
            chi$statistic, chi$parameter, chi$p.value))
cat("  ->", ifelse(chi$p.value < 0.05,
                   "Significant relationship (p < 0.05)",
                   "No significant relationship"), "\n")

# (b) Do fatal accidents have more casualties? (ANOVA)
aov_fit <- aov(Casualties ~ Severity, data = df)
cat("\nANOVA: Casualties ~ Severity\n")
print(summary(aov_fit))

# (c) Logistic regression: what increases the chance of a fatal accident?
if ("Speed_Limit" %in% names(df)) {
  model <- glm(IsFatal ~ Speed_Limit + Weather + Vehicle_Type + TimeOfDay,
               data = df, family = binomial)
  cat("\nLogistic Regression (probability of a FATAL accident):\n")
  coefs <- summary(model)$coefficients
  odds  <- data.frame(Predictor  = rownames(coefs),
                      Odds_Ratio = round(exp(coefs[, 1]), 3),
                      P_Value    = round(coefs[, 4], 4))
  print(odds, row.names = FALSE)

  # Predict risk for a sample scenario
  scenario <- data.frame(Speed_Limit = 80, Weather = "Fog",
                         Vehicle_Type = "Two-Wheeler",
                         TimeOfDay = factor("Late Night (21-23)",
                                            levels = levels(df$TimeOfDay)))
  p <- predict(model, scenario, type = "response")
  cat(sprintf("\nScenario: 80 km/h, Fog, Two-Wheeler, Late Night -> Fatal probability = %.1f%%\n",
              100 * p))
}

# ---------------------------------------------------------------------
# 6. VISUALISATIONS (saved as PNG in /output)
# ---------------------------------------------------------------------
cat("\n================ GENERATING CHARTS ================\n")
cols_sev <- c(Minor = "#4CAF50", Serious = "#FF9800", Fatal = "#E53935")

save_plot <- function(name, expr, w = 900, h = 600) {
  png(file.path(out_dir, name), width = w, height = h, res = 110)
  on.exit(dev.off())
  par(mar = c(5, 5, 4, 2))
  expr()
  cat("  saved:", name, "\n")
}

# 1. Accidents by hour
save_plot("01_accidents_by_hour.png", function() {
  bp <- barplot(hour_tab, col = ifelse(as.integer(names(hour_tab)) == peak_hour,
                                       "#E53935", "#42A5F5"),
                main = "Accidents by Hour of Day", xlab = "Hour (0-23)",
                ylab = "Number of Accidents", las = 1, border = NA)
  legend("topleft", legend = "Peak hour", fill = "#E53935", bty = "n")
})

# 2. Accidents by weekday
save_plot("02_accidents_by_weekday.png", function() {
  barplot(weekday_tab, col = "#7E57C2", main = "Accidents by Day of Week",
          ylab = "Number of Accidents", las = 2, border = NA)
})

# 3. Monthly trend (line)
save_plot("03_monthly_trend.png", function() {
  mt <- table(df$Month, df$Year)
  matplot(mt, type = "b", pch = 16, lty = 1, lwd = 2, xaxt = "n",
          col = c("#1E88E5", "#43A047", "#FB8C00", "#8E24AA")[seq_len(ncol(mt))],
          main = "Monthly Accident Trend by Year", xlab = "Month",
          ylab = "Accidents", las = 1)
  axis(1, at = 1:12, labels = month.abb)
  legend("topright", legend = colnames(mt), lty = 1, lwd = 2, pch = 16,
         col = c("#1E88E5", "#43A047", "#FB8C00", "#8E24AA")[seq_len(ncol(mt))], bty = "n")
})

# 4. Severity pie chart
save_plot("04_severity_pie.png", function() {
  st <- table(df$Severity)
  pct <- round(100 * st / sum(st), 1)
  pie(st, labels = paste0(names(st), " (", pct, "%)"),
      col = cols_sev[names(st)], main = "Accident Severity Distribution",
      border = "white")
}, 700, 600)

# 5. City-wise stacked by severity
save_plot("05_city_by_severity.png", function() {
  ct <- table(df$Severity, df$City)
  ct <- ct[, order(-colSums(ct))]
  barplot(ct, col = cols_sev, main = "Accidents by City and Severity",
          ylab = "Accidents", las = 2, border = NA,
          legend.text = TRUE, args.legend = list(x = "topright", bty = "n"))
})

# 6. Vehicle type fatal rate
save_plot("06_vehicle_fatal_rate.png", function() {
  v <- by_vehicle[order(-by_vehicle$Fatal_Rate_Pct), ]
  par(mar = c(5, 9, 4, 2))
  barplot(rev(v$Fatal_Rate_Pct), names.arg = rev(v$Vehicle_Type), horiz = TRUE,
          col = "#EF5350", las = 1, border = NA, xlab = "Fatal accident rate (%)",
          main = "Fatality Rate by Vehicle Type")
})

# 7. Weather vs severity (proportions)
save_plot("07_weather_vs_severity.png", function() {
  wt <- prop.table(table(df$Severity, df$Weather), margin = 2) * 100
  barplot(wt, col = cols_sev, main = "Severity Mix by Weather Condition (%)",
          ylab = "Percentage", las = 1, border = NA,
          legend.text = TRUE, args.legend = list(x = "topright", bty = "n"))
})

# 8. Heatmap: weekday x hour
save_plot("08_heatmap_weekday_hour.png", function() {
  hm <- table(df$Weekday, factor(df$Hour, levels = 0:23))
  image(x = 0:23, y = 1:7, z = t(as.matrix(hm)),
        col = hcl.colors(30, "YlOrRd", rev = TRUE),
        xlab = "Hour of Day", ylab = "", yaxt = "n",
        main = "Accident Hotspots: Day vs Hour")
  axis(2, at = 1:7, labels = levels(df$Weekday), las = 1, cex.axis = 0.8)
}, 1000, 600)

# 9. Speed limit vs average casualties
if ("Speed_Limit" %in% names(df)) {
  save_plot("09_speed_vs_casualties.png", function() {
    sp <- aggregate(Casualties ~ Speed_Limit, data = df, FUN = mean)
    plot(sp$Speed_Limit, sp$Casualties, type = "b", pch = 19, lwd = 2,
         col = "#00897B", xlab = "Speed Limit (km/h)",
         ylab = "Average Casualties per Accident",
         main = "Speed Limit vs Average Casualties", las = 1)
    abline(lm(Casualties ~ Speed_Limit, data = sp), lty = 2, col = "grey40")
  })
}

# ---------------------------------------------------------------------
# 7. KEY INSIGHTS REPORT
# ---------------------------------------------------------------------
worst_city    <- by_city$City[1]
deadliest_veh <- by_vehicle$Vehicle_Type[which.max(by_vehicle$Fatal_Rate_Pct)]
deadliest_wx  <- by_weather$Weather[which.max(by_weather$Fatal_Rate_Pct)]
deadliest_tod <- by_tod$TimeOfDay[which.max(by_tod$Fatal_Rate_Pct)]

report <- c(
  "=========== KEY INSIGHTS ===========",
  sprintf("1. Total accidents analysed        : %d", nrow(df)),
  sprintf("2. City with most accidents        : %s", worst_city),
  sprintf("3. Peak accident hour              : %02d:00", peak_hour),
  sprintf("4. Most dangerous weekday          : %s", names(which.max(weekday_tab))),
  sprintf("5. Highest fatality rate (vehicle) : %s", deadliest_veh),
  sprintf("6. Highest fatality rate (weather) : %s", deadliest_wx),
  sprintf("7. Highest fatality rate (time)    : %s", as.character(deadliest_tod)),
  "",
  "Suggestions:",
  sprintf("- Increase patrolling in %s during peak hours.", worst_city),
  sprintf("- Run safety campaigns for %s riders/drivers.", deadliest_veh),
  sprintf("- Add warning signs and speed control during %s conditions.", deadliest_wx)
)
cat("\n", paste(report, collapse = "\n"), "\n", sep = "")
writeLines(report, file.path(out_dir, "insights_report.txt"))



