# =====================================================================
#  REPORT CHARTS  (run AFTER road_accident_analyzer.R)
#  Creates report-style images:
#    11_panel_2x2.png          -> 4 charts combined in ONE image
#    12_accidents_by_city.png  -> single bar chart
#    13_accidents_over_time.png-> single time plot
#    14_casualties_histogram.png -> single histogram
#  Needs road_accidents.csv in the working folder (created by the
#  main script). Uses only base R.
# =====================================================================

out_dir <- "output"
if (!dir.exists(out_dir)) dir.create(out_dir)
if (!file.exists("road_accidents.csv"))
  stop("road_accidents.csv not found. Run road_accident_analyzer.R first.")

df <- read.csv("road_accidents.csv", stringsAsFactors = FALSE)
df$Date     <- as.Date(df$Date)
df$Severity <- factor(df$Severity, levels = c("Minor", "Serious", "Fatal"))

# ---- data for the charts ---------------------------------------------
city_tab <- sort(table(df$City), decreasing = TRUE)               # bar chart
ym       <- format(df$Date, "%Y-%m")
time_tab <- table(ym)                                             # monthly counts
tick_at  <- seq(1, length(time_tab), by = 6)                      # x-axis ticks

# ---- individual plotting functions (same style as the report) --------
plot_city <- function(cex = 1) {
  barplot(city_tab, col = "skyblue", main = "Accidents by City",
          ylab = "Number of Accidents", las = 2, cex.names = 0.8 * cex)
}
plot_hist <- function() {
  hist(df$Casualties, col = "lightgreen", border = "white",
       main = "Casualties Distribution", xlab = "Casualties per Accident",
       ylab = "Frequency", breaks = seq(0, max(df$Casualties) + 1, by = 1))
}
plot_box <- function() {
  boxplot(Casualties ~ Severity, data = df, col = "orange",
          main = "Casualties by Severity", xlab = "Severity",
          ylab = "Casualties", las = 1)
}
plot_time <- function() {
  plot(seq_along(time_tab), as.numeric(time_tab), type = "b", pch = 16,
       col = "blue", xaxt = "n", main = "Accidents Over Time",
       xlab = "Month", ylab = "Number of Accidents", las = 1)
  axis(1, at = tick_at, labels = names(time_tab)[tick_at], cex.axis = 0.8)
}

# ---- 1. Panel: 4 charts in one image ---------------------------------
png(file.path(out_dir, "11_panel_2x2.png"), width = 1000, height = 800, res = 110)
par(mfrow = c(2, 2), mar = c(6, 4.5, 3, 1), oma = c(0.5, 0.5, 0.5, 0.5))
plot_city(cex = 0.9)
plot_hist()
plot_box()
plot_time()
box("outer", lwd = 1.5)
dev.off()

# ---- 2. Single charts (one per image, big and clear) -----------------
single <- function(file, fun) {
  png(file.path(out_dir, file), width = 1000, height = 700, res = 110)
  par(mar = c(6, 5, 4, 2), oma = c(0.5, 0.5, 0.5, 0.5))
  fun()
  box("outer", lwd = 1.5)
  dev.off()
  cat("saved:", file, "\n")
}
single("12_accidents_by_city.png",       function() plot_city())
single("13_accidents_over_time.png",     plot_time)
single("14_casualties_histogram.png",    plot_hist)
cat("saved: 11_panel_2x2.png\nDone! Check the 'output' folder.\n")
