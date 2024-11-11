library("googledrive")
library("mgcv")
library("purrr")
library("lubridate")
library("readr")
library("dplyr")
library("mgcViz")

# Get the latest file's index
ind <- drive_ls("~/WeightTracker/") |> 
  pluck("drive_resource") |> 
  map(pluck, "createdTime") |>
  unlist() |> 
  lubridate::ymd_hms() |> 
  which.max()

# Select the cached credentials
2

# Get its identifier
file_id <- drive_ls("~/WeightTracker/") |> 
  pluck("id", ind)

# Download to a temporary file and read it
temp_file <- tempfile(fileext = ".csv")
drive_download(as_id(file_id), path = temp_file, overwrite = TRUE)

# Read into a data frame
data <- read_csv(temp_file, col_select = c("Time of Measurement", "Weight(kg)"))
unlink(temp_file)
colnames(data) <- c("dateTime", "weight")

# Compute time of day on (0,1) scale
comp_hour_of_day <- function(dateTime){
  (hour(dateTime) +
     minute(dateTime) / 60 +
     second(dateTime) / (60 ^ 2))
}


ndf <- data |> 
  dplyr::mutate(
    days_since_purchase = as.numeric(julian(dateTime, origin=min(dateTime))),
    hour_of_day = comp_hour_of_day(dateTime))

mod <- gam(weight~s(days_since_purchase, bs="cr", k=20)+s(hour_of_day, bs="cc"), 
           data=ndf, 
           method="REML") 
jpeg("weight_since24oct2024.jpg", width=700, height = 500)
plot(mod, select=1, seWithMean = TRUE, shift = coef(mod)[1], 
     rug = TRUE, residuals = TRUE,
     pch = 19, cex = 1, col=rgb(0,0,0, alpha=0.6),
     shade = TRUE, shade.col = "lightblue", 
     ylab="Weight (kg)", 
     xlab="#Days since 24 October 2024",
     main = "Mean weight (conditional on time of day)")
dev.off()