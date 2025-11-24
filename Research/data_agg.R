# Built following this example: https://rokuk.org/projects/climateviz/
# Animation: https://www.r-bloggers.com/2021/05/animated-graph-gif-with-gganimate-ggplot/

library(ncdf4)
library(ggplot2)
library(gganimate)
library(gifski)
library(reshape2)

# Open file
data <- nc_open("daily_stats_2024_temp.nc")
print(data)

# Get vars
temp <- ncvar_get(data, "t2m") # Degrees Kelvin
lon <- ncvar_get(data, "longitude") # degrees east
lat <- ncvar_get(data, "latitude") # degrees north
time <- ncvar_get(data, "valid_time") # days since 2024-01-01

# Close file
nc_close(data)


for (i in 1:60) {
  temp_display <- temp[,,i]
  temp_display <- temp_display[length(lon):1 , ]
  temp_display <- temp_display[, length(lat):1]
  image(temp_display)
}


# Provo: 40.25 N, 248.25 E (111.75 W)
start_date <- as.Date("2024-01-01")

provo_temp <- temp[248.25*4 + 1, 40.25*4 + 1, ]
plot(time + start_date, provo_temp)



n_lon <- length(lon)
n_lat <- length(lat)
n_time <- length(time)

data_red <- matrix(NA, 0, 5) 

week_val <- 1
for (week_ind in 1:52) {
  lat_ind_range <- ifelse(week_ind == 52, (week_ind * 7 - 6):n_time, (week_ind * 7 - 6):(week_ind * 7))

  lat_val <- -89.5
  data_week <- matrix(NA, 0, 5) 
  for (lat_ind in 1:180) {
    lat_ind_range <- ifelse (lat_ind == 180, (lat_ind * 4 - 3):721, (lat_ind * 4 - 3):(lat_ind * 4))

    lon_val <- .5
    data_lat <- matrix(NA, 360, 5) 
    for (lon_ind in 1:360) {
      lon_ind_range <- (lon_ind * 4 - 3):(lon_ind * 4)

      avg <- mean(temp[lon_ind_range, lat_ind_range, week_ind_range])
      data_lat[lon_ind,] <- c(lat_val, lon_val, week_val, )

      lon_val <- lon_val +1
    }

    data_week <- rbind(data_week, data_lat)
    lat_val <- lat_val + 1
  }

  data_red <- rbind(data_red, data_week)
  cat("Done: Week", week_val)
  week_val <- week_val + 1
}


.

[1][2][3][4][][][][]...
[5][6][7][8][][][][]...
[][][][][][][][]...
[][][][]...
[][][][]...
[][][][]...