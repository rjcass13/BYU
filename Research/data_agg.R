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

temp
n_lon <- length(lon)
n_lat <- length(lat)
n_time <- length(time)

# time_short <- time[1:30]

# Make into a DF
# grid <- expand.grid(lon, lat, time_short)
# colnames(grid) <- c('lon', 'lat', 'time')
# grid$temp <- 0



# for (i in 1:n_time) {
#   for (j in 1:n_lat) {
#     for (k in 1:n_lon) {
#       grid$temp[k + (j-1) * n_lon + (i-1) * n_lat * n_lon] <- temp[k, j, i]
#     }
#   }
#   print('one time step done')
# }

# memory_usage_bytes <- object.size(grid)
# print(paste("Memory usage of df:", round(memory_usage_bytes / (1024^2), 2), "MB"))

# memory_usage_bytes <- object.size(temp)
# print(paste("Memory usage of df:", round(memory_usage_bytes / (1024^2), 2), "MB"))

# p <- image(temp_first_table)
# memory_usage_bytes <- object.size(p)
# print(paste("Memory usage of df:", round(memory_usage_bytes / (1024^2), 2), "MB"))

# class(temp)
# df_long <- reshape2::melt(temp)
# df_long$Var1 <- df_long$Var1 / 4 - .25
# df_long$Var2 <- df_long$Var2 / 4 - .25
# max(df_long$Var3)
# colnames(df_long) <- c('lon', 'lat', 'time', 'temp')



data_red <- data.frame(matrix(ncol = 5, nrow = 0))

week_val <- 1
for (week_ind in 1:52) {
  data_week <- data.frame(matrix(ncol = 5, nrow = 0))

  if (week_ind == 52) {
    week_ind_range <- (week_ind * 7 - 6):(n_time) # If it's the last index, include the last rows
  } else {
    week_ind_range <- (week_ind * 7 - 6):(week_ind * 7)
  }
  
  lat_val <- 89.5
  for (lat_ind in 1:180) {
    # The latitude values range from -90 to 90. Set to the center value, -89.5 to 89.5.
    # Index 1 >> 89.5, 180 720 >> 89.5. There are 721 rows so each 

    # Get the latitude indices to average over
    if (lat_ind == 180) {
      lat_ind_range <- (lat_ind * 4 - 3):(721) # If its the last index, include the last row
    } else {
      lat_ind_range <- (lat_ind * 4 - 3):(lat_ind * 4)
    }

    lon_val <- .5
    data_lat <- data.frame(matrix(ncol = 5, nrow = 360))
    for (lon_ind in 1:360) {
      # The longitude values range from 0 to 360. Set to the center value, -89.5 to 89.5.

      lon_ind_range <- (lon_ind * 4 - 3):(lon_ind * 4) # Get the longitude indices to average over
      avg_temp <- mean(temp[lon_ind_range, lat_ind_range, week_ind_range])
      avg_precip <- mean(temp[lon_ind_range, lat_ind_range, week_ind_range])

      #reduced_row <- (week_ind - 1) * 360 * 180 + (lat_ind - 1) * 360 + lon_ind 
      data_lat[lon_ind, ] <- c(lat_val, lon_val, week_ind, avg_temp, avg_precip) # Find the mean value on that grid

      lon_val <- lon_val + 1 # Increment Longitude
    }
    lat_val <- lat_val - 1 # Increment Latitude
    data_week <- rbind(data_week, data_lat)
    rm(data_lat)
  }
  cat("Done: Week", week_val)
  week_val <- week_val + 1 # Increment Week
  data_red <- rbind(data_red, data_week)
  rm(data_week)
}

colnames(data_red) <- c('lat', 'lon', 'date', 'temp', 'precip')
