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



time_short <- time[1:30]

# Make into a DF
grid <- expand.grid(lon, lat, time_short)
colnames(grid) <- c('lon', 'lat', 'time')
grid$temp <- 0

n_lon <- length(lon)
n_lat <- length(lat)
n_time <- length(time_short)

for (i in 1:n_time) {
  for (j in 1:n_lat) {
    for (k in 1:n_lon) {
      grid$temp[k + (j-1) * n_lon + (i-1) * n_lat * n_lon] <- temp[k, j, i]
    }
  }
  print('one time step done')
}

memory_usage_bytes <- object.size(grid)
print(paste("Memory usage of df:", round(memory_usage_bytes / (1024^2), 2), "MB"))

memory_usage_bytes <- object.size(temp)
print(paste("Memory usage of df:", round(memory_usage_bytes / (1024^2), 2), "MB"))

p <- image(temp_first_table)
memory_usage_bytes <- object.size(p)
print(paste("Memory usage of df:", round(memory_usage_bytes / (1024^2), 2), "MB"))

class(temp)
df_long <- reshape2::melt(temp)
df_long$Var1 <- df_long$Var1 / 4 - .25
df_long$Var2 <- df_long$Var2 / 4 - .25
max(df_long$Var3)
colnames(df_long) <- c('lon', 'lat', 'time', 'temp')


.

[1][2][3][4][][][][]...
[5][6][7][8][][][][]...
[][][][][][][][]...
[][][][]...
[][][][]...
[][][][]...