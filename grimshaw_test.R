library(tidyverse)
library(dplyr)
region <- read_csv("https://grimshawville.byu.edu/testregion.csv")
plot(region, pch = 15, col = "gray", xlim = c(34, 97), ylim = c(34, 49))


# Find average difference between x values and y values
# For each test point, see if there is another x/y pair within the confidence interval of that point
n = length(region)

# Sort the x values and get the differences between consecutive values
x_diff = region$x |> sort() |> diff()
# Find the average x difference after removing 0s
x_diff_mean = mean(x_diff[x_diff > 0])
x_diff_sd = sd(x_diff[x_diff > 0])

# Sort the y values and get the differences between consecutive values
y_diff = region$y |> sort() |> diff()
# Find the average y difference after removing 0s
y_diff_mean = mean(y_diff[y_diff > 0])
y_diff_sd = sd(y_diff[y_diff > 0])


check_point = function(test_x, test_y, n_region, region, x_diff_mean, x_diff_sd, y_diff_mean, y_diff_sd, alpha) {
  # Based on the point to check, get the nearest value in each quadrant around that point
  # For the nearest point in each quadrant, see if that point is within range of the average
  # distance between points in the region 
  
  # Get the closest points to the test point in each quadrant (top left, top right, bottom left, bottom right)
  x_greater = region[region$x >= test_x, ]
  x_lesser = region[region$x <= test_x, ]
  # Quadrant 1: Top Right
  quad_1 = x_greater[x_greater$y >= test_y, ]
  # Quadrant 2: Top Left
  quad_2 = x_lesser[x_lesser$y >= test_y, ]
  # Quadrant 3: Top Right
  quad_3 = x_lesser[x_lesser$y <= test_y, ]
  # Quadrant 4: Top Left
  quad_4 = x_greater[x_greater$y <= test_y, ]
  
  # Find the nearest point in each quadrant
  quad_1_distance = min((quad_1$x - test_x) ^2 + (quad_1$y - test_y) ^2)
  quad_2_distance = min((quad_2$x - test_x) ^2 + (quad_2$y - test_y) ^2)
  quad_3_distance = min((quad_3$x - test_x) ^2 + (quad_3$y - test_y) ^2)
  quad_4_distance = min((quad_4$x - test_x) ^2 + (quad_4$y - test_y) ^2)

  # Find the 'within range' interval to apply to each dimension
  # Range = x^2 + y^2
  distance_mean = x_diff_mean^2 + y_diff_mean^2

  quad_1_in_range = quad_1_distance < distance_mean
  quad_2_in_range = quad_2_distance < distance_mean
  quad_3_in_range = quad_3_distance < distance_mean
  quad_4_in_range = quad_4_distance < distance_mean

  # Return TRUE if there is a point within range on all 4 sides
  #test_x_right & test_x_left & test_y_above & test_y_below
  quad_1_in_range & quad_2_in_range & quad_3_in_range & quad_4_in_range
}

print(check_point(40, 45, n, region, x_diff_mean, x_diff_sd, y_diff_mean, y_diff_sd, .05))




