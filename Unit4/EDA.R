# read in the data
crash <- read.table('Crash.txt',header=T)

# Remove Case Number
crash <- crash[ , !names(crash) %in% c("CASENUM")]
# Type of Intersection should be factor
# REST_USE: number represents type of restraint. Should be factor
# AIR_BAG: WHICH airbag deployed. Factor
# VTRAFWAY: Type of Highway (factor)
# VNUM_LAN: # of lanes, factor
# VSPD_LIM: Speed limit, only have in 5-75, so can treat as numeric (no categoriical)
# VALIGN: Straight vs. Curve, etc. (factor)
# VSURCOND: Roadway condition (factor)
# ALCOHOL: Factor
# HOUR: hour of day. Treat as factor?
# Basically, all factors except Speed Limit and maybe hour

# Columns to exclude from conversion (e.g., col2 and col4)
cols_to_exclude <- c("VSPD_LIM")
# Get names of columns to convert
cols_to_convert <- setdiff(names(crash), cols_to_exclude)
# Convert selected columns to factors
crash[cols_to_convert] <- lapply(crash[cols_to_convert], as.factor)
