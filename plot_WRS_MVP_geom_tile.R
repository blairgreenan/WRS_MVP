# plot_WRS_MVP.R
# Blair Greenan
# Fisheries and Oceans Canada
# 24 Aug 2026
#
# This script creates plots of data collected during MVP Tows 4 & 5 in the Western Ross Sea

# load the tidyverse and cmocean packages
library(tidyverse)
library(ggplot2)
library(cmocean)
library(akima)
library(patchwork)
library(metR)
library(scales)

######## Load the Rdata file
load("C:/Science Projects/Ross Sea/Documents/Papers/Ross Bank/Figures/MVP/MVP.RData")

# Adjust the longitudes as they or 0 to 360
neg_lon <- which(MVP_tidy_tibble$lon<0)
MVP_tidy_tibble$lon[neg_lon] <- 360 + MVP_tidy_tibble$lon[neg_lon]

# Select Start and End time for plot
# Ross Sea Ice Shelf Survey
#Start_time <- as.POSIXct("2012-01-22", tz="UTC")
#End_time <- as.POSIXct("2012-01-25", tz="UTC")
# Ross Bank Survey
Start_time <- as.POSIXct("2012-01-11 23:00:00", tz="UTC")
End_time <- as.POSIXct("2012-01-15 01:00:00", tz="UTC")

# Get the index values for the range of times
MVP_survey_time <- which(MVP_tidy_tibble$daten>=Start_time & MVP_tidy_tibble$daten<=End_time)

MVP_lat <- MVP_tidy_tibble$lat[MVP_survey_time]
MVP_lon <- MVP_tidy_tibble$lon[MVP_survey_time]
MVP_depth <- MVP_tidy_tibble$pres[MVP_survey_time]
MVP_temp <- MVP_tidy_tibble$temp[MVP_survey_time]
MVP_sal <- MVP_tidy_tibble$sal[MVP_survey_time]
MVP_chl <- MVP_tidy_tibble$chl[MVP_survey_time]
# Add the MVP fluorometer calibration (see RossSea_Bottle_Chl_MVP_FL_calibration.R)
MVP_chl <- 0.51 + (0.42*MVP_chl)
MVP_sigmat <- MVP_tidy_tibble$sigmat[MVP_survey_time]
MVP_lopc <- MVP_tidy_tibble$lopc[MVP_survey_time]

# limit the data to the upper 100m
MVP_lat_100 <- MVP_lat[MVP_depth<150]
MVP_lon_100 <- MVP_lon[MVP_depth<150]
MVP_depth_100 <- MVP_depth[MVP_depth<150]
MVP_temp_100 <- MVP_temp[MVP_depth<150]
MVP_sal_100 <- MVP_sal[MVP_depth<150]
MVP_chl_100 <- MVP_chl[MVP_depth<150]
MVP_sigmat_100 <- MVP_sigmat[MVP_depth<150]
MVP_lopc_100 <- MVP_lopc[MVP_depth<150]



# Temperature
df1 <- tibble(
  lon = MVP_lon_100,
  depth = MVP_depth_100,
  temp = MVP_temp_100
  )

ggp1 <- ggplot(df1,
       aes(x = lon, y = depth, fill = temp)) +
  geom_tile() +
  scale_y_reverse() +
  scale_fill_cmocean(
    name = "thermal",
    limits = c(-2, 1.5),
    oob = scales::squish,
  ) +
  labs(
    x = "Longitude (°E)",
    y = "Depth (m)"
  ) +
  guides(fill = guide_colourbar(direction = "horizontal", title.position = "top", title=expression("Temperature (\u00B0C)")))

# Salinity
df2 <- tibble(
  lon = MVP_lon_100,
  depth = MVP_depth_100,
  sal = MVP_sal_100
)

ggp2 <- ggplot(df2,
              aes(x = lon, y = depth, fill = sal)) +
  geom_tile() +
  scale_y_reverse() +
  scale_fill_cmocean(
    name = "haline",
    limits = c(34.2, 34.5),
    oob = scales::squish
  ) +
  labs(
    x = "Longitude (°E)",
    y = "Depth (m)"
  ) +
  guides(fill = guide_colourbar(direction = "horizontal", title.position = "top", title=expression("Salinity")))

# Chl a
df3 <- tibble(
  lon = MVP_lon_100,
  depth = MVP_depth_100,
  chl = MVP_chl_100
#  chl = log10(MVP_chl_100)
)

ggp3 <- ggplot(df3,
               aes(x = lon, y = depth, fill = chl)) +
  geom_tile() +
  scale_y_reverse() +
  scale_fill_cmocean(
    name = "algae",
    limits = c(1, 12),
    oob = scales::squish
  ) +
  labs(
    x = "Longitude (°E)",
    y = "Depth (m)"
  ) +
  guides(fill = guide_colourbar(direction = "horizontal", title.position = "top", title=expression("Chlorophyll (mg m"^"-3"*")")))

# sigmat
df4 <- tibble(
  lon = MVP_lon_100,
  depth = MVP_depth_100,
  sigmat = MVP_sigmat_100
)

ggp4 <- ggplot(df4,
               aes(x = lon, y = depth, fill = sigmat)) +
  geom_tile() +
#  geom_contour(aes(z = sigmat),
#               colour = "black",
#               alpha = 0.5) +
  scale_y_reverse() +
  scale_fill_cmocean(
    name = "dense",
    limits = c(27.3, 27.8),
    breaks = c(27.3, 27.5, 27.7),
    oob = scales::squish
  ) +
  labs(
    x = "Longitude (°E)",
    y = "Depth (m)"
  ) +
  guides(fill = guide_colourbar(direction = "horizontal", title.position = "top", title=expression("Density (kg m"^"-3"*")")))

# lopc
# NaNs in lopc data need to be removed
good <- is.finite(MVP_lon_100) &
  is.finite(MVP_depth_100) &
  is.finite(MVP_lopc_100)

df5 <- tibble(
  lon   = MVP_lon_100[good],
  depth = MVP_depth_100[good],
  lopc  = MVP_lopc_100[good]
)

ggp5 <- ggplot(df5,
               aes(x = lon, y = depth, fill = lopc)) +
  geom_tile() +
  scale_y_reverse() +
  scale_fill_cmocean(
    name = "matter",
    limits = c(0, 5000)
  ) +
  labs(
    x = "Longitude (°E)",
    y = "Depth (m)"
  )

# plot results
ggp1/ggp2/ggp4/ggp3 + plot_layout(axes = "collect")
ggsave("MVP_geom_tile.png", units="in", width=8.8, height=5.65, dpi=1200, scale = 1.25)

