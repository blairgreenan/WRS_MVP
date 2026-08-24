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


interp_temp <- with(
  df1,
  interp(
    x = lon,
    y = depth,
    z = temp,
    nx = 200,
    ny = 200
  )
)

interp_df1 <- expand.grid(
  lon = interp_temp$x,
  depth = interp_temp$y
)

interp_df1$temp <- as.vector(interp_temp$z)

ggp1 <- ggplot(df1,
       aes(x = lon, y = depth, color = temp)) +
  geom_point(size = 1) +
#  geom_contour(aes(z = temp),
#               colour = "black",
#               alpha = 0.5) +
  scale_y_reverse() +
  scale_color_gradientn(colors = cmocean("thermal")(100)) +
  labs(
    x = "Longitude (°E)",
    y = "Depth (m)"
  )

# Salinity
df2 <- tibble(
  lon = MVP_lon_100,
  depth = MVP_depth_100,
  sal = MVP_sal_100
)


interp_sal <- with(
  df2,
  interp(
    x = lon,
    y = depth,
    z = sal,
    nx = 200,
    ny = 200
  )
)

interp_df2 <- expand.grid(
  lon = interp_sal$x,
  depth = interp_sal$y
)

interp_df2$sal <- as.vector(interp_sal$z)

ggp2 <- ggplot(interp_df2,
              aes(x = lon, y = depth, fill = sal)) +
  geom_raster() +
#  geom_contour(aes(z = sal),
#               colour = "black",
#               alpha = 0.5) +
  scale_y_reverse() +
  scale_fill_cmocean(
    name = "haline"
  ) +
  labs(
    x = "Longitude (°E)",
    y = "Depth (m)"
  ) +
  theme_bw()


# Chl a
df3 <- tibble(
  lon = MVP_lon_100,
  depth = MVP_depth_100,
  chl = MVP_chl_100
#  chl = log10(MVP_chl_100)
)


interp_chl <- with(
  df3,
  interp(
    x = lon,
    y = depth,
    z = chl,
    nx = 300,
    ny = 300
  )
)

interp_df3 <- expand.grid(
  lon = interp_chl$x,
  depth = interp_chl$y
)

interp_df3$chl <- as.vector(interp_chl$z)

ggp3 <- ggplot(interp_df3,
               aes(x = lon, y = depth, fill = chl)) +
  geom_raster() +
#  geom_contour(aes(z = chlp),
#               colour = "black",
#               alpha = 0.5) +
  scale_y_reverse() +
  scale_fill_cmocean(
    name = "algae"
  ) +
  labs(
    x = "Longitude (°E)",
    y = "Depth (m)"
  ) +
  theme_bw()


# sigmat
df4 <- tibble(
  lon = MVP_lon_100,
  depth = MVP_depth_100,
  sigmat = MVP_sigmat_100
)


interp_sigmat <- with(
  df4,
  interp(
    x = lon,
    y = depth,
    z = sigmat,
    nx = 200,
    ny = 200
  )
)

interp_df4 <- expand.grid(
  lon = interp_sigmat$x,
  depth = interp_sigmat$y
)

interp_df4$sigmat <- as.vector(interp_sigmat$z)

ggp4 <- ggplot(interp_df4,
               aes(x = lon, y = depth, fill = sigmat)) +
  geom_raster() +
#  geom_contour(aes(z = sigmat),
#               colour = "black",
#               alpha = 0.5) +
  scale_y_reverse() +
  scale_fill_cmocean(
    name = "dense"
  ) +
  labs(
    x = "Longitude (°E)",
    y = "Depth (m)"
  ) +
  theme_bw()


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


interp_lopc <- with(
  df5,
  interp(
    x = lon,
    y = depth,
    z = lopc,
    nx = 200,
    ny = 200
  )
)

interp_df5 <- expand.grid(
  lon = interp_lopc$x,
  depth = interp_lopc$y
)

interp_df5$lopc <- as.vector(interp_lopc$z)

ggp5 <- ggplot(interp_df5,
               aes(x = lon, y = depth, fill = lopc)) +
  geom_raster() +
#  geom_contour(aes(z = lopc),
#              colour = "black",
#               alpha = 0.5) +
  scale_y_reverse() +
  scale_fill_cmocean(
    name = "matter"
  ) +
  labs(
    x = "Longitude (°E)",
    y = "Depth (m)"
  ) +
  theme_bw()


# sal/sigmat

ggp6 <- ggplot(interp_df2,
               aes(x = lon, y = depth, fill = sal)) +
  geom_raster() +
    geom_contour(aes(z = interp_df4$sigmat),
                 colour = "black",
                 formatter = label_number(accuracy = 0.1),
                 alpha = 0.5) +
  geom_text_contour(aes(z = interp_df4$sigmat),
    size = 3
  ) +
    scale_y_reverse() +
  scale_fill_cmocean(
    name = "haline"
  ) +
  labs(
    x = "Longitude (°E)",
    y = "Depth (m)"
  ) +
  theme_bw()


# plot results
ggp1/ggp2/ggp3/ggp5 + plot_layout(axes = "collect")


