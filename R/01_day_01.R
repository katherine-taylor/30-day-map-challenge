# 01_day_01.R
# Mon Nov  1 21:37:59 2021 ------------------------------


# Libraries ---------------------------------------------------------------

library(tidyverse)
# install.packages("maps")
library(maps)
# install.packages("mapdata")
library(mapdata)
# install.packages("hrbrthemes")
library(hrbrthemes)

# Import Data -------------------------------------------------------------

fires_df <-
  read_csv(here::here("data/WFIGS_-_2021_Wildland_Fire_Locations_to_Date.csv")) |>
  janitor::clean_names()

usa <- map_data("usa")

cleaned_fires <- fires_df |>
  select(initial_latitude, initial_longitude, daily_acres) |>
  filter((initial_latitude >= 25 & initial_latitude <= 50) &
           (initial_longitude >= -180 &
              initial_longitude <= -50)
  ) |>
  mutate(group = rep(1, n()),
         daily_acres = log(daily_acres))

# actual plot
ggplot(usa, aes(x = long, y = lat, group = group)) +
  geom_polygon(fill = "transparent", color = "black") +
  geom_point(
    data = cleaned_fires,
    aes(
      x = initial_longitude,
      y = initial_latitude,
      group = group,
      color = daily_acres
    ) )+
      theme_ipsum() +
      theme(
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
      ) +
      scale_color_viridis_c(
      ) +
    coord_fixed(1.3) +
  labs(title = "2021 US Wildfires by Arces Per Day",
       subtitle = "Colored by log(arces burned per day)",
       caption = "Data from National Interagency Fire Center")

table(fires_df$modified_by_system)
