library(tidyverse)
library(lubridate)
library(patchwork)

# data
oslo <- read_csv("data/oslo.csv") |> mutate(name = "Oslo", day = parse_date_time(datetime, "mdy"))
perth <- read_csv("data/perth.csv") |> mutate(name = "Perth", day = parse_date_time(datetime, "mdy"))

data_long <- bind_rows(oslo, perth)
data_long

# difference
temp_oslo <- oslo |> select(day, temp_oslo = temp)
temp_perth <- perth |> select(temp_perth = temp)

temp_diff <- bind_cols(temp_oslo, temp_perth) |> mutate(diff = temp_perth - temp_oslo)

# days with diff. within +- 1 degrees C\
equal <- temp_diff |> filter(abs(diff) <= 1)

# plots
# plot average temp
plot_1 <-
  ggplot() +
  geom_path(data = data_long, aes(day, temp, color = name), alpha = 1 / 2, size = 1) +
  scale_x_datetime(
    date_breaks = "1 month",
    date_minor_breaks = "1 month",
    labels = scales::label_date_short()
  ) +
  coord_cartesian(expand = FALSE) +
  theme_light() +
  labs(
    x = NULL,
    y = "Temperature in °C",
    color = NULL
  ) +
  geom_hline(yintercept = 0)

# plot difference
plot_2 <-
  ggplot() +
  geom_segment(data = temp_diff, aes(x = day, xend = day, y = 0, yend = diff), size = 1.0, color = "gray80") +
  scale_x_datetime(
    date_breaks = "1 month",
    date_minor_breaks = "1 month",
    labels = scales::label_date_short()
  ) +
  coord_cartesian(expand = FALSE) +
  theme_light() +
  labs(
    x = NULL,
    y = "Perth - Oslo in °C",
    color = NULL
  ) +
  geom_hline(yintercept = 0) +
  geom_point(data = equal, aes(day, 0), size = 1, color = "red")

# combine plots
plot <- plot_1 / plot_2
plot

# save
ggsave("plots/temp.png", width = 15, height = 7.5)
