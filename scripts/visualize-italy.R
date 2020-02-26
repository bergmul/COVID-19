library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(magrittr)

in_path = "csse_covid_19_data/csse_covid_19_time_series"

confirmed = read.csv(file.path(in_path, "time_series_19-covid-Confirmed.csv"))
deaths    = read.csv(file.path(in_path, "time_series_19-covid-Deaths.csv"))
recovered = read.csv(file.path(in_path, "time_series_19-covid-Recovered.csv"))

# tidy data, join, fix date
confirmed %<>% 
  pivot_longer(
    cols         = starts_with("X"),
    names_to     = "date",
    names_prefix = "X",
    values_to    = "confirmed"
  )

deaths %<>% 
  pivot_longer(
    cols         = starts_with("X"),
    names_to     = "date",
    names_prefix = "X",
    values_to    = "deaths"
  ) 

recovered %<>% 
  pivot_longer(
    cols         = starts_with("X"),
    names_to     = "date",
    names_prefix = "X",
    values_to    = "recovered"
  )

df = 
  confirmed %>% 
  left_join(deaths,    by = c("Province.State",  "Country.Region", "date", "Lat", "Long")) %>%
  left_join(recovered, by = c("Province.State",  "Country.Region", "date", "Lat", "Long")) %>%
  mutate(date = mdy(date))

# plot Italy
df %>% 
  filter(Country.Region == "Italy") %>%
  pivot_longer(cols = c("confirmed", "deaths", "recovered"),
               names_to = "variable",
               values_to = "value") %>%
  ggplot(
    aes(x = date,
        y = value,
        color = variable
    )
  ) +
    geom_line() +
    geom_point() +
    scale_y_log10() +
  theme_bw()
  



