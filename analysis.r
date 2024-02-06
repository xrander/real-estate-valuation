library(readxl)
library(tidyverse)
library(janitor)

real_estate <- read_excel("Real estate valuation data set.xlsx") |> 
  clean_names()

head(real_estate)

real_estate <- real_estate |> 
  mutate(
    year = x1_transaction_date %/% 1,
    month = round((x1_transaction_date %% 1) * 12), # to get month from taiwanese date
    .before = x2_house_age
  )


real_estate <- real_estate |> 
  mutate(
    month = case_when(
      month == 0 ~ 1,
      TRUE ~ month
    ),
    month = month.abb[month]
  ) |> 
  select(!c(1, 2))

names(real_estate)

real_estate <- real_estate |> 
  rename(
    house_age = x2_house_age,
    distance_to_station = x3_distance_to_the_nearest_mrt_station,
    number_convenience_stores = x4_number_of_convenience_stores,
    latitude = x5_latitude,
    longitude = x6_longitude,
    house_price = y_house_price_of_unit_area
  )

real_estate <- real_estate |> 
  mutate(
    house_size_m2 = (house_price * 10000) / 3.9,
    house_price_usd = (house_price * 10000) * 0.032
  )


ggplot(real_estate, aes(house_price)) +
  geom_histogram(binwidth = 2) +
  geom_density(stat = "bin", binwidth = 2)

ggplot(real_estate, aes(house_price)) +
  geom_boxplot()

outlier <- real_estate |> 
  filter(house_price > 70 | house_price < 10)

real_estate |> 
  arrange(distance_to_station)

ggplot(real_estate, aes(longitude, latitude, col = cut_number(house_price_usd, n = 5))) +
  geom_point() +
  labs(col = "House price range") +
  geom_point(
    data = outlier, 
    aes(longitude, latitude),
    size = 3.5,
    shape = "circle open",
    col = "red"
  ) 
  
ggplot(real_estate, aes(house_price_usd, ))