
# Import Libraries --------------------------------------------------------


library(readxl)
library(tidyverse)
library(tidymodels)
library(vip)
library(janitor)
library(GGally)
library(ggthemes)
library(scales)


# Import Data -------------------------------------------------------------


real_estate <- read_excel("Real estate valuation data set.xlsx") |> 
  clean_names()

head(real_estate)


# Data Preparation --------------------------------------------------------



real_estate <- real_estate |> 
  mutate(
    year = x1_transaction_date %/% 1,
    month = round((x1_transaction_date %% 1) * 12), # to get month from taiwanese date
    .before = x2_house_age
  )


real_estate <- real_estate |> 
  mutate(month = case_when(month == 0 ~ 1, TRUE ~ month)) |> 
  select(!c(1, 2))

real_estate <- real_estate |> 
  rename(
    age = x2_house_age,
    distance_to_station = x3_distance_to_the_nearest_mrt_station,
    number_convenience_stores = x4_number_of_convenience_stores,
    latitude = x5_latitude,
    longitude = x6_longitude,
    price = y_house_price_of_unit_area
  )

real_estate <- real_estate |> 
  mutate(
    age = ceiling(age),
    sale_date = make_date(year = as.integer(year), month = month),
    .before = age
  ) |> 
  select(-c(year, month))

names(real_estate)

real_estate <- real_estate |> 
  mutate(
    size_m2 = (price * 10000) / 3.9,
    price_usd = (price * 10000) * 0.032,
    .before = price
  )

# Missing Values? ---------------------------------------------------------

sum(is.na(real_estate))
sum(duplicated(real_estate))

# EDA ---------------------------------------------------------------------
## Target Variable ---------------------------

price_median <- 
  tibble(
    med = median(real_estate$price_usd),
    label = paste0("$", med)
  )

ggplot(real_estate, aes(price_usd)) +
  geom_histogram(binwidth = 500, alpha =0.7, fill = "wheat3") +
  geom_density(stat = "bin", binwidth = 500, col = "brown") +
  geom_vline(aes(xintercept = median(price_usd)), col = "violetred3") +
  geom_text(
    data = price_median,
    aes(x = med, y = 30, label = label),
    hjust = -0.3,
    col = "red"
  ) +
  labs(
    x = "Price",
    y = "count",
    title = "Long-tailed Price distribution"
  ) +
  theme_igray() +
  scale_x_continuous(label = label_dollar())

outlier <- 
  tibble(
    x = 1,
    max_price = max(real_estate$price_usd),
  )


ggplot(real_estate, aes(price_usd, x = 1)) +
  ggbeeswarm::geom_quasirandom(
    col = "darkgreen",
    shape = "circle"
  ) + 
  geom_point(
    data = outlier, 
    aes(x, max_price),
    shape = "circle filled", stroke = 1.2, size = 3,
    fill = "red",  col = "orange",
  ) +
  geom_text(
    data = outlier,
    aes(y = max_price, label = "Outlier"),
    vjust = 1.7
  ) +
  scale_y_continuous(
    label = label_dollar(),
    breaks = seq(0, 40000, 5000)
  ) +
  labs(
    x = "",
    y = "Price",
    title = "Red dot shows house out that is overprized"
  ) +
  coord_flip() +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.y = element_blank()
  ) +
  theme_pander()

real_estate <- real_estate |> filter(!price_usd > 30000)

range(real_estate$price_usd)

## Multivariate -----------------------------------

ggplot(real_estate, aes(factor(sale_date), price_usd)) +
  geom_violin(fill = "olivedrab3") +
  geom_jitter(aes(y = price_usd), size = 0.5, alpha = 0.5, col = "red") +
  theme(axis.text.x = element_text(angle = 20)) +
  labs(x = "Sale Date", y = "Price", 
       title = "January and November shows large volume of sales",
       subtitle = "Mid year (May/June) shows increase in house purchase, as sales in other months declines"
  ) +
  scale_y_continuous(label = label_dollar()) +
  theme_pander()

ggplot(real_estate, aes(fct_reorder(cut_number(age, 10), price_usd, .fun = sum), price_usd)) +
  geom_col(fill = "springgreen3") +
  labs(
    x = "Age",
    y = "Price",
    title = str_wrap("New houses age 0 to 4 years fetch made more sales in dollar
                     in general than old houses", width = 60)
  ) +
  scale_y_continuous(label = label_dollar()) +
  coord_flip() +
  theme_igray()


correlation <- cor(real_estate$price_usd, real_estate$age)

ggplot(real_estate, aes(price_usd, age)) +
  geom_smooth(method = "lm", se = F, col = "tomato2") +
  expand_limits(y = c(0, 45)) +
  labs(
    x = "Price",
    y = "Age",
    title = "House price reduces as age increases"
  )+
  annotate(
    geom = "label",
    label = paste("correlation:", round(correlation, 2), sep = " "),
    x = 15000, y = 25, col = "red"
  ) +
  theme_clean()

ggplot(real_estate, aes(price_usd, distance_to_station)) +
  geom_point() +
  scale_y_log10(label = label_number()) +
  labs(
    x = "Price",
    y = "Distance to Station (m)",
    title = "Negative relationship between Price and Distance to Station",
    subtitle = "Houses closer to the station are costlier"
  ) +
  theme_pander()

ggplot(real_estate, aes(longitude, latitude, col = price_usd)) +
  geom_jitter() +
  labs(
    col = "Price (USD)",
    x = "Longitude",
    y = "Latitude",
    title = "The prices of houses increases as we move North East",
    subtitle = str_wrap("Prices of houses increases where there are clusters\ of house, this
                        may be due to the proximity to the MRT station", width = 55)
  ) +
  scale_colour_gradient(low = "gray", high = "red") +
  theme_pander() +
  theme(legend.position = "top") +
  guides(
    color = guide_colorbar(
      barwidth = 15, 
      barheight = 1/2, 
      ticks.colour = "black", 
      title.position = "left", 
      title.theme = element_text(size = 8))
    )


ggcorr(real_estate |> select(!c(sale_date, price)))


# Model Development ------------------------------------------

real_estate <- real_estate |> 
  mutate(
    month = month(sale_date),
    .before = age
  ) |> 
  select(-c(sale_date, price))

head(real_estate)


## Data Sharing ------------------------------------------------------------

set.seed(333)


real_estate_split <- initial_split(real_estate, prop = .8, strata = price_usd)

real_estate_train <- training(real_estate_split)
real_estate_test <- testing(real_estate_split)

real_estate_split

## Model Specification -----------------------------------------------------

xg_model <- 
  boost_tree(
    mtry = tune(), min_n = tune(),
    tree_depth = tune(), trees = 1000,
    loss_reduction = tune(),
    sample_size = tune(),
    learn_rate = tune(),
  ) |> 
  set_engine("xgboost") |> 
  set_mode("regression")

xg_model |>  translate()


# Tune Model --------------------------------------------------------------


set.seed(3434)


xgb_grid <- grid_latin_hypercube(
  tree_depth(),
  min_n(),
  loss_reduction(),
  sample_size = sample_prop(),
  finalize(mtry(), real_estate_train),
  learn_rate(),
  size = 30
)

xgb_grid

xg_wf <- workflow() |> 
  add_formula(price_usd ~ .) |> 
  add_model(xg_model)

xg_wf

set.seed(222)

real_estate_folds <- vfold_cv(real_estate_train, strata = price_usd)

doParallel::registerDoParallel()

set.seed(222)

xg_tune_res <- tune_grid(
  xg_wf,
  resamples = real_estate_folds,
  grid = xgb_grid,
  control = control_grid(save_pred = T)
)

xg_tune_res |> 
  collect_metrics() |> 
  filter(.metric == "rmse") |> 
  select(mean, mtry:sample_size) |> 
  pivot_longer(
    mtry:sample_size,
    values_to = "value",
    names_to = "parameter"
  ) |> 
  ggplot(aes(value, mean, color = parameter)) +
  geom_jitter(show.legend = F, width = .4) +
  facet_wrap(~parameter, scales = "free_y")

show_best(xg_tune_res, metric = "rmse")

best_rmse <- select_best(xg_tune_res, "rmse")
best_rmse

final_boost_tree <- finalize_workflow(
  xg_wf,
  best_rmse
)


final_boost_tree |> 
  fit(data = real_estate_train) |> 
  pull_workflow_fit() |> 
  vip(
    geom = "col",
    aesthetics = list(fill = "springgreen3")
  ) +
  theme_pander()

final_result <- last_fit(final_boost_tree, real_estate_split)

collect_metrics(final_result)

final_result |> 
  collect_predictions() |> 
  select("actual" = price_usd, "prediction" = .pred) |> 
  ggplot(aes(actual, prediction)) +
  geom_point(col = "orange2") +
  geom_label(
    aes(x = 10500, y = 15000, label = "R-square: 0.9974"),
    col = "blue"
  ) +
  geom_abline(col = "red") +
  theme_few()

real_estate_boost_tree_model <- final_result |> 
  extract_fit_parsnip()