# Core packages: 
library(tidyverse)
library(tidyquant)
library(readr)

# Modeling packages
library(parsnip)
library(recipes)
library(rsample)
library(yardstick)
library(broom)

# Connector packages
library(rpart)
library(rpart.plot)
library(xgboost)

# Import data
control_tbl <- read_csv("~/Desktop//MITA/AB Test/control_data.csv")
experiment_tbl <- read_csv("~/Desktop//MITA/AB Test/experiment_data.csv")

control_tbl %>% head(5)
control_tbl %>% glimpse()
experiment_tbl %>% glimpse()

# Check missing data

control_tbl %>%
  map_df(~ sum(is.na(.))) %>%
  gather(key = "feature", value = "missing_count") %>%
  arrange(desc(missing_count))

experiment_tbl %>% 
  map_df(~ sum(is.na(.))) %>%
  gather(key = "feature", value = "missing_count") %>%
  arrange(desc(missing_count))


control_tbl %>%
  filter(is.na(Enrollments))


set.seed(123)
data_formatted_tbl <- control_tbl %>%
  
  # Combine with Experiment data
  bind_rows(experiment_tbl, .id = "Experiment") %>%
  mutate(Experiment = as.numeric(Experiment) - 1) %>%
  
  # Add row id
  mutate(row_id = row_number()) %>%
  
  # Create a Day of Week feature
  mutate(DOW = str_sub(Date, start = 1, end = 3) %>% 
           factor(levels = c("Sun", "Mon", "Tue", "Wed", 
                             "Thu", "Fri", "Sat"))
  ) %>%
  select(-Date, -Payments) %>%
  
  # Remove missing data
  filter(!is.na(Enrollments)) %>%
  
  # Shuffle the data (note that set.seed is used to make reproducible)
  sample_frac(size = 1) %>%
  
  # Reorganize columns
  select(row_id, Enrollments, Experiment, everything())

data_formatted_tbl %>% glimpse()

set.seed(123)
split_obj <- data_formatted_tbl %>%
  initial_split(prop = 0.8, strata = "Experiment")

train_tbl <- training(split_obj)
test_tbl  <- testing(split_obj)

train_tbl %>% glimpse()
test_tbl %>% glimpse()

## Linear Regression

model_01_lm <- linear_reg("regression") %>%
  set_engine("lm") %>%
  fit(Enrollments ~ ., data = train_tbl %>% select(-row_id))

# knitr::kable() used for pretty tables
model_01_lm %>%
  predict(new_data = test_tbl) %>%
  bind_cols(test_tbl %>% select(Enrollments)) %>%
  metrics(truth = Enrollments, estimate = .pred) %>%
  knitr::kable()

# |.metric |.estimator |  .estimate|
# |:-------|:----------|----------:|
# |rmse    |standard   | 26.0634680|
# |rsq     |standard   |  0.0636897|
# |mae     |standard   | 19.4039158|

# The model is on average off by +/-19 enrollments (means absolute error). 
# The test set R-squared is low at 0.06.

model_01_lm %>%
  # Format Data
  predict(test_tbl) %>%
  bind_cols(test_tbl %>% select(Enrollments)) %>%
  mutate(observation = row_number() %>% as.character()) %>%
  gather(key = "key", value = "value", -observation, factor_key = TRUE) %>%
  
  # Visualize
  ggplot(aes(x = observation, y = value, color = key)) +
  geom_point() +
  expand_limits(y = 0) +
  theme_tq() +
  scale_color_tq() +
  labs(title = "Enrollments: Prediction vs Actual",
       subtitle = "Model 01: Linear Regression (Baseline)")

linear_regression_model_terms_tbl <- model_01_lm$fit %>%
  tidy() %>%
  arrange(p.value) %>%
  mutate(term = as_factor(term) %>% fct_rev()) 

# knitr::kable() used for pretty tables
linear_regression_model_terms_tbl %>% knitr::kable()

# |term        |    estimate|  std.error|  statistic|   p.value|
# |:-----------|-----------:|----------:|----------:|---------:|
# |Clicks      |  -0.5788150|  0.1323455| -4.3735162| 0.0001533|
# |Pageviews   |   0.0503213|  0.0148314|  3.3928928| 0.0020803|
# |Experiment  | -17.5541754|  7.6074013| -2.3075127| 0.0286325|
# |(Intercept) | 134.7873196| 79.5652606|  1.6940474| 0.1013534|
# |DOWMon      |  25.4287578| 17.4786746|  1.4548447| 0.1568322|
# |DOWThu      | -17.4230930| 15.8000716| -1.1027224| 0.2795358|
# |DOWWed      |  12.4235287| 14.5532451|  0.8536604| 0.4005376|
# |DOWSat      | -11.0150158| 15.5347097| -0.7090584| 0.4841509|
# |DOWFri      |   8.5195293| 14.2637448|  0.5972856| 0.5551166|
# |DOWTue      |  -0.7023031| 16.7661840| -0.0418881| 0.9668852|


linear_regression_model_terms_tbl %>%
  ggplot(aes(x = p.value, y = term)) +
  geom_point(color = "#2C3E50") +
  geom_vline(xintercept = 0.05, linetype = 2, color = "red") +
  theme_tq() +
  labs(title = "Feature Importance",
       subtitle = "Model 01: Linear Regression")

# We identified feature importance. Clicks, Pageviews, and Experiment are the most important features. 
# Experiment is 3rd, with a p.value 0.026. Typically this is considered significant.
# We can also see the term coefficient for Experiment is -17.6 indicating as decreasing 
# Enrollments by 17.6 per day when the Experiment is run.

## Decision Tree

# we will implement decision tree model using the "rpart" library and to prevent overfitting we  will
# use 5-fold cross validation technique

model_02_decision_tree <- decision_tree(
  mode = "regression",
  cost_complexity = 0.001, 
  tree_depth = 5, 
  min_n = 4) %>%
  set_engine("rpart") %>%
  fit(Enrollments ~ ., data = train_tbl %>% select(-row_id))

# cost_complexity: A cutoff for model splitting based on increase in explainability
# tree_depth: The max tree depth
# min_n: The minimum number of observations in terminal (leaf) nodes

model_02_decision_tree %>%
  predict(new_data = test_tbl) %>%
  bind_cols(test_tbl %>% select(Enrollments)) %>%
  metrics(truth = Enrollments, estimate = .pred) %>%
  knitr::kable()

# |.metric |.estimator |  .estimate|
# |:-------|:----------|----------:|
# |rmse    |standard   |   23.85086|
# |rsq     |standard   |    0.27693|
# |mae     |standard   |   19.06771|

# the mae of the predictions is approximately the same as linear model i.e. +/- 19 enrollments per day

model_02_decision_tree %>%
  # Format Data
  predict(test_tbl) %>%
  bind_cols(test_tbl %>% select(Enrollments)) %>%
  mutate(observation = row_number() %>% as.character()) %>%
  gather(key = "key", value = "value", -observation, factor_key = TRUE) %>%
  
  # Visualize
  ggplot(aes(x = observation, y = value, color = key)) +
  geom_point() +
  expand_limits(y = 0) +
  theme_tq() +
  scale_color_tq() +
  labs(title = "Enrollments: Prediction vs Actual",
       subtitle = "Model 02: Decision Tree")

model_02_decision_tree$fit %>%
  rpart.plot(
    roundint = FALSE, 
    cex = 0.8, 
    fallen.leaves = TRUE,
    extra = 101, 
    main = "Model 02: Decision Tree")

# From the graph, we can say that when the experiement >=0.5 and the pageviews are less than 7720, 
# the enrollments decreases. 

## XGBoost

# we will implement decision tree model using the "XGBoost" library and to prevent overfitting we  will
# use 5-fold cross validation technique


set.seed(123)
model_03_xgboost <- boost_tree(
  mode = "regression",
  mtry = 100, 
  trees = 1000, 
  min_n = 8, 
  tree_depth = 6, 
  learn_rate = 0.2, 
  loss_reduction = 0.01, 
  sample_size = 1) %>%
  set_engine("xgboost") %>%
  fit(Enrollments ~ ., data = train_tbl %>% select(-row_id))

# mtry: The number of predictors that will be randomly sampled at each split when creating the tree models.
# trees: The number of trees contained in the ensemble.
# min_n: The minimum number of data points in a node that are required for the node to be split further.
# tree_depth: The maximum depth of the tree (i.e. number of splits).
# learn_rate: The rate at which the boosting algorithm adapts from iteration-to-iteration.
# loss_reduction: The reduction in the loss function required to split further.
# sample_size: The amount of data exposed to the fitting routine

model_03_xgboost %>%
  #Format Data
  predict(new_data = test_tbl) %>%
  bind_cols(test_tbl %>% select(Enrollments)) %>%
  metrics(truth = Enrollments, estimate = .pred) %>%
  knitr::kable()

# |.metric |.estimator |  .estimate|
# |:-------|:----------|----------:|
# |rmse    |standard   | 13.8760506|
# |rsq     |standard   |  0.7256724|
# |mae     |standard   | 11.5450172|

# Visualize
ggplot(aes(x = observation, y = value, color = key)) +
  geom_point() +
  expand_limits(y = 0) +
  theme_tq() +
  scale_color_tq() +
  labs(title = "Enrollments: Prediction vs Actual",
       subtitle = "Model 03: XGBoost")

xgboost_feature_importance_tbl <- model_03_xgboost$fit %>%
  xgb.importance(model = .) %>%
  as_tibble() %>%
  mutate(Feature = as_factor(Feature) %>% fct_rev())

xgboost_feature_importance_tbl %>% knitr::kable()

# |Feature    |      Gain|     Cover| Frequency|
# |:----------|---------:|---------:|---------:|
# |Pageviews  | 0.6331988| 0.6879569| 0.6465324|
# |Clicks     | 0.2964312| 0.2061510| 0.2076063|
# |Experiment | 0.0703701| 0.1058921| 0.1458613|

xgboost_feature_importance_tbl %>%
  ggplot(aes(x = Gain, y = Feature)) +
  geom_point(color = "#2C3E50") +
  geom_label(aes(label = scales::percent(Gain)), 
             hjust = "inward", color = "#2C3E50") +
  expand_limits(x = 0) +
  theme_tq() +
  labs(title = "XGBoost Feature Importance")
# The information gain is 93% from Pageviews and Clicks combined. 
# Experiment has about a 7% contribution to information gain, 
# indicating it’s still predictive (just not nearly as much as Pageviews). 
# This tells a story that if Enrollments are critical, Udacity should focus on getting Pageviews.
# The XGBoost model error has dropped to +/-11 Enrollments.
# It shows that Experiment provides an information gain of 7%
# Udacity should be focusing on Page Views and secondarily Clicks to maintain or increase Enrollments.



