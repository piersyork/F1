library(tidymodels)
box::use(readr[read_csv],
         lubridate[ms, is.period])

f1_raw <- read_csv("Data/f1.csv") 

initial_process <- function(data) {
  data %>% 
    filter(!f_result > 20) %>% 
    mutate(across(c(Q1:Q3, fp3_time, fp3_gap, fp2_time, fp2_gap, fp1_time, fp1_gap),
                  ms),
           winner = factor(if_else(winner == 1, "yes", "no"))) %>% 
    select(!where(is.period)) %>% 
    group_by(year, Driver) %>% 
    arrange(year) %>% 
    mutate(cum_points = cumsum(points) - points,
           race_num = row_number()) %>% 
    ungroup() %>% 
    mutate(across(ends_with("laps"), ~ifelse(is.na(.x), 0, .x)),
           across(ends_with("result"), ~ifelse(is.na(.x), 20, .x)),
           f_result = factor(paste0("P", f_result)),
           is_ham = ifelse(Driver == "Lewis Hamilton HAM", 1, 0)) 
    # select(-Driver, -points, -race, -date, -winner, -contains("laps"))
}

f1 <- f1_raw %>% 
  initial_process()

f1_2021 <- read_csv("Data/f1_2021.csv") %>% 
  mutate(winner = if_else(f_result == 1, 1, 0)) %>% 
  initial_process()

f1 %>% 
  filter(date == max(date)) %>% 
  select(Driver, winner)

f1_split <- initial_split(f1, 0.75, winner)
f1_train <- training(f1_split)
f1_test <- testing(f1_split)

logistic_model <- logistic_reg()

logistic_fit <- logistic_model %>% 
  fit(winner ~ cum_points + quali_result + race_num + fp3_result + fp2_result + fp1_result, f1_train)

log_pred <- logistic_fit %>% 
  predict(new_data = f1_test, type = "class")

log_results <- f1_test %>% 
  # select(winner, cum_points) %>% 
  bind_cols(log_pred)

log_results %>% 
  conf_mat(winner, .pred_class) %>% 
  autoplot(type = "heatmap")


## svm
svm_model <- svm_poly(degree = 3) %>% 
  set_mode("classification")

svm_fit <- svm_model %>% 
  last_fit(winner ~ cum_points + quali_result + race_num + fp3_result + fp2_result + fp1_result, f1_split)

svm_pred <- svm_fit %>% 
  collect_predictions()

svm_pred %>% 
  conf_mat(winner, .pred_class)

svm_pred %>% 
  roc_curve(winner, .pred_yes) %>% 
  autoplot()

svm_fit %>% 
  collect_metrics()



### predicting 2021
pre_2021_fit <- decision_tree(mode = "classification") %>% 
  fit(winner ~ cum_points + quali_result + fp3_result + fp2_result + fp1_result + fp3_laps , f1) # 

post_21_pred <- predict(pre_2021_fit, new_data = f1_2021, type = "prob")

race_pred_21 <- f1_2021 %>% 
  select(Driver, race, winner) %>% 
  bind_cols(post_21_pred) %>% 
  group_by(race) %>% 
  mutate(pred_winner = factor(if_else(.pred_yes == max(.pred_yes), "yes", "no"))) %>% 
  ungroup()

race_pred_21 %>%
  conf_mat(winner, pred_winner) 

f1_2021 %>% 
  select(Driver, race, winner, quali_result) %>% 
  bind_cols(post_21_pred) %>% 
  group_by(race) %>% 
  mutate(pred_winner = factor(if_else(.pred_yes == max(.pred_yes), "yes", "no")),
         quali_pred = factor(if_else(quali_result == 1, "yes", "no"))) %>% 
  ungroup() %>% 
  conf_mat(winner, quali_pred)

f1_2021$quali_result


## plots
f1_2021 %>% 
  # filter(Driver %in% c("Lewis Hamilton HAM", "Max Verstappen VER")) %>%
  filter(Driver != "Robert Kubica KUB") %>% 
  mutate(f_result = as.numeric(stringr::str_remove(f_result, "P"))) %>% 
  ggplot(aes(as.numeric(f_result, colour = Driver))) +
  geom_histogram(bins = 20) +
  facet_wrap(~Driver) +
  scale_y_continuous(n.breaks = 6)
f1_2021$Driver %>% unique()

f1_2021 %>% 
  filter(Driver != "Robert Kubica KUB") %>% 
  group_by(Driver) %>% 
  mutate(f_result = as.numeric(stringr::str_remove(f_result, "P"))) %>%
  summarise(result_sd = sd(f_result)) %>% 
  select(Driver, result_sd) %>% 
  arrange(result_sd)


### recipes
f1_rec <- recipe(winner ~ cum_points + quali_result + fp3_result + fp2_result + fp1_result + fp3_laps , f1_train) %>% 
  step_normalize(all_numeric_predictors())

f1_wkflw <- workflow() %>% 
  add_model(logistic_reg()) %>%
  # add_model(svm_poly()) %>% 
  add_recipe(f1_rec)


f1_fit <- fit(f1_wkflw, f1_train)


pred <- f1_fit %>% 
  predict(new_data = f1_test, type = "class")

results <- f1_test %>% 
  # select(winner, cum_points) %>% 
  bind_cols(pred)

results %>% 
  conf_mat(winner, .pred_class) %>% 
  autoplot(type = "heatmap")


















