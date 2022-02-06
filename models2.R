box::use(dplyr[...],
         readr[read_csv],
         caret[preProcess, createDataPartition, train, trainControl, confusionMatrix],
         lubridate[ms, is.period],
         functions/scrape[get_races],
         magrittr[use_series, extract, subtract],
         keras[activation_relu])


f1_raw <- read_csv("Data/f1.csv") 

f1_2021 <- read_csv("Data/f1_2021.csv")

f1_2021_p <- f1_2021 %>% 
  # filter(!f_result > 20) %>% 
  mutate(across(c(Q1:Q3, fp3_time, fp3_gap, fp2_time, fp2_gap, fp1_time, fp1_gap),
                ms),
         winner = factor(if_else(f_result == 1, "yes", "no"), levels = c("yes", "no"))) %>% 
  select(!where(is.period)) %>% 
  group_by(year, Driver) %>% 
  arrange(year) %>% 
  mutate(cum_points = cumsum(points) - points,
         race_num = row_number()) %>% 
  ungroup() %>% 
  mutate(across(ends_with("laps"), ~ifelse(is.na(.x), 0, .x)),
         across(ends_with("result"), ~ifelse(is.na(.x), 20, .x)),
         f_result = factor(paste0("P", f_result))) %>% 
  select(-Driver, -points, -race, -date, -contains("laps")) %>% 
  mutate(win = factor(ifelse(f_result == "P1", 1, 0), levels = c(1, 0))) %>% 
  select(-f_result, -Car, -win)

pred_one <- function(x) {
  pred <- x[1] %>% pull()
  
  f1_all %>% 
    filter(year == max(year)) %>% 
    mutate(pred = pred) %>% 
    group_by(race) %>% 
    filter(pred == max(pred)) %>% 
    ungroup() %>% 
    count(f_result) %>% 
    mutate(pct = n/sum(n)*100)
}


f1_all <- f1_raw %>% 
  filter(!f_result > 20) %>% 
  mutate(across(c(Q1:Q3, fp3_time, fp3_gap, fp2_time, fp2_gap, fp1_time, fp1_gap),
                ms),
         winner = factor(if_else(winner == 1, "yes", "no"), levels = c("yes", "no"))) %>% 
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

f1 <- f1_all %>% 
  select(-Driver, -points, -race, -date, -contains("laps")) %>% 
  mutate(win = factor(ifelse(f_result == "P1", 1, 0), levels = c(1, 0))) %>% 
  select(-f_result, -is_ham, -Car, -win)


pp_f1 <- preProcess(f1, method = c("center", "scale", "YeoJohnson", "nzv"))


transformed <- predict(pp_f1, newdata = f1)  

transformed2021 <- predict(pp_f1, f1_2021_p)

train <- transformed %>% 
  filter(year != max(year)) %>% 
  select(-year)

test <- transformed %>% 
  filter(year == max(year)) %>% 
  select(-year)


stats <- function (data, lev = NULL, model = NULL)  {
  conf <- confusionMatrix(data$pred, data$obs)
  out <- conf$byClass
  
  names(out) <- c("sens", "spec", "posPredVal", "negPredVal", "prec", "recall", 
                  "F1", "prevalence", "detRate", "detPrev", "balanced")
  return(out)
}


ctrl <- trainControl(method = "repeatedcv", repeats = 1, number = 3, 
                     verboseIter = TRUE, summaryFunction = stats)

# model6 <- train(winner ~ ., data = train,
#                 method = "mlpML",
#                 trControl = ctrl, 
#                 metric = "posPredVal",
#                 tuneGrid = expand.grid(layer1 = 1:10,
#                                        layer2 = 1:10,
#                                        layer3 = 1:10))
# 
# model6$results %>% 
#   filter(layer1 == 3, layer2 == 2, layer3 == 10) # these are the optimal values


pred6 <- predict(model6, newdata = test[, -5])
confusionMatrix(pred6, test$winner)

pred6_all <- predict(model6, newdata = transformed[, -8])
confusionMatrix(pred6_all, transformed$winner)

prob6 <- predict(model6, newdata = test[, -5], type = "prob")
pred_one(prob6)

model7 <- train(winner ~ ., data = train,
                method = "mlpML",
                trControl = trainControl(method = "none"), 
                tuneGrid = expand.grid(layer1 = 10,
                                       layer2 = 10,
                                       layer3 = 10)) # changing layers does not appear to change
pred7 <- predict(model7, newdata = test[, -5], type = "prob")

pred_one(pred7)



comp <- f1_all %>% 
  filter(year == max(year)) %>%
  mutate(pred = pull(pred7[1])) %>% 
  group_by(race) %>% 
  mutate(pred_winner = factor(ifelse(pred == max(pred), "yes", "no"), c("yes", "no"))) %>% 
  select(Driver, race, pred_winner, f_result, pred, quali_result, winner)

confusionMatrix(comp$pred_winner, comp$winner)

comp %>% distinct(race)
comp %>% 
  filter(pred > 0.4)


pred2021 <- predict(model7, newdata = transformed2021[, -5], type = "prob")
f1_2021 %>% distinct(race)
f1_21_df <- f1_2021 %>% 
  select(Driver, f_result, Car, race, quali_result) %>% 
  mutate(pred_prob = pred2021$yes) %>% 
  group_by(race) %>% 
  mutate(pred_win = ifelse(pred_prob == max(pred_prob), 1, 0)) #%>% 
  # filter(race == "Styria") %>%
  # arrange(pred_prob)
f1_21_df %>% 
  filter(f_result == 1) %>% 
  ungroup() %>% 
  count(pred_win) %>% 
  mutate(pct = n/sum(n)*100)

m_cost <- train(winner ~ ., data = train,
                method = "C5.0",
                trControl = ctrl,
                tuneGrid = expand.grid(model = c("tree", "rules"), winnow = FALSE,
                                       trials = 1:20),
                metric = "mix")
predCost <- predict(m_cost, newdata = test[, -5])
confusionMatrix(predCost, test$winner)

predCostProb <- predict(m_cost, newdata = test[, -5], type = "prob")
pred_one(predCostProb)

train_up <- upSample(x = train[, -5], y = train$winner)
train_down <- downSample(x = train[, -5], y = train$winner)
table(train_up$Class)
table(train$winner)
table(train_down$Class)

mlp_sam <- train(Class ~ ., data = train_down,
             method = "mlpML",
             trControl = trainControl(method = "none"),
             metric = "posPredVal", 
             tuneGrid = expand.grid(layer1 = 10,
                                    layer2 = 10,
                                    layer3 = 10))
pred_a <- predict(mlp_sam, test[, -5], type = "prob")

pred_one(pred_a)










