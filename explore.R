box::use(dplyr[...],
         readr[read_csv],
         caret[preProcess, createDataPartition, train, trainControl, confusionMatrix],
         lubridate[ms, is.period],
         functions/scrape[get_races])


f1_raw <- read_csv("Data/f1.csv") 

f1 <- f1_raw %>% 
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
         is_ham = ifelse(Driver == "Lewis Hamilton HAM", 1, 0)) %>% 
  select(-Driver, -points, -race, -date, -winner, -contains("laps"))


f1_raw %>% 
  filter(year == 2020) %>% 
  group_by(Driver) %>% 
  summarise(points = sum(points)) %>% 
  arrange(desc(points))


f1$f_result %>% unique()
table(f1$f_result, useNA = "ifany")

pp_f1 <- preProcess(f1, method = c("center", "scale", "YeoJohnson", "nzv"))

transformed <- predict(pp_f1, newdata = f1)  

# benchmark to compare models to. Predicting based only off of quali order
confusionMatrix(factor(ifelse(f1$quali_result > 20, paste0("P", 20), paste0("P", f1$quali_result))), 
                f1$f_result)

set.seed(1234)
trainIndex <- createDataPartition(transformed$f_result, p = .7, list = FALSE)

train <- transformed[ trainIndex,]
test <- transformed[-trainIndex,]

cvCtrl <- trainControl(method = "repeatedcv", repeats = 10,
                       classProbs = TRUE)


model1 <- train(f_result ~ ., data = train,
                method = "knn",
                trControl = cvCtrl)

pred1 <- predict(model1, newdata = test[, -1])

tibble(act = test$f_result, pred = pred1) %>%
  filter(act == "P1") %>%
  count(pred) %>%
  mutate(pct = n/sum(n)*100)

confusionMatrix(pred1, test$f_result)


model2 <- train(f_result ~ ., data = train,
                method = "treebag")

pred2 <- predict(model2, newdata = test[, -1])

confusionMatrix(pred2, test$f_result)


model3 <- train(f_result ~ ., data = train,
                method = "naive_bayes",
                trControl = cvCtrl)

pred3 <- predict(model3, newdata = test[, -1])
confusionMatrix(pred3, test$f_result)


stats <- function (data, lev = NULL, model = NULL)  {
  c(postResample(data[, "pred"], data[, "obs"]),
    Sens = sensitivity(data[, "pred"], data[, "obs"]),
    Spec = specificity(data[, "pred"], data[, "obs"]))
}

f12 <- f1 %>% 
  mutate(win = factor(ifelse(f_result == "P1", 1, 0), levels = c(1, 0))) %>% 
  select(-f_result, -is_ham)

# benchmark based on quali
confusionMatrix(factor(ifelse(f12$quali_result == 1, 1, 0), levels = c(1, 0)), 
                f12$win)

pp_f12 <- preProcess(f12, method = c("center", "scale", "YeoJohnson", "nzv"))

transformed2 <- predict(pp_f12, newdata = f12)  

set.seed(1234)
trainIndex2 <- createDataPartition(transformed2$win, p = .7, list = FALSE)

train2 <- transformed2[ trainIndex2,]
test2 <- transformed2[-trainIndex2,]


ctrl <- trainControl(method = "none", number = 10)


# model4 <- train(win ~ ., data = train2,
#                 method = "svmRadialWeights",
#                 trControl = ctrl,
#                 tuneGrid = expand.grid(sigma = 7,
#                                        C = 1,
#                                        Weight = 2))
model4 <- train(win ~ ., data = train2,
                method = "C5.0Cost",
                trControl = ctrl,
                tuneGrid = expand.grid(trials = 10,
                                       model = "tree",
                                       winnow = FALSE,
                                       cost = 2))
                

pred4 <- predict(model4, newdata = test2[, -8])
confusionMatrix(pred4, test2$win)


model5 <- train(win ~ ., data = train2,
                method = "cforest",
                trControl = ctrl)

pred5 <- predict(model5, newdata = test2[, -8])
confusionMatrix(pred5, test2$win)


model6 <- train(win ~ ., data = train2,
                method = "ordinalRF",
                trControl = ctrl)

pred6 <- predict(model6, newdata = test2[, -8])
confusionMatrix(pred6, test2$win)

f1_raw %>% 
  filter(year == 2017) %>% 
  distinct(race)


f1_all <- f1_raw %>% 
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

dates <- f1_all %>% distinct(date) %>% pull()

aus_2020 <- f1_all %>% 
  filter(date == dates[1]) %>% 
  select(-contains("laps"), -race, -date, -points, -winner) %>% 
  mutate(win = factor(ifelse(f_result == "P1", 1, 0), levels = c(1, 0))) %>% 
  select(-f_result, -is_ham)

aus_trans <- predict(pp_f12, aus_2020)
pred_aus <- predict(model6, aus_trans[, 2:9])

aus_2020$pred <- pred_aus

confusionMatrix(pred_aus, aus_trans$win)

aus_2020 %>% 
  select(Driver, pred, win, quali_result)


