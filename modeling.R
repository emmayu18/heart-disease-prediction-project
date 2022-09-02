library(tidyverse)
library(tidymodels)
tidymodels_prefer()
set.seed(123)

## load data
heart_dat <- readRDS("data/processed/heart.rds")

## split data and v-fold
heart_split <- initial_split(heart_dat, prop = 0.8, strata = heart_disease)
heart_train <- training(heart_split)
heart_test <- testing(heart_split)
heart_fold <- vfold_cv(heart_train, v = 10, repeats = 5, 
                       strata = heart_disease)

## recipe
heart_rec <- recipe(heart_disease ~ ., data = heart_train) %>%
  step_dummy(all_nominal_predictors())

heart_tree_rec <- recipe(heart_disease ~ ., data = heart_train) %>%
  step_dummy(all_nominal_predictors(), one_hot = TRUE)

## define models
glm_model <- logistic_reg(mode = "classification") %>%
  set_engine("glm")

rf_model <- rand_forest(mode = "classification",
                        mtry = tune(),
                        min_n = tune()) %>%
  set_engine("ranger")

bt_model <- boost_tree(mode = "classification",
                       mtry = tune(),
                       min_n = tune(),
                       learn_rate = tune()) %>%
  set_engine("xgboost")

kn_model <- nearest_neighbor(mode = "classification",
                             neighbors = tune()) %>%
  set_engine("kknn")

## parameters for tuning
rf_params <- parameters(rf_model) %>% 
  update(mtry = mtry(c(1, 11)))
rf_grid <- grid_regular(rf_params, levels = 5)

bt_params <- parameters(bt_model) %>%
  update(mtry = mtry(c(1, 11)),
         learn_rate = learn_rate(c(-5, -0.2)))
bt_grid <- grid_regular(bt_params, levels = 5)

kn_params <- parameters(kn_model)
kn_grid <- grid_regular(kn_params, levels = 5)

## define workflows
glm_wflow <- workflow() %>%
  add_model(glm_model) %>%
  add_recipe(heart_rec)

rf_wflow <- workflow() %>%
  add_model(rf_model) %>%
  add_recipe(heart_tree_rec)

bt_wflow <- workflow() %>%
  add_model(bt_model) %>%
  add_recipe(heart_tree_rec)

kn_wflow <- workflow() %>%
  add_model(kn_model) %>%
  add_recipe(heart_tree_rec)

## tuning
rf_tuned <- rf_wflow %>%
  tune_grid(heart_fold, grid = rf_grid, metrics = metric_set(roc_auc))
save(rf_tuned, file = "tuned/rf_tuned.rda")

bt_tuned <- bt_wflow %>%
  tune_grid(heart_fold, grid = bt_grid, metrics = metric_set(roc_auc))
save(bt_tuned, file = "tuned/bt_tuned.rda")

kn_tuned <- kn_wflow %>%
  tune_grid(heart_fold, grid = kn_grid, metrics = metric_set(roc_auc))
save(kn_tuned, file = "tuned/kn_tuned.rda")

load("tuned/rf_tuned.rda")
load("tuned/bt_tuned.rda")
load("tuned/kn_tuned.rda")

select_best(rf_tuned, metric = "roc_auc")
select_best(bt_tuned, metric = "roc_auc")
select_best(kn_tuned, metric = "roc_auc")

## metrics
keep_pred <- control_resamples(save_pred = TRUE, save_workflow = TRUE)
glm_resample <- glm_wflow %>%
  fit_resamples(resamples = heart_fold, control = keep_pred)
collect_metrics(glm_resample)
show_best(rf_tuned, metric = "roc_auc", n = 1)
show_best(bt_tuned, metric = "roc_auc", n = 1)
show_best(kn_tuned, metric = "roc_auc", n = 1)

## fit models
glm_fit <- fit(glm_wflow, heart_train)
rf_wflow_tuned <- rf_wflow %>%
  finalize_workflow(select_best(rf_tuned, metric = "roc_auc"))
rf_fit <- fit(rf_workflow_tuned, heart_train)
bt_wflow_tuned <- bt_wflow %>%
  finalize_workflow(select_best(bt_tuned, metric = "roc_auc"))
bt_fit <- fit(bt_wflow_tuned, heart_train)
kn_wflow_tuned <- kn_wflow %>%
  finalize_workflow(select_best(kn_tuned, metric = "roc_auc"))
kn_fit <- fit(kn_wflow_tuned, heart_train)

## final result
# glm_res <- predict(glm_fit, new_data = heart_test) 
# glm_res %>%
#   bind_cols(heart_test %>% select(heart_disease)) %>%
#   accuracy(truth = heart_disease, estimate =.pred_class)
# predict(glm_fit, new_data = heart_test, type = "prob") %>%
#   bind_cols(glm_res,
#             heart_test %>% select(heart_disease)) %>%
#   roc_auc(truth = heart_disease, .pred_0)
  
# rf_res <- predict(rf_fit, new_data = heart_test) 
# rf_res %>%
#   bind_cols(heart_test %>% select(heart_disease)) %>%
#   accuracy(truth = heart_disease, estimate = .pred_class)
# predict(rf_fit, new_data = heart_test, type = "prob") %>%
#   bind_cols(rf_res,
#             heart_test %>% 
#               select(heart_disease)) %>%
#   roc_auc(truth = heart_disease, .pred_0)

bt_res <- predict(bt_fit, new_data = heart_test) 
bt_res %>%
  bind_cols(heart_test %>% select(heart_disease)) %>%
  accuracy(truth = heart_disease, estimate = .pred_class)
predict(bt_fit, new_data = heart_test, type = "prob") %>%
  bind_cols(bt_res,
            heart_test %>% 
              select(heart_disease)) %>%
  roc_auc(truth = heart_disease, .pred_0)

# kn_res <- predict(kn_fit, new_data = heart_test) 
# kn_res %>%
#   bind_cols(heart_test %>% select(heart_disease)) %>%
#   accuracy(truth = heart_disease, estimate = .pred_class)
# predict(kn_fit, new_data = heart_test, type = "prob") %>%
#   bind_cols(kn_res,
#             heart_test %>% 
#               select(heart_disease)) %>%
#   roc_auc(truth = heart_disease, .pred_0)
