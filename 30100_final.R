library(tidyverse)
library(vdemdata)
library(glmnet)
library(tidymodels)
library(xgboost)
library(vip)
library(randomForest)

load('Data/WVS_Cross-National_Wave_7_rData_v6_0.rdata')
wvs7 <- `WVS_Cross-National_Wave_7_v6_0` |> as.tibble()

workingvdem <- tibble(vdem) |>
  filter(year > 1975) |>
  group_by(country_text_id) |>
  arrange(year) |>
  mutate(diff_polyarchy = v2x_polyarchy - lag(v2x_polyarchy)) |>
  # Backsliding boolean variable.
  # I'm quantitatively defining it as having decreased in electoral
  # democracy score by more than .005 and having decreased by at
  # least 0.03 in the last two years.
  # I also filtered so that the regime has to be at least some semblance
  # of an electoral autocracy (at least) so that hard autocracies getting
  # more autocratic aren't included.
  mutate(backslided = (diff_polyarchy < -0.005) &
           (v2x_regime_amb > 2) &
           (lag(v2x_polyarchy, 2) - v2x_polyarchy > 0.03)) |>
  # Filling it out
  mutate(backslided = (diff_polyarchy < -0.001) &
           (backslided |
              ((lag(backslided) & lag(backslided, 2))|
                 lead(backslided) & lag(backslided) |
                 lead(backslided, 2) & lead(backslided)))) |>
  ungroup() |>
  filter(year > 2009) |>
  arrange(country_text_id) |>
  select(country_name,
         country_text_id,
         year,
         v2x_polyarchy,
         v2x_libdem, 
         v2x_regime_amb, 
         diff_polyarchy, 
         backslided)

# 2022/2023 values (is NA for ones in backsliding. Let's make that True.)
workingvdem$backslided <- ifelse(is.na(workingvdem$backslided),
                                 TRUE,
                                 workingvdem$backslided)

isnum_lowna <- apply(workingvdem, 2, function(col){
  sum(is.na(col))
})
# The 1 is for South Sudan here.
na_rows <- apply(workingvdem, 1, function(row){sum(is.na(row))})
missing_rows <- na_rows != 0
workingvdem <- workingvdem[!missing_rows,]

wvs_test <- dplyr::select(wvs7, A_YEAR, B_COUNTRY_ALPHA, matches("^Q([0-9]+)")) |>
  rename(year = A_YEAR, country_text_id = B_COUNTRY_ALPHA)

isnum <- sapply(wvs_test, is.numeric) # Cols where we're working with #'s
# For those cols, remove all vals less than 0.
wvs_test[isnum] <- lapply(wvs_test[isnum], \(x) ifelse(x < 0, NA, x))

# By country
wvs_test <- wvs_test |>
  group_by(year, country_text_id) |>
  summarize(across(everything(), \(x) mean(x, na.rm = TRUE)))
isnum <- sapply(wvs_test, is.numeric) # Cols where we're working with #'s
# For those cols, remove all vals less than 0.
wvs_test[isnum] <- lapply(wvs_test[isnum], \(x) ifelse(is.na(x), -1, x))

merged <- merge(workingvdem, wvs_test, by = c("year", "country_text_id")) |>
  as_tibble()
merged$backslided <- factor(merged$backslided)

grid <- 10^seq(10, -2, length = 100)
y <- merged$v2x_polyarchy
z <- merged$diff_polyarchy |> scale()
merged_lasso <- dplyr::select(merged, matches("^Q([0-9]+)"))

# Lasso
glmnet(data.matrix(merged_lasso), y, alpha = 1, lambda = grid) |> plot()

set.seed(1)
cv_out <- cv.glmnet(data.matrix(merged_lasso), y, alpha = 1) 
plot(cv_out)

top_features <- coef(cv_out, s = "lambda.1se")
top_features <- top_features[-1,]
top_features <- top_features[top_features != 0]
sort(abs(top_features), decreasing = TRUE) |> head(15)

glmnet(data.matrix(merged_lasso), z, alpha = 1, lambda = grid) |> plot()
# However, can't find most important features because everything's so small!

merged_select <- merged_lasso |> dplyr::select(names(top_features))
lasso_ms <- glmnet(data.matrix(merged_select), y, alpha = 1, lambda = grid) # y is still well-being
par(mar = c(5, 4, 4, 8), xpd = TRUE) # Need space for the legend
plot(lasso_ms)

# Making the legend
lasso_coefs <- coef(lasso_ms)
var_names <- rownames(lasso_coefs)[-1]
colors <- seq_len(length(var_names))
legend("right", legend = var_names, col = colors, lty = 1, cex = 0.8, inset = c(-0.25, 0))


## Preprocessing
model_data <- dplyr::select(merged, country_name, v2x_polyarchy, diff_polyarchy, backslided, matches(var_names))

set.seed(1)
data_split <- initial_split(model_data, prop = 0.5)
train_data <- training(data_split)
test_data  <- testing(data_split)

rec <- recipe(v2x_polyarchy ~ ., data = train_data) |>
  step_select(all_numeric()) |>
  update_role(diff_polyarchy, new_role = "ID")

## Cross-validation object
cv_folds <- vfold_cv(train_data, v = 5, repeats = 1)

# xgboost vs randomForests
model_rf100 = rand_forest(trees = 100) |> 
  set_engine("ranger") |>
  set_mode("regression")

model_xgboost <- boost_tree(trees = 100, mtry=5) |> 
  set_mode("regression") |> 
  set_engine("xgboost")

wf_set <- workflow_set(
  preproc = list(rec),
  models = list(
    xgboost = model_xgboost,
    random_forest = model_rf100)
)
  
wf_set_fitted <- workflow_map(wf_set, "fit_resamples", resamples = cv_folds)
wf_set_fitted |> collect_metrics()

xg_grid <- grid_regular(learn_rate(), levels = 5)

model_xgboost_tune <- boost_tree(trees = 500, mtry=5, learn_rate = tune()) |> 
  set_mode("regression") |> 
  set_engine("xgboost")

wf <- workflow() |>
  add_model(model_xgboost_tune) |>
  add_recipe(rec)

model_res <- wf |>
  tune_grid(resamples = cv_folds,
            grid = xg_grid,
            control = control_grid(save_pred = TRUE))
collect_metrics(model_res)

best_tree <- model_res |> select_best(metric = "rmse")
final_wf <- wf |> finalize_workflow(best_tree)
final_fit <- final_wf |> last_fit(data_split)
final_fit |> collect_metrics()

final_fit |>
  collect_predictions() |>
  ggplot(aes(x = v2x_polyarchy, y = .pred)) +
  geom_abline(color = 'red', linewidth = 1) +
  geom_point() +
  labs(title = "Comparison of Prediction vs. Actual Electoral Democracy Score") +
  xlab("Actual Electoral Democracy Score") +
  ylab("Predicted Electoral Democracy Score")

final_fit |>
  extract_fit_parsnip() |>
  vip(num_features = 20)

# On All the data...

set.seed(1)
full_split <- initial_split(merged, prop = 0.5)
train_full <- training(full_split)
test_full <- testing(full_split)

rec_full <- recipe(v2x_polyarchy ~ ., data = train_full) |>
  step_select(all_numeric()) |>
  update_role(year, diff_polyarchy, v2x_regime_amb, new_role = "ID")
wf_full <- workflow() |>
  add_model(model_xgboost) |>
  add_recipe(rec_full)

cv_full <- vfold_cv(train_full, v = 5, repeats = 1)

model_full <- wf_full |>
  tune_grid(resamples = cv_full,
            grid = xg_grid,
            control = control_grid(save_pred = TRUE))
collect_metrics(model_full)

best_tree_full <- model_full |> select_best(metric = "rmse")
final_wf_full <- wf_full |> finalize_workflow(best_tree_full)
final_full <- final_wf_full |> last_fit(full_split)
final_full |> collect_metrics()

final_full |>
  collect_predictions() |>
  ggplot(aes(x = v2x_polyarchy, y = .pred)) +
  geom_abline(color = 'red', linewidth = 1) +
  geom_point() +
  labs(title = "Comparison of Prediction vs. Actual Electoral Democracy Score") +
  xlab("Actual Electoral Democracy Score") +
  ylab("Predicted Electoral Democracy Score")

final_full |>
  extract_fit_parsnip() |>
  vip(num_features = 20)


## Hmm...How about backsliding?
rec_diff <- recipe(diff_polyarchy ~ ., data = train_data) |>
  step_select(all_numeric()) |>
  update_role(v2x_polyarchy, new_role = "ID")
wf_diff <- workflow() |>
  add_model(model_xgboost) |>
  add_recipe(rec_diff)

model_diff <- wf_diff |>
  tune_grid(resamples = cv_folds,
            grid = xg_grid,
            control = control_grid(save_pred = TRUE))
collect_metrics(model_diff)

best_tree_diff <- model_diff |> select_best(metric = "rmse")
final_wf_diff <- wf_diff |> finalize_workflow(best_tree_diff)
final_diff <- final_wf_diff |> last_fit(data_split)
final_diff |> collect_metrics()

final_diff |>
  collect_predictions() |>
  ggplot(aes(x = diff_polyarchy, y = .pred)) +
  geom_abline(color = 'red', linewidth = 1) +
  geom_point() +
  labs(title = "Comparison of Prediction vs. Actual Difference in Electoral Democracy Score") +
  xlab("Actual Difference") +
  ylab("Predicted Difference")

final_diff |>
  extract_fit_parsnip() |>
  vip(num_features = 20)


# Final thought...classification?
rec_class <- recipe(backslided ~ ., data = train_data) |>
  step_select(backslided, all_numeric()) |>
  update_role(v2x_polyarchy, diff_polyarchy, new_role = "ID")

# xgboost
model_xgboost_class <- boost_tree(trees = 500, mtry=5, learn_rate = tune()) |> 
  set_mode("classification") |> 
  set_engine("xgboost")

wf_class <- workflow() |>
  add_model(model_xgboost_class) |>
  add_recipe(rec_class)

model_class <- wf_class |>
  tune_grid(resamples = cv_folds,
            grid = xg_grid,
            control = control_grid(save_pred = TRUE),
            metric_set(roc_auc))
collect_metrics(model_class)

best_class <- model_class |> select_best(metric = "roc_auc")
final_wf_class <- wf_class |> finalize_workflow(best_class)
final_class <- final_wf_class |> last_fit(data_split)
final_class |> collect_metrics()

final_class |>
  collect_predictions() |>
  roc_curve(backslided, .pred_TRUE) |>
  autoplot()
final_class |>
  extract_fit_parsnip() |>
  vip(num_features = 20)
