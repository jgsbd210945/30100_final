library(tidyverse)
library(vdemdata)
library(glmnet)

load('WVS_Cross-National_Wave_7_rData_v6_0.rdata')
wvs7 <- `WVS_Cross-National_Wave_7_v6_0` |> as.tibble()
load('WV6_Data_R_v20201117.rdata')
wvs6 <- `WV6_Data_R_v20201117` |> as.tibble()

workingvdem <- tibble(vdem) |>
  select(c(country_name, country_text_id, year, v2x_polyarchy, v2x_regime_amb)) |>
  filter(year >= 2005)

workingvdem <- workingvdem |>
  group_by(country_text_id) |>
  arrange(year) |> # Should already be the case, but just in case it's not
  mutate(diff_polyarchy = v2x_polyarchy - lag(v2x_polyarchy)) |>
  ungroup() |>
  arrange(country_text_id)

workingvdem <- workingvdem |> group_by(country_text_id) |>
  # Did electoral democracy decrease in a non-entrenched autocracy?
  mutate(backslided = (diff_polyarchy < -0.01) & (v2x_regime_amb) > 3) |>
  # Did it do it at least three years in a row?
  mutate(backslided = (backslided & lag(backslided) & lag(lag(backslided))) |
           lead(backslided) & backslided & lag(backslided) |
           lead(lead(backslided)) & lead(backslided) & backslided) |>
  filter(year > 2009)
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

#merged |>
#  dplyr::select(-c(year, country_text_id, country_name)) |>
#  cor() |>
#  corrplot::corrplot(method = "number", type = "upper") # Unhelpful!


grid <- 10^seq(10, -2, length = 100)
y7 <- merged$v2x_polyarchy
z7 <- merged$diff_polyarchy |> scale()
merged_lasso <- dplyr::select(merged, matches("^Q([0-9]+)"))

lasso_mod <- glmnet(data.matrix(merged_lasso), y7, alpha = 1, lambda = grid)
plot(lasso_mod)

set.seed(1)
cv7_out <- cv.glmnet(data.matrix(merged_lasso), y7, alpha = 1)
plot(cv7_out)

top_features7 <- coef(cv7_out, s = "lambda.1se")
top_features7 <- top_features7[-1,]
top_features7 <- top_features7[top_features7 != 0]
sort(abs(top_features7), decreasing = TRUE) |> head(15)

lasso_modz <- glmnet(data.matrix(merged_lasso), z7, alpha = 1, lambda = grid)
plot(lasso_modz)
# However, can't find most important features because everything's so small!

merged7_select <- merged_lasso |> dplyr::select(names(top_features7))
lasso_m7s <- glmnet(data.matrix(merged7_select), y7, alpha = 1, lambda = grid) # y is still well-being
par(mar = c(5, 4, 4, 8), xpd = TRUE) # Need space for the legend
plot(lasso_m7s)

# Making the legend
lasso_coefs7 <- coef(lasso_m7s)
var_names7 <- rownames(lasso_coefs7)[-1]
colors <- seq_len(length(var_names7))
legend("right", legend = var_names7, col = colors, lty = 1, cex = 0.8, inset = c(-0.25, 0))






wvs6_test <- dplyr::select(wvs6, B_COUNTRY_ALPHA, matches("^V([0-9]+)")) |>
  dplyr::select(-c(V1, V2, V2A, V3)) |>
  rename(country_text_id = B_COUNTRY_ALPHA)

wvs6_test$year = 2013 # no specific year, assuming 2013.

isnum <- sapply(wvs6_test, is.numeric) # Cols where we're working with #'s
# For those cols, remove all vals less than 0.
wvs6_test[isnum] <- lapply(wvs6_test[isnum], \(x) ifelse(x < 0, NA, x))

# By country
wvs6_test <- wvs6_test |>
  group_by(year, country_text_id) |>
  summarize(across(everything(), \(x) mean(x, na.rm = TRUE)))
isnum <- sapply(wvs6_test, is.numeric) # Cols where we're working with #'s
# For those cols, remove all vals less than 0.
wvs6_test[isnum] <- lapply(wvs6_test[isnum], \(x) ifelse(is.na(x), -1, x))

merged6 <- merge(workingvdem, wvs6_test, by = c("year", "country_text_id")) |>
  as_tibble()

y6 <- merged6$v2x_polyarchy
z6 <- merged6$diff_polyarchy |> scale()
merged6_lasso <- dplyr::select(merged6, matches("^V([0-9]+)")) |>
  dplyr::select(-matches("^v2x")) # Have to EXPLICITLY remove these.

lasso_modz <- glmnet(data.matrix(merged6_lasso), z6, alpha = 1, lambda = grid)
plot(lasso_modz)
# However, can't find most important features because everything's so small!

lasso_mod6 <- glmnet(data.matrix(merged6_lasso), y6, alpha = 1, lambda = grid)
plot(lasso_mod6)

set.seed(1)
cv6_out <- cv.glmnet(data.matrix(merged6_lasso), y6, alpha = 1)
plot(cv6_out)

top_features6 <- coef(cv6_out, s = "lambda.1se")
top_features6 <- top_features6[-1,]
top_features6 <- top_features6[top_features6 != 0]
sort(abs(top_features6), decreasing = TRUE) |> head(15)

merged6_select <- merged6_lasso |> dplyr::select(names(top_features6))
lasso_m6s <- glmnet(data.matrix(merged6_select), y6, alpha = 1, lambda = grid) # y is still well-being
par(mar = c(5, 4, 4, 8), xpd = TRUE) # Need space for the legend
plot(lasso_m6s)

# Making the legend
lasso_coefs6 <- coef(lasso_m6s)
var_names6 <- rownames(lasso_coefs6)[-1]
colors <- seq_len(length(var_names6))
legend("right", legend = var_names6, col = colors, lty = 1, cex = 0.8, inset = c(-0.25, 0))




