load('Data/WV6_Data_R_v20201117.rdata')
wvs6 <- `WV6_Data_R_v20201117` |> as.tibble()

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
