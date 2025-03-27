#-------------------------------------------------------------------------------
#                              AMENITIES SCORING
#                uses a lasso regression to score each unique 
#                        amenity's effect on log_price
#-------------------------------------------------------------------------------
library(tidyverse)
library(glmnet)



# ------------------------ PROCESS AMENITIES IN DF_BASE ------------------------
dfl_mapped <- df_base %>%
  mutate(row_id = row_number()) %>%
  mutate(amenities = str_remove_all(amenities, "\\[|\\]|\"")) %>%
  separate_rows(amenities, sep = ", ") %>%
  left_join(df_amenities_encoding_key, by = "amenities")



# ----------------------- CREATE ONE-HOT ENCODED MATRIX ------------------------
proxy_matrix <- dfl_mapped %>%
  filter(!is.na(proxy_amenity)) %>%
  distinct(row_id, proxy_amenity) %>%
  mutate(value = 1) %>%
  pivot_wider(names_from = proxy_amenity,
              values_from = value,
              values_fill = 0) %>%
  arrange(row_id)

# Save row_id and proxy_matrix separately
row_ids <- proxy_matrix$row_id
x <- proxy_matrix %>% select(-row_id) %>% as.matrix()
y <- df_base$log_price[row_ids]  # Assign prices to row_ids used in x



#------------------------------ TRAIN LASSO MODEL ------------------------------
set.seed(123)
cv_lasso_AS <- cv.glmnet(as.matrix(x),
                         as.matrix(y), 
                         alpha = 1, 
                         nfolds = 10)
lambda_lasso_cv_AS <- cv_lasso_AS$lambda.min
cat("LASSO Lambda:",lambda_lasso_cv_AS,"\n")

model_lasso_AS <- glmnet(x, 
                         y, 
                         alpha = 1, 
                         lambda = lambda_lasso_cv_AS, 
                         thresh = 1e-12)

#Extract coefficients (our unique amenities' scores)
coef_df <- coef(model_lasso_AS, s = "lambda.min") %>%
  as.matrix() %>%
  as.data.frame() %>%
  rownames_to_column("proxy_amenity") %>%
  rename(score = s1) %>%
  filter(proxy_amenity != "(Intercept)", score != 0)



# ----------------- COMPUTE AMENITIES_SCORE PER LISTING ------------------------
valid_names <- intersect(colnames(x), coef_df$proxy_amenity)

aligned_scores <- coef_df %>%
  filter(proxy_amenity %in% valid_names) %>%
  arrange(match(proxy_amenity, colnames(x))) %>%
  pull(score)

x_matched <- x[, valid_names, drop = FALSE]

amenity_scores <- tibble(
  row_id = row_ids,
  amenities_score = as.numeric(x_matched %*% aligned_scores)
)



# ------------------------ SAVE TO DF_BASE_SCORED ------------------------------
df_base_scored <- df_base %>%
  mutate(row_id = row_number()) %>%
  left_join(amenity_scores, by = "row_id") %>%
  select(-row_id)

