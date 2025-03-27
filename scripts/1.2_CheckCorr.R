#-------------------------------------------------------------------------------
#                                CHECK CORRELATIONS
#                      Computes Correlation matrix and reveals:
#                all correlations above 0.85 ; top 10 correlations
#-------------------------------------------------------------------------------
library(tidyverse)
library(ggcorrplot)
library(viridis)

#----------------------------- PRELIMINARY CLEANING ----------------------------
# Price is in character format... remove $ and turn to numeric 
df_base <- df_listings %>%
  mutate(price = as.numeric(gsub("\\$", "", price)))%>%
  drop_na(price) %>%
  filter(price > 1) %>% # Because some places are listed for $1 (actual transaction done outside of the Airbnb platform to avoid fees)
  mutate(price = log(price))%>%
  rename(log_price = price)


df_base <- df_base %>%
  select(-c(calendar_updated, neighbourhood_group_cleansed, license)) %>%
  mutate(across(where(is.character), as.factor))


#---------------- COMPUTE CORRELATION MATRIX FOR ALL VARIABLES -----------------
compute_correlation_matrix <- function(df) {
  
  # Ensure correct numeric conversion and replace empty strings with NA
  df <- df %>%
    mutate(across(where(is.character), ~na_if(., ""))) %>%  # Convert "" to NA
    mutate(across(where(is.character), as.factor)) %>%
    mutate(across(where(is.factor), as.numeric)) %>%
    mutate(across(where(is.numeric), as.numeric)) 
  
  # Identify numeric and categorical variables
  num_vars <- df %>% select(where(is.numeric))
  cat_vars <- df %>% select(where(is.factor))
  
  ## 1️⃣ Pearson Correlation (Numeric-Numeric)
  if (ncol(num_vars) > 1) {
    num_cor <- cor(num_vars, use = "pairwise.complete.obs", method = "pearson")
  } else {
    num_cor <- NULL
  }
  
  ## DEBUG: Check num_cor before merging
  print("Checking Pearson Correlation Matrix (num_cor) BEFORE merging:")
  print(num_cor)
  
  ## 2️⃣ Spearman Correlation (Numeric-Categorical)
  if (ncol(cat_vars) > 0) {
    spearman_cor <- matrix(NA, nrow = ncol(df), ncol = ncol(df),
                           dimnames = list(names(df), names(df)))
    
    for (num_var in names(num_vars)) {
      for (cat_var in names(cat_vars)) {
        numeric_cat <- as.numeric(df[[cat_var]])  # Convert factor to numeric
        
        spearman_cor[num_var, cat_var] <- tryCatch(
          cor(df[[num_var]], 
              numeric_cat, 
              method = "spearman", 
              use = "pairwise.complete.obs"),
          error = function(e) NA  
        )
      }
    }
  } else {
    spearman_cor <- NULL
  }
  
  ## 3️⃣ Cramér’s V for Categorical Variables
  cramers_v <- function(x, y) {
    tbl <- table(x, y)
    
    if (min(dim(tbl)) <= 1) {
      return(NA)  # Avoid errors from empty tables
    }
    
    chi2 <- suppressWarnings(chisq.test(tbl, correct = FALSE)$statistic)
    n <- sum(tbl)
    min_dim <- min(nrow(tbl) - 1, ncol(tbl) - 1)
    
    return(sqrt(chi2 / (n * min_dim)))
  }
  
  cramers_v_matrix <- function(df) {
    cat_vars <- df %>% select(where(is.factor))
    pairs <- combn(names(cat_vars), 2, simplify = FALSE)
    mat <- matrix(NA, ncol = length(cat_vars), nrow = length(cat_vars),
                  dimnames = list(names(cat_vars), names(cat_vars)))
    
    for (pair in pairs) {
      mat[pair[1], pair[2]] <- mat[pair[2], pair[1]] <- cramers_v(df[[pair[1]]], df[[pair[2]]])
    }
    diag(mat) <- 1  
    return(mat)
  }
  
  if (ncol(cat_vars) > 1) {
    cat_cor <- cramers_v_matrix(df)
  } else {
    cat_cor <- NULL
  }
  
  ## 4️⃣ Merge All Correlation Matrices (Fixed Merging)
  all_vars <- colnames(df)
  
  # Ensure numeric type
  combined_cor <- matrix(as.numeric(NA), nrow = length(all_vars), ncol = length(all_vars),
                         dimnames = list(all_vars, all_vars))
  
  ## DEBUG: Check names before merging
  print("Checking row & column names of combined_cor BEFORE merging:")
  print(rownames(combined_cor))
  print(colnames(combined_cor))
  
  if (!is.null(num_cor)) {
    num_names <- rownames(num_cor)
    combined_cor[num_names, num_names] <- num_cor
  }
  
  if (!is.null(spearman_cor)) {
    combined_cor[rownames(spearman_cor), colnames(spearman_cor)] <- spearman_cor
  }
  
  if (!is.null(cat_cor)) {
    combined_cor[rownames(cat_cor), colnames(cat_cor)] <- cat_cor
  }
  
  ## Convert back to matrix
  combined_cor <- as.matrix(combined_cor)
  
  return(combined_cor)
}



# Compute full correlation matrix
full_cor_matrix <- compute_correlation_matrix(df_base)
full_cor_matrix <- as.matrix(full_cor_matrix)
full_cor_matrix[lower.tri(full_cor_matrix)] <- t(full_cor_matrix)[lower.tri(full_cor_matrix)]



#------------------------- PLOT CORRELATION MATRIX -----------------------------
viridis_palette <- viridis(100, option = "magma")
custom_palette <- c(rev(viridis_palette[15:100]), 
                    viridis_palette[15:100])
clear("viridis_palette")

ggcorrplot(full_cor_matrix, 
           type = "full",
           lab = TRUE, 
           show.legend = TRUE, 
           lab_size = 2,
           ggtheme = theme_minimal()) +
  scale_fill_gradientn(colors = custom_palette, limits = c(-1, 1))
