#-------------------------------------------------------------------------------
#                       DEFINE `RUN_ANALYSIS() FUNCTION`
#    Modular callable function to run Ridge, Elastic-Net, Lasso, Random 
#                      Forest, & XGBOOST on any clean df
#-------------------------------------------------------------------------------

library(tidyverse)
library(caret)
library(glmnet)
library(ranger)
library(xgboost)

run_analysis <- function(df, target = "log_price", seed = 123, nfolds = 5, 
                         print_results=TRUE, plot_cv = TRUE, show_prog = FALSE){
  
  start_time <- Sys.time()
  #--------------------------- PARTITION & CONFIGURE ---------------------------
  #                           (75% train ; 25% test)
  set.seed(seed)
  train_index <- createDataPartition(df[[target]], p = 0.75, list = FALSE)
  train_data <- df[train_index, ]
  test_data  <- df[-train_index, ]
  form <- as.formula(paste(target, "~ ."))
  
  # Design Matrices for glmnet/xgboost
  x_train <- model.matrix(form, data = train_data)[, -1]
  x_test  <- model.matrix(form, data = test_data)[, -1]
  y_train <- train_data[[target]]
  y_test  <- test_data[[target]]
  
  # Helper functions for rmse, Rsquared, and % error
  get_rmse <- function(actual, predicted) {
    sqrt(mean((actual - predicted)^2))
  }
  get_perc_error <- function(rmse) {
    round(100*(exp(rmse) - 1), 2)
  }
  get_Rsquare <- function(rmse) {
    round(1-(rmse^2/var(y_test)),4)
  }
  
  
  #---------------------------------- RIDGE ------------------------------------
  if(show_prog){print("Running RIDGE Cross-Validation...\n")}
  # Train Model
  set.seed(seed)
  cv_ridge <- cv.glmnet(as.matrix(x_train),
                        as.matrix(y_train), 
                        alpha = 0, 
                        nfolds = 10)
  if(plot_cv){plot(cv_ridge)}
  lambda_ridge_cv <- cv_ridge$lambda.min
  if(show_prog){cat("RIDGE Lambda:",lambda_ridge_cv,"\n")}
  
  if(show_prog){print("Running RIDGE Regression...\n")}
  model_ridge <- glmnet(x_train, 
                         y_train, 
                         alpha = 0, 
                         lambda = lambda_ridge_cv, 
                         thresh = 1e-12
  )
  
  # Test Performance
  predictions_ridge_train <- predict(model_ridge, x_train)
  predictions_ridge_test <- predict(model_ridge, x_test)
  
  rmse_ridge_train <- get_rmse(y_train, predictions_ridge_train)
  rmse_ridge_test <- get_rmse(y_test, predictions_ridge_test)
  Rsquare_ridge <- get_Rsquare(rmse_ridge_test)
  perc_error_ridge <- get_perc_error(rmse_ridge_test)

  
  
  #---------------------------------- ElNet ------------------------------------
  if(show_prog){print("Running Elastic-Net Cross-Validation...\n")}
  # Train Model
  set.seed(seed)
  cv_ElNet <- cv.glmnet(as.matrix(x_train),
                        as.matrix(y_train), 
                        alpha = 0.5, 
                        nfolds = 10)
  if(plot_cv){plot(cv_ElNet)}
  lambda_ElNet_cv <- cv_ElNet$lambda.min
  if(show_prog){cat("Elastic-Net Lambda:",lambda_ElNet_cv,"\n")}
  
  if(show_prog){print("Running Elastic-Net Regression...\n")}
  model_ElNet <- glmnet(x_train, 
                        y_train, 
                        alpha = 0.5, 
                        lambda = lambda_ElNet_cv, 
                        thresh = 1e-12
  )
  
  # Test Performance
  predictions_ElNet_train <- predict(model_ElNet, x_train)
  predictions_ElNet_test <- predict(model_ElNet, x_test)
  
  rmse_ElNet_train <- get_rmse(y_train, predictions_ElNet_train)
  rmse_ElNet_test <- get_rmse(y_test, predictions_ElNet_test)
  Rsquare_ElNet <- get_Rsquare(rmse_ElNet_test)
  perc_error_ElNet <- get_perc_error(rmse_ElNet_test)
  
  
  
  
  #---------------------------------- LASSO ------------------------------------
  if(show_prog){print("Running LASSO Cross-Validation...\n")}
  # Train Model
  set.seed(seed)
  cv_lasso <- cv.glmnet(as.matrix(x_train),
                        as.matrix(y_train), 
                        alpha = 1, 
                        nfolds = 10)
  if(plot_cv){plot(cv_lasso)}
  lambda_lasso_cv <- cv_lasso$lambda.min
  cat("LASSO Lambda:",lambda_lasso_cv,"\n")
  
  if(show_prog){print("Running LASSO Regression...\n")}
  model_lasso <- glmnet(x_train, 
                         y_train, 
                         alpha = 1, 
                         lambda = lambda_lasso_cv, 
                         thresh = 1e-12
  )
  
  # Test Pelassoormance
  predictions_lasso_train <- predict(model_lasso, x_train)
  predictions_lasso_test <- predict(model_lasso, x_test)
  
  rmse_lasso_train <- get_rmse(y_train, predictions_lasso_train)
  rmse_lasso_test <- get_rmse(y_test, predictions_lasso_test)
  Rsquare_lasso <- get_Rsquare(rmse_lasso_test)
  perc_error_lasso <- get_perc_error(rmse_lasso_test)
  

  #------------------------------ RANDOM FOREST --------------------------------
  if(show_prog){print("Running RANDOM FOREST Analysis...\n")}
  # Train Model
  set.seed(seed)
  model_rf <- ranger(formula = form,
                      data = train_data,
                      num.tree = 100,
                      mtry = 40,
                      min.node.size = 100,
                      importance = "impurity"
  )
  
  # Test Performance
  predictions_rf_train <- predict(model_rf, data = train_data)$predictions
  predictions_rf_test <- predict(model_rf, data = test_data)$predictions
  
  rmse_rf_train <- get_rmse(y_train, predictions_rf_train)
  rmse_rf_test <- get_rmse(y_test, predictions_rf_test)
  Rsquare_rf <- get_Rsquare(rmse_rf_test)
  perc_error_rf <- get_perc_error(rmse_rf_test)
  
  
  # Extract Feature Importance 
  importance_rf <- as.data.frame(model_rf$variable.importance) %>%
    rownames_to_column(var = "Feature") %>%
    arrange(desc(model_rf$variable.importance),"\n")
  
  top_features_rf <- head(importance_rf$Feature, n = 10)
  bottom_features_rf <- tail(importance_rf$Feature, n = 10)
  
  
  #---------------------------------- XGBOOST ------------------------------------
  if(show_prog){print("Running XGBOOST Analysis...\n")}
  # Train Model
  set.seed(seed)
  model_xgboost <- xgboost(data = x_train,
                            label = y_train,
                            nrounds = 100,
                            objective = "reg:squarederror",
                            eta = 0.1,
                            max_depth = 6,
                            subsample = 0.8,
                            colsample_bytree = 0.6,
                            verbose = 0
  )
  
  
  # Test Performance
  predictions_xgboost_train <- predict(model_xgboost, x_train)
  predictions_xgboost_test <- predict(model_xgboost, x_test)
  
  rmse_xgboost_train <- get_rmse(y_train, predictions_xgboost_train)
  rmse_xgboost_test <- get_rmse(y_test, predictions_xgboost_test)
  Rsquare_xgboost <- get_Rsquare(rmse_xgboost_test)
  perc_error_xgboost <- get_perc_error(rmse_xgboost_test)
  
  # Extract Feature Importance 
  importance_xgboost <- as.data.frame(xgb.importance(model = model_xgboost))%>%
    select(c(Feature,Gain))
  top_features_xgboost <- head(importance_xgboost$Feature, n=10)
  bottom_features_xgboost <- tail(importance_xgboost$Feature, n=10)
  #------------------------------- OUTPUT RESULTS ------------------------------
  end_time <- Sys.time()
  duration_secs <- as.numeric(difftime(end_time, start_time, units = "secs"))
  duration_mins <- floor(duration_secs / 60)
  duration_remainder_secs <- round(duration_secs %% 60)
  duration_msg <- paste0("Analysis duration: ", duration_mins, " minutes, ", duration_remainder_secs, " seconds")
  if (show_prog) message(duration_msg)

  results <- tibble(
    Model = c("RIDGE", "ElasticNet", "LASSO", "RANDOM FOREST", "XGBOOST"),
    
    "TRAIN RMSE (log)" = round(c(rmse_ridge_train, 
                         rmse_ElNet_train, 
                         rmse_lasso_train, 
                         rmse_rf_train, 
                         rmse_xgboost_train),4),
    
    "TEST RMSE (log)" = round(c(rmse_ridge_test, 
                        rmse_ElNet_test,
                        rmse_lasso_test, 
                        rmse_rf_test,
                        rmse_xgboost_test),4),
    
    "R²" = c(Rsquare_ridge,
                Rsquare_ElNet,
                Rsquare_lasso,
                Rsquare_rf,
                Rsquare_xgboost),
    
    "Avg. Prediction Error (%)" = c(perc_error_ridge, 
                                    perc_error_ElNet,
                                    perc_error_lasso,
                                    perc_error_rf,
                                    perc_error_xgboost)
  )
  if(print_results) print(results)
  
  top_features <- tibble(
    RANDOM_FOREST = top_features_rf,
    XGBOOST = top_features_xgboost
  )
  
  bottom_features <- tibble(
    RANDOM_FOREST = bottom_features_rf,
    XGBOOST = bottom_features_xgboost
  )
  
  list(
    cv_plots = list(ridge = cv_ridge, 
                    ElNet = cv_ElNet, 
                    lasso = cv_lasso),
    metrics = results,
    feature_importance = list(top_features = top_features,
                              bottom_features = bottom_features),
    model_ridge = model_ridge,
    model_ElNet = model_ElNet,
    model_lasso = model_lasso,
    model_rf = model_rf,
    model_xgboost = model_xgboost
  )
  
  
}



#-------------------------- DISPLAY RESULTS FUNCTION ---------------------------
library(reactable)
library(htmltools)

display_results <- function(A_results, RMSE = TRUE, Feature_Importance = TRUE,
                            Cross_Val_Plots = TRUE) {
  
  if (Cross_Val_Plots) {
    cat("**Lambda Cross-Validation Plots**\n\n")
    plot(A_results$cv_plots$ridge, main = "RIDGE")
    plot(A_results$cv_plots$ElNet, main = "Elastic-Net")
    plot(A_results$cv_plots$lasso, main = "LASSO")
  }
  
  if (RMSE) {
    metrics <- reactable(A_results$metrics,
                    defaultColDef = colDef(align = "center",
                                           headerStyle = list(fontFamily = "Arial",
                                                              whiteSpace = "nowrap",
                                                              textOverflow = "fill",
                                                              fontSize = "12px",
                                                              fontWeight = "bold",
                                                              background = "black",
                                                              color = "white"),
                                           style = list(fontFamily = "Arial",
                                                        whiteSpace = "nowrap",
                                                        textOverflow = "fill",
                                                        fontSize = "12px",
                                                        fontWeight = "normal",
                                                        background = "white",
                                                        color = "black")),
              fullWidth = FALSE,
              width = 800)
  }
  
  if (Feature_Importance) {
    top_table <- reactable(A_results$feature_importance$top_features,
                       defaultColDef = colDef(align = "center",
                                           headerStyle = list(fontFamily = "Arial",
                                                              whiteSpace = "nowrap",
                                                              textOverflow = "fill",
                                                              fontSize = "12px",
                                                              fontWeight = "bold",
                                                              background = "#2e7d32",
                                                              color = "white"),
                                           style = list(fontFamily = "Arial",
                                                        whiteSpace = "nowrap",
                                                        textOverflow = "fill",
                                                        fontSize = "12px",
                                                        fontWeight = "normal",
                                                        background = "white",
                                                        color = "black")),
              fullWidth = FALSE,
              width = 400)
    bottom_table <- reactable(A_results$feature_importance$bottom_features,
                    defaultColDef = colDef(align = "center",
                                           headerStyle = list(fontFamily = "Arial",
                                                              whiteSpace = "nowrap",
                                                              textOverflow = "fill",
                                                              fontSize = "12px",
                                                              fontWeight = "bold",
                                                              background = "#b1282f",
                                                              color = "white"),
                                           style = list(fontFamily = "Arial",
                                                        whiteSpace = "nowrap",
                                                        textOverflow = "fill",
                                                        fontSize = "12px",
                                                        fontWeight = "normal",
                                                        background = "white",
                                                        color = "black")),
              fullWidth = FALSE,
              width = 400)
  }
  
  tagList(
    # Full-width block
    div(
      style = "margin-bottom: 30px;",
      h4("Prediction Metrics"),
      metrics
    ),
    
    # Side-by-side block
    div(
      style = "display: flex; center-content: space-between;",
      
      # Table 1 (left)
      div(
        style = "width: 49%;",
        h4("Feature Importance (Top 10)"),
        top_table
      ),
      
      # Table 2 (right)
      div(
        style = "width: 49%;",
        h4("Feature Importance (Bottom 10)"),
        bottom_table
      )
    )
  )
}

