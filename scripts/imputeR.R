library("VIM")
library(magrittr)
library(caret)

validate_and_impute <- function(df, dependent_vars, explanatory_vars) {
    
  # Combine variables for subsetting
  vars_to_include <- c("FSA",dependent_vars, explanatory_vars)
  
  # Subset the DataFrame to only include rows with no missing values for specified variables
  complete_cases_df <- na.omit(df[, vars_to_include],cols=explanatory_vars)
  
  # Sample 10% of the data indices for validation
  validation_indices <- sample(1:nrow(complete_cases_df), size = 0.02 * nrow(complete_cases_df))
  
  # Create validation and training sets
  validation_df <- complete_cases_df[validation_indices, ]
  training_df <- complete_cases_df[-validation_indices, ]
  
  # Define training control
  train_control <- trainControl(method = "cv", number = 10)  # 10-fold CV
  
  alpha_values <- seq(0, 1, length.out = 30)
  mse_results <- data.frame(alpha = alpha_values, MSE = rep(NA, 30))
  
  for (alpha in alpha_values) {
    fold_mse <- c()
    fold_mae <- c()
    for (i in 1:train_control$number) {
      # Create indices for the fold
      fold_indices <- createFolds(training_df[[dependent_vars]], k = train_control$number, list = TRUE, returnTrain = TRUE)
      
      # Subset fold data
      fold_df <- training_df[fold_indices[[i]], ]
      training_df2 <- training_df
      training_df2[fold_indices[[i]],dependent_vars] <- NA
      
      # Perform imputation
      df_knn <- kNN(training_df2, variable = dependent_vars, dist_var = explanatory_vars, k = 5)
      df_ranger <- rangerImpute(as.formula(paste(dependent_vars, "~", paste(explanatory_vars, collapse = " + "))), training_df2)
      
      # Combine model predictions
      combined_predictions <- alpha * df_knn[[dependent_vars]] + (1 - alpha) * df_ranger[[dependent_vars]]
      
      # Calculate MSE for the fold
      true_values <- fold_df[[dependent_vars]]
      fold_mse[i] <- mean((true_values - combined_predictions[fold_indices[[i]]])^2)
      fold_mae[i] <- median(abs(true_values - combined_predictions[fold_indices[[i]]]))
    }
    
    # Store the average MSE across all folds for this alpha
    mse_results$MSE[mse_results$alpha == alpha] <- mean(fold_mse)
    mse_results$MAE[mse_results$alpha == alpha] <- mean(fold_mae)
  }
  
  # Find the alpha that minimizes MSE
  optimal_alpha <- mse_results$alpha[which.min(mse_results$MSE)]
  
  mse_optimal <- min(mse_results$MSE)
  mse_alpha0 <- mse_results$MSE[1]
  mse_alpha1 <- mse_results$MSE[length(alpha_values)]
  
  list(optimal_alpha = optimal_alpha, MSE_optimal = mse_optimal, MSE_alpha0 = mse_alpha0, MSE_alpha1 = mse_alpha1)
  
  #Validation set MSE & MAE
  validation_df <- complete_cases_df
  validation_df[validation_indices, dependent_vars] <- NA
  df_knn <- kNN(validation_df, variable = dependent_vars, dist_var = explanatory_vars, k = 5)
  df_ranger <- rangerImpute(as.formula(paste(dependent_vars, "~", paste(explanatory_vars, collapse = " + "))), validation_df)
  combined_predictions <- optimal_alpha * df_knn[[dependent_vars]] + (1 - optimal_alpha) * df_ranger[[dependent_vars]]
  combined_predictions0 <- 0 * df_knn[[dependent_vars]] + (1 - 0) * df_ranger[[dependent_vars]]
  combined_predictions1 <- 1 * df_knn[[dependent_vars]] + (1 - 1) * df_ranger[[dependent_vars]]
  
  true_values <- complete_cases_df[validation_indices,dependent_vars]
  # Calculate validation MSE
  validation_mse <- mean((true_values - combined_predictions[validation_indices])^2)
  
  # Calculate validation MAE, avoiding division by zero
  non_zero_indices <- true_values != 0  # Indices of non-zero true values
  validation_mae <- mean(abs(true_values[non_zero_indices] - combined_predictions[validation_indices][non_zero_indices]) / true_values[non_zero_indices])
  validation_mae <- median(abs(true_values- combined_predictions[validation_indices]))
  
  # Calculate MSE for alpha = 0
  validation_mse0 <- mean((true_values - combined_predictions0[validation_indices])^2)
  
  # Calculate MAE for alpha = 0, avoiding division by zero
  validation_mae0 <- mean(abs(true_values[non_zero_indices] - combined_predictions0[validation_indices][non_zero_indices]) / true_values[non_zero_indices])
  validation_mae <- median(abs(true_values- combined_predictions[validation_indices]))
  
  # Calculate MSE for alpha = 1
  validation_mse1 <- mean((true_values - combined_predictions1[validation_indices])^2)
  
  # Calculate MAE for alpha = 1, avoiding division by zero
  validation_mae1 <- mean(abs(true_values[non_zero_indices] - combined_predictions1[validation_indices][non_zero_indices]) / true_values[non_zero_indices])
  validation_mae <- median(abs(true_values- combined_predictions[validation_indices]))
  
  df_knn <- kNN(df, variable = dependent_vars, dist_var = explanatory_vars, k = 5)
  df_ranger <- rangerImpute(as.formula(paste(dependent_vars, "~", paste(explanatory_vars, collapse = " + "))), df)
  df[,paste0(dependent_vars,"_imp")] <- optimal_alpha * df_knn[[dependent_vars]] + (1 - optimal_alpha) * df_ranger[[dependent_vars]]
  
  results <- list(
    df = df,
    optimal_alpha = optimal_alpha,
    validation_mse = sqrt(validation_mse),
    validation_mae = validation_mae,
    validation_mse0 = sqrt(validation_mse0),
    validation_mae0 = validation_mae0,
    validation_mse1 = sqrt(validation_mse1),
    validation_mae1 = validation_mae1,
    mse_optimal = sqrt(min(mse_results$MSE)),
    mae_optimal = sqrt(min(mse_results$MAE)),
    mse_alpha0 = sqrt(mse_results$MSE[1]),
    mse_alpha1 =sqrt( mse_results$MSE[length(alpha_values)])
  )
  
  return(results)
}

