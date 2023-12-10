
my_e_net <- function(data, droplist, target, alpha=1, title="Feature Importance", sparse=FALSE){
  # Prepare data by dropping NAs and any features not of interest
  #drop other Air Quality variables
  aq_cols <- c(
    "AQI_mean",
    "AQI_adj_mean",   
    "PM1_mean",
    "PM2.5_Dyson",
    "PM10_mean",
    "elevated_hours"
  )
  not_targets <- aq_cols[!aq_cols == target]
  
  enet_data <- data %>%  drop_na() %>% dplyr::select(-any_of(droplist), -any_of(not_targets))
  
  #Set target variable as an enriched expression so it can be used inside of tidy select funcs
  var <- dplyr::enexpr(target)
  
  
  #Create X matrix and y vector by separating target var from DF
  X <- makeX(enet_data %>% dplyr::select(-!!var))
  y <- enet_data[[target]]
  
  
  # Fit a Lasso model with 10-fold cross-validation
  set.seed(42)
  cvfit <- cv.glmnet(X, y, alpha = alpha)  # alpha = 1 for Lasso
  
  # Find the optimal lambda (alpha) from cross-validation
  best_lambda <- cvfit$lambda.1se #Optional: choose lambda.min or for min. lambda or lambda 1.se
  
  #Calculate the mean square error for the chosen lambda
  mse <- cvfit$cvm[match(best_lambda, cvfit$lambda)]
  
  # Create a data frame for plotting feature importance
  feature_importance <- as.data.frame(as.matrix(coef(cvfit, best_lambda)))
  
  feature_importance$features <- rownames(feature_importance)
  
  colnames(feature_importance) <- c("Coefficient", "Feature")
  
  # Create a feature importance plot
  # remove Intercept and feautres with coef == 0
  p <- ggplot(
    feature_importance |> filter(Feature != "(Intercept)", Coefficient !=0), 
    aes(x = reorder(Feature, -Coefficient), y = Coefficient)) +
    geom_bar(stat = "identity", fill = "blue") +
    labs(title = title, 
         subtitle = paste("MSE:", mse), #add MSE for each version to plot
         x = "Feature", 
         y = "Coefficient") +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.margin = margin(1,1,1,2,"cm"))
  
  
  return(p)
  
}
