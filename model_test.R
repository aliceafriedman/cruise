#Function that prunes features

pruner <- function(data=df_daily, droplist=c(), target="PM2.5_Dyson"){
  #create sample size
  
  # Prepare data by dropping NAs and any features not of interest
  #drop other Air Quality variables
  targets <- c(
    "AQI_mean",
    "AQI_adj_mean",   
    "PM1_mean",
    "PM2.5_Dyson",
    "PM10_mean",
    "elevated_hours"
  )
  not_targets <- targets[!targets == target]
  
  
  ## set the seed to make your partition reproducible

  inject.dots <- function(df) {names(df) <- gsub(" ", ".", names(df));df}
  df <- data %>% drop_na() %>% dplyr::select(-any_of(droplist), -any_of(not_targets)) |> inject.dots()
  
  
  set.seed(123)  
  sample <- sample(c(TRUE, FALSE), nrow(df), replace=TRUE, prob=c(0.7,0.3))

  
  train_set <- df[sample, ]
  test_set <- df[!sample, ]
  
  var <- expr(target)
  
  Xtrain <- train_set |> dplyr::select(-any_of(target)) |> makeX()
  X_test <- test_set |> dplyr::select(-any_of(target))
  
  ytrain <- train_set[[target]]
  y_test <- test_set[[target]]
  
  # Fit a Lasso model with cross-validation
  fit <- cv.glmnet(Xtrain, ytrain, alpha = 0.35)  # alpha = 1 for Lasso, 0 for Ridge
  
  feature_importance <- as.data.frame(
    as.matrix(coef(fit, fit$lambda.min)
    ))
  
  print(feature_importance)
  feats <- feature_importance |> filter(s1 > 0) |> rownames()
  
  
  pruned_data <- Xtrain |> as.data.frame() |> dplyr::select(all_of(feats))
  pruned_data[[target]] <- ytrain

  f <- as.formula(paste(target, " ~ .", collapse=" "))

  full.model <- lm(f, data = pruned_data)
  print(summary(full.model))

  # Stepwise regression model
  step.model <- stepAIC(full.model, direction = "both", trace = T)

  summary(step.model) |> print()

  return(list(model=step.model, test=X_test, obs=y_test))
}

plot_res <- function(pruny, title){
  
  fitted <- predict(object = pruny$model, newdata = pruny$test)
  
  res <- data.frame(Fitted = fitted, Observed = pruny$obs) |>
    mutate(
      Res = (-Fitted + Observed),
      AbsError = abs(Res/Observed),
      SqError = Res ^2
      )

  mse <- mean(res$SqError) |> round(2)
  
  mape <- mean(res$AbsError) |> round(2) * 100
  
  rsq <- summary(pruny$model)$r.squared |> round(2)
  

  ggplot(res, aes(x=Fitted, y = Observed)) + geom_point(color="blue")+
    labs(title =  title, 
         subtitle = paste("MSE:", mse, "  MAPE:", mape, "% R sq:", rsq)) +
    ylim(0,55) +
    xlim(0,55)

  
}



