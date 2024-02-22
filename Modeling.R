
##### MODELING

require(moderndive)

#. Look
#. create visualizations
#. summary stats

# EXPLORATORY ANALYSIS:
# Generate tables of joint and conditional proportions, respectively:

tab <- table(comics$align, comics$gender)
options(scipen = 999, digits = 3) # Print fewer digits
prop.table(tab)     # Joint proportions
prop.table(tab, 2)  # Conditional on columns


# Create box plots of city mpg by ncyl
ggplot(common_cyl, aes(x = as.factor(ncyl), y = city_mpg)) +
  geom_boxplot()

# Create overlaid density plots for same data
ggplot(common_cyl, aes(x = city_mpg, fill = as.factor(ncyl))) +
  geom_density(alpha = .3)

# Add log10_size
house_prices_2 <- house_prices %>%
  mutate(log10_size = log10(sqft_living))

# Compute correlation
evals %>%
  summarize(correlation = cor(score, bty_avg))

#####################################################################

######## Linear Models (regression) #############

######################################################################

# Plot linear regression line
ggplot(evals, aes(x = bty_avg, y = score)) +
  geom_point() +
  labs(x = "beauty score", y = "score") +
  geom_smooth(method = "lm", se = FALSE)

# Fit model (univariate)
model_score_2 <- lm(score ~ bty_avg, data = evals)

### y_hat <- (predictor value * avg(row 2)) + y intercept(row 1)

# Output regression table - fitted values in the estimate column.
get_regression_table(model_score_2)

# Output all fitted/predicted values and residuals
get_regression_points(model_score_2)

#------------------------------
# A tibble: 2 × 7
#   term      estimate std_error statistic p_value lower_ci upper_ci
#  <chr>        <dbl>     <dbl>     <dbl>   <dbl>    <dbl>    <dbl>
# 1 intercept    3.88      0.076     51.0      0    3.73     4.03 
# 2 bty_avg      0.067     0.016     4.09      0    0.035    0.099
#------------------------------

# Add a new column score_hat_2 which replicates how score_hat is computed using
# the table's values.

get_regression_points(model_score_2) %>% 
  mutate(score_hat_2 = 3.88 + 0.067 * bty_avg)

# Add a new column residual_2 which replicates how residual is computed using 
# the table's values.

mutate(residual_2 = score - score_hat)

######  Models using categorical variables

# Fit regression model for categorical variable "rank"
model_score_4 <- lm(score ~ rank, data = evals)

# Plot residuals
ggplot(model_score_4_points, aes(x = residual)) +
  geom_histogram() +
  labs(x = "residuals", title = "Residuals from score ~ rank model")

#### SUM OF SQUARED RESIDUALS ###
# Automate prediction and residual computation
get_regression_points(model_price_2) %>%
  mutate(sq_residuals = residual^2) %>%
  summarize(sum_sq_residuals = sum(sq_residuals))

######################################################################

# Linear Regression in Caret

# Test Train Split
# Set seed
set.seed(22)

# Shuffle row indices: rows
rows <- sample(nrow(diamonds))

# Randomly order data
shuffled_diamonds <- diamonds[rows, ]

# Determine row to split on: split
split <- round(nrow(diamonds) * 0.80)

# Create train
train <- diamonds[1:split, ]

# Create test
test <- diamonds[(split + 1):nrow(diamonds), ]

# Fit lm model on train: model
model <- lm(price ~ ., train)

# Predict on test: p
p <- predict(model, test)

# Compute errors: error
error <- p - test[["price"]]

# Calculate RMSE
sqrt(mean(error^2))

# CROSS VALIDATION

set.seed(22)

model <- train(
  price ~.,
  diamonds,
  method = "lm",
  trControl = trainControl(
    method = "cv",
    number = 10,
    verboseIter = TRUE
  )
)

#####################################################################

### Multiple Regression ###

# Fit model as log10_price as a function of log10_size and bedrooms:
model_price_2 <- lm(log10_price ~ log10_size + bedrooms, 
                    data = house_prices)
# + indicates using multiple predictor variables.

# Fit multiple regression model
model_price_4 <- lm(log10_price ~ log10_size + waterfront,
                    data = house_prices)



### Parallel slopes model: 
# group by categogry prior to fitting model (waterfront in sample above)
# should have same slope but various intercepts

####################### MODEL ASSESSMENT AND SELECTION #################
########################################################################


# Asessing model with R-Squared: 
# Closer to 1, the better fit of the model. 

# Get fitted/values & residuals, compute R^2 using residuals
get_regression_points(model_price_2) %>%
  summarize(r_squared = 1 - var(residual) / var(log10_price)) #log10price is y value

###### RMSE ######
# MSE - substitute mean for sum in same sum squared error equation
# RMSE - Take square root of MSE - units match units of outcome variable y

# Get all residuals, square them, take the mean and square root               
get_regression_points(model_price_2) %>%
  mutate(sq_residuals = residual^2) %>%
  summarize(mse = mean(sq_residuals)) %>%
  mutate(rmse = sqrt(mse))



##############################################################################

# CLASSIFICATION MODELS WITH CARET  

# Confustion Matrices: 

# If p exceeds threshold of 0.9, M else R: m_or_r
m_or_r <- ifelse(p > 0.9, "M", "R")

# Convert to factor: p_class
p_class <- factor(m_or_r, levels = levels(test[["Class"]]))

# Create confusion matrix
confusionMatrix(p_class, test[["Class"]])


# The ROC Curve: 
# Predict on test: p
p <- predict(model, test, type = "response")
# Make ROC curve
colAUC(p, test[["Class"]], plotROC = TRUE)


# Area under Curve: 
# Create trainControl object: myControl
myControl <- trainControl(
  method = "cv",
  number = 10,
  summaryFunction = twoClassSummary,
  classProbs = TRUE, # IMPORTANT!
  verboseIter = TRUE
)

#Now that you have a custom trainControl object, it's easy to fit caret models 
#that use AUC rather than accuracy to tune and evaluate the model. You can just pass your 
#custom trainControl object to the train() function via the trControl argument, e.g.:

# Train glm with custom trainControl: model
model <- train(
  Class ~ ., 
  Sonar, 
  method = "glm",
  trControl = myControl
)

# Print model to console
model

#############################################################################################

# Random Forests Models 
    # yield accurate, non-linear models

# Fit random forest: model
model <- train(
  quality ~ .,
  tuneLength = 1,
  data = wine, 
  method = "ranger",
  trControl = trainControl(
    method = "cv", 
    number = 5, 
    verboseIter = TRUE
  )
)

# Print model to console
model

#suppose that a tree has a total of 10 splits and mtry = 2. This means that there are 10 
#samples of 2 predictors each time a split is evaluated.Use a larger tuning grid this time, but 
#stick to the defaults provided by the train() function. Try a tuneLength of 3, rather than 1, 
#to explore some more potential models, and plot the resulting model using the plot function.

# Fit random forest: model
model <- train(
  quality ~ .,
  tuneLength = 3,
  data = wine, 
  method = "ranger",
  trControl = trainControl(
    method = "cv", 
    number = 5, 
    verboseIter = TRUE
  )
)

# Print model to console
model

# Plot model
plot(model)

################################################################################################

#### glmnet models ##############

#Classification problems are a little more complicated than regression problems because you have to provide a 
#custom summaryFunction to the train() function to use the AUC metric to rank your models. Start by making a custom 
#trainControl, as you did in the previous chapter. Be sure to set classProbs = TRUE, otherwise the twoClassSummary 
#for summaryFunction will break.

# Create custom trainControl: myControl
myControl <- trainControl(
  method = "cv", 
  number = 10,
  summaryFunction = twoClassSummary,
  classProbs = TRUE, # IMPORTANT!
  verboseIter = TRUE
)

# Fit glmnet model: model
model <- train(
  y ~ ., 
  overfit,
  method = "glmnet",
  trControl = myControl
)

# Print model to console
model

# Print maximum ROC statistic
max(model[["results"]][["ROC"]])

# Train glmnet with custom trainControl and tuning: model
model <- train(
  y ~ ., 
  overfit,
  tuneGrid = expand.grid(
    alpha = 0:1,
    lambda = seq(0.0001, 1, length = 20)
  ),
  method = "glmnet",
  trControl = myControl
)

# Print model to console
model

# Print maximum ROC statistic
max(model[["results"]][["ROC"]])



###############################################################################
# Meadian and KNN Imputation

# Apply median imputation: median_model
median_model <- train(
  x = breast_cancer_x, 
  y = breast_cancer_y,
  method = "glm",
  trControl = myControl,
  preProcess = "medianImpute"
)

# Apply KNN imputation: knn_model
knn_model <- train(
  x = breast_cancer_x, 
  y = breast_cancer_y,
  method = "glm",
  trControl = myControl,
  preProcess = "knnImpute"
)

# Print knn_model to console
knn_model

# Combine multiple preprocessing methods to improve model performance:

# Update model with standardization
model <- train(
  x = breast_cancer_x, 
  y = breast_cancer_y,
  method = "glm",
  trControl = myControl,
  preProcess = c("center", "scale", "medianImpute", "pca")
)

# check rmse 
min(model$results$RMSE)


#################################### Comparing Models ####################

# Create custom indices: myFolds
myFolds <- createFolds(churn_y, k = 5)

# Create reusable trainControl object: myControl
myControl <- trainControl(
  summaryFunction = twoClassSummary,
  classProbs = TRUE, # IMPORTANT!
  verboseIter = TRUE,
  savePredictions = TRUE,
  index = myFolds
)


# Create model_list
model_list <- list(item1 = model_glmnet, item2 = model_rf)

# Pass model_list to resamples(): resamples
resamples <- resamples(model_list)

# Summarize the results
summary(resamples)
