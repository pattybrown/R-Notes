
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



#######################
# Random Forests Models
#######################




