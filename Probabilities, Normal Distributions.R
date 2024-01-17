### Probabilities, Normal Distributions


# Probability of making a deal < 7500$ 
pnorm(7500, mean = 5000, sd = 2000)

# Probability of deal > 1000
pnorm(1000, mean = 5000, sd = 2000, lower.tail = FALSE)

# Probability of deal between 3000 and 7000
pnorm(7000, mean = 5000, sd = 2000) - pnorm(3000, mean = 5000, sd = 2000)

# Calculate amount that 75% of deals will be more than
qnorm(0.75, mean = 5000, sd = 2000, lower.tail = FALSE)

#########################################################

# Calculate new average amount if sales increase by 20% from 5000
new_mean <- 5000 * 1.2

# Calculate new standard deviation if it increases by 30%
new_sd <- 2000 * 1.3

# Simulate 36 sales
new_sales <- new_sales %>% 
  mutate(amount = rnorm(36, mean = new_mean, sd = new_sd))

# Create histogram with 10 bins
ggplot(new_sales, aes(amount)) + geom_hist(bins = 10)

# Calculate percent of sales over 1000$
pnorm(1000, mean = 5000, sd = 2000, lower.tail = FALSE)

###############################################################

# SAMPLES

# Sample 20 num_users with replacement from amir_deals
sample(amir_deals$num_users, size = 20, replace = TRUE) %>%
# Take mean
  mean()
  
# Repeat the above 100 times
sample_means <- replicate(100, sample(amir_deals$num_users, size = 20, replace = TRUE)
  %>% mean())

##################################################################

# POISSON DISTRIBUTIONS

# Probability of something happening AT LEAST 5 times in a day when the mean is 4
dpois(5, 4)

# Probablity of something happening AT MOST 2 times in a day when mean is 4
ppois(2, 4)

# Probability of > 10 responses
ppois(10, 4, lower.tail = FALSE)



# EXPONENTIAL DISTRIBUTIONS

# Probability something takes < 1hr when mean time is 2.5 hrs
pexp(1, rate = 1/2.5)

# Probability it takes > 4 hours
pexp(4, rate = 1/2.5, lower.tail = FALSE)


