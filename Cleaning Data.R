### Cleaning Data

require(dplyr)
require(stringr)

##### Categorical Data: 

# Convert categorical variables in COL1 to lower case:
DATA %>% mutate(NEWCOL = str_to_lower(COL1)) #also works for str_to_upper

# Remove whitespace from beginning and end of a string: 
DATA %>% mutate(NEWCOL = str_trim(COL1))

# Collapsing categories - convert several categories from COL1 to one factor (other): 
CATEGORIES <- c("CAT1", "CAT2") 
DATA %>% mutate(NEWCOL = fct_collapse(COL1, other = CATEGORIES))


                
##### Text Data: 

# Detect a pattern in COL1:
str_detect(COL1, "pattern or character")

# Replace all strings with replacement string in COL1:
DATA %>% mutate(NEWCOL = str_replace_all(COL1, "string", "replacement"))

# Remove hyphens and spaces so COL1 contains only numbers: 
DATA$COL1 %>% str_remove_all("-") %>%
  str_remove_all(" ")

# Return length of each string in COL1:
str_length(COL1)

# Filter to find all strings from COL1 that do not have 5 characters:
DATA %>% filter(str_length(COL1) != 5)
# Or to keep only those with 5 strings:
DATA %>% filter(str_length(COL1) == 5)


# Comparing Strings:
require(fuzzyjoin)
# Use string distance to join CITY column in DATA to CITY column in CITIES to clean up typos:
stringdist_left_join(DATA, CITIES, by = "CITY", method = "dl")







##### Numeric Data: 


##### MISSING DATA

# Remove decimals
floor(x)


