### Dates ###

require(tidyverse)

# parse multiple date formats: 
parse_date_time(DATECOL, orders = c("%Y-%m-%d")) # add all possible date formats to orders arg.

# Calculate age - how much time has passed from origin date and today
as.numeric(as.Date("2023-10-13") %--% today(), "years") #can also use years, etc. 

# Parse formatted dates: 

ymd(x) #x = "2010 September 20th" 

dmy(y) #y = "02.01.2010" (2010-01-02)

mdy_hm(z) #z = "Sep, 12th 2010 14:00"\

# Create date vector: 

make_date(year = YEARCOL, month = MONTHCOL, day = DAYCOL)

# Extracting parts of a datetime: 

year(DATE)
month(DATE)
day(DATE)

# Round date to nearest 5 minutes:
round_date(DATECOL, unit = "5 minutes")

