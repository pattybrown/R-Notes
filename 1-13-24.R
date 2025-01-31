### Patrick Brown
### 1/13/2025

### Geosyntec Data Exercise


require(readxl)
require(lubridate)
require(dplyr)
require(tidyr)
require(janitor)
require(ggplot2)
require(writexl)
require(stringr)
require(readr)


##### Task 1: Generate time series plot of TCE at MW-9 #####


data <- Copy_of_pb11324 #import data

data <- clean_names(data) #clean column names

data$samp_date <- as.Date(data$samp_date) #classify date column

tce <- data %>% filter(parameter == "TCE") #filter for TCE results only

tcemw9 <- tce %>% filter(field_pt_name == "MW-9") #filter TCE only dataset for MW-9 results only

# Check for duplicates and NDs. None observed. 

# Plot TCE concentrations at MW-9:
F1 <- ggplot(tcemw9, aes(samp_date, value, color = "TCE Concentration")) +
  geom_point() +
  geom_line() +
  theme(legend.position = "right", plot.title = element_text(size = 16, hjust = 0.5),
        plot.margin = unit(c(.7,.7,.7,.7), "cm")) +
  labs(color = "TCE Concentration") +
  ggtitle("TCE at MW-9") + 
  ylab("TCE (ug/L)") +
  xlab("Date") +
  scale_x_date(date_breaks = "3 years", date_labels = "%Y", 
               limits = as.Date(c("2006-01-01", "2020-01-01"))) 
F1

##### Task 2: Generate table that shows location of maximum concentration ever detected for each chemical #####

pivot <- data %>% pivot_wider(names_from = parameter, values_from = value) #pivot data to create parameter columns 

max_data <- pivot %>% group_by(field_pt_name) %>% 
  summarize(across(c(14:91), ~  max(.x, na.rm = TRUE))) #create table, grouped by location, showing historical max TCE concentrations

write_xlsx(max_data, "Task 2.xlsx") #export table to excel

##### Task 3: Summarize entire TCE dataset. 

detcol <- function(x){q
  ifelse(grepl("ND|<", x), 0, 1)} #write function that will create binary column indicating detections

excol <- function(x){q
  ifelse(x > 5, 1, 0)}  #write function that will create binary column indicating exceedances of 5 ug/L

#remove duplicates, retaining highest result
tce <- tce %>% group_by(field_pt_name, samp_date) %>% filter(value == max(value))

tce$detection <- detcol(tce$parvq) #create detections column
tce$exceedance <- excol(tce$value) #create exceedances column

tce_summary <- tce %>% group_by(field_pt_name) %>% 
  summarize(mean_tce = mean(value), max_tce = max(value), min_tce = min(value), 
            n_detections = sum(detection), historical_exceedances = sum(exceedance)) #create table with summary statistics

tce_full <- tce %>% mutate(exceedance = excol(value)) #Full data table with column indicating exceedance concentrations. 

write_xlsx(tce_summary, "Task 3a.xlsx") #export table to excel
write_xlsx(tce_full, "Task 3b.xlsx") #export table to excel

#### Bonus Map ####

#create year and month columns in TCE dataset:
tce$year <- year(tce$samp_date)
tce$month <- month(tce$samp_date) #confirm all 2019 data was collected in March

#Filter to pull only March 2019 TCE data
tce_2019 <- tce %>% filter(year == "2019")

#write 2019 data to excel:
write_xlsx(tce_2019, "tce_2019.xlsx")

