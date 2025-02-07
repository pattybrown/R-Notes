
### Dealing with dates from Excel into R

### FIRST: 

#In Excel, select the entirety of your timestamp column -> right-click and select 'Format Cells'

#Select 'Custom' in the 'Category' panel (last item in the list)

#In 'Type', set the format to this: yyyy-mm-ddThh:mm:ss.000 (feel free to skip the .000 if your timestamp isn't at that level of precision). Note that there's a 'T' between the date and time components - this is crucial; adding this basically stops Excel from trying to 'help' and messing things up in the process
                                                            
#Save the Excel sheet as a CSV and read it into R - your timestamp will look the way it's supposed to

### IMPORT:

menzies_data <- read_csv("PROJECTS/Menzies/menzies data.csv")
menzies_data

menzies_data$sample_date <- anydate(menzies_data$sample_date)

