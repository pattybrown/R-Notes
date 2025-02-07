# Pivot GW Data

haha <- copy_Fmt_ProUCL_HAHA_042021_042023_Recent_Data
haha <- copy_Fmt_ProUCL_HAHA_042021_042023_Recent_Data[,c(1,2,3,5,7,9,11,13,15,17,19,21,23,25,27,29,31,33,35,37,39,41,43)]

hahapivot <- pivot_longer(data = haha,
                         # the columns we want to pivot
                         cols = c(`Antimony`:`Total Dissolved Solids`),
                         # name of column we create for pivoted col names above
                         names_to = "Analytes",
                         # name of column we create for pivoted values
                         values_to = "Analyte Values")

hahapivot2 <- hahapivot %>% filter(!is.na('Sample Date'))
hahapivot$`Analyte Values` <- as.numeric(hahapivot$`Analyte Values`)
write.csv(hahapivot, "hahapivot.csv")