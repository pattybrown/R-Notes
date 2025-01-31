## West Pad Data Analysis

# wpad = clean 2024 petroloeum hydrocarbon dataset
# precip = precip and thawing days data
wpad <- westpad_tph_clean

wpad2024 <- wpad %>% filter(!is.na(sample_date))
wpad2024$year <- year(wpad2024$sample_date)

precip2024 <- precip

alldata <- wpad2024 %>% filter(MW %in% c("MW-12", "MW-13", "MW-14", "B-MW-6")) %>% 
  select(gasoline_range_organics, diesel_range_organics, year, tph,  MW)


alldata <- alldata %>% group_by(MW, year) %>% summarise(gasoline_range_organics = max(gasoline_range_organics),
                                          diesel_range_organics = max(diesel_range_organics), 
                                          tph = max(tph))
write_xlsx(alldata, "WP_Cor_Data.xlsx")

by <- join_by(year == year)

means <- aggregate(alldata, by = list(Group.date = alldata$year), FUN = mean)

## GRO ##
#############################################################################################

mw12 <- wpad2024 %>% filter(MW == "MW-12")
mw12_gro <- wpad2024 %>% filter(MW == "MW-12") %>% select(gasoline_range_organics, year, MW)

mw13 <- wpad2024 %>% filter(MW == "MW-13")
mw13_gro <- wpad2024 %>% filter(MW == "MW-13") %>% select(gasoline_range_organics, year, MW)

bmw6 <- wpad2024 %>% filter(MW == "B-MW-6")
bmw6_gro <- wpad2024 %>% filter(MW == "B-MW-6") %>% select(gasoline_range_organics, year, MW)

mw14 <- wpad2024 %>% filter(MW == "MW-14")
mw14_gro <- wpad2024 %>% filter(MW == "MW-14") %>% select(gasoline_range_organics, year, MW)




### GRO Current Year

mw12_gro_correlations <- full_join(mw12_gro, precip, by) %>% arrange(year)

mw13_gro_correlations <- full_join(mw13_gro, precip, by) %>% arrange(year)

mw14_gro_correlations <- full_join(mw14_gro, precip, by) %>% arrange(year)

bmw6_gro_correlations <- full_join(bmw6_gro, precip, by) %>% arrange(year)

mw12_cor_matrix_gro <- cor(mw12_gro_correlations[, c("gasoline_range_organics", "total_precipitation", "thawing_degrees_days")], use = "complete.obs", method = "spearman")
mw13_cor_matrix_gro <- cor(mw13_gro_correlations[, c("gasoline_range_organics", "total_precipitation", "thawing_degrees_days")], use = "complete.obs", method = "spearman")
mw14_cor_matrix_gro <- cor(mw14_gro_correlations[, c("gasoline_range_organics", "total_precipitation", "thawing_degrees_days")], use = "complete.obs", method = "spearman")
bmw6_cor_matrix_gro <- cor(bmw6_gro_correlations[, c("gasoline_range_organics", "total_precipitation", "thawing_degrees_days")], use = "complete.obs", method = "spearman")

cor.test(mw12_gro_correlations$gasoline_range_organics, mw12_gro_correlations$thawing_degrees_days)
cor.test(mw12_gro_correlations$gasoline_range_organics, mw12_gro_correlations$total_precipitation)
cor.test(mw12_gro_correlations$total_precipitation, mw12_gro_correlations$thawing_degrees_days)

cor.test(mw13_gro_correlations$gasoline_range_organics, mw13_gro_correlations$thawing_degrees_days)
cor.test(mw13_gro_correlations$gasoline_range_organics, mw13_gro_correlations$total_precipitation)
cor.test(mw13_gro_correlations$total_precipitation, mw13_gro_correlations$thawing_degrees_days)

cor.test(mw14_gro_correlations$gasoline_range_organics, mw14_gro_correlations$thawing_degrees_days)
cor.test(mw14_gro_correlations$gasoline_range_organics, mw14_gro_correlations$total_precipitation)
cor.test(mw14_gro_correlations$total_precipitation, mw14_gro_correlations$thawing_degrees_days)

cor.test(bmw6_gro_correlations$gasoline_range_organics, bmw6_gro_correlations$thawing_degrees_days)
cor.test(bmw6_gro_correlations$gasoline_range_organics, bmw6_gro_correlations$total_precipitation)
cor.test(bmw6_gro_correlations$total_precipitation, bmw6_gro_correlations$thawing_degrees_days)


### No correlation at MW-12 - No p-values greater than 0.05. 

## DRO ##
######################################################################################################################
mw12_dro <- wpad2024 %>% filter(MW == "MW-12") %>% select(diesel_range_organics, year, MW)

mw13_dro <- wpad2024 %>% filter(MW == "MW-13") %>% select(diesel_range_organics, year, MW)

bmw6_dro <- wpad2024 %>% filter(MW == "B-MW-6") %>% select(diesel_range_organics, year, MW)

mw14_dro <- wpad2024 %>% filter(MW == "MW-14") %>% select(diesel_range_organics, year, MW)

mw12_dro_correlations <- full_join(mw12_dro, precip, by) %>% arrange(year)

mw13_dro_correlations <- full_join(mw13_dro, precip, by) %>% arrange(year)

mw14_dro_correlations <- full_join(mw14_dro, precip, by) %>% arrange(year)

bmw6_dro_correlations <- full_join(bmw6_dro, precip, by) %>% arrange(year)

mw12_cor_matrix_dro <- cor(mw12_dro_correlations[, c("diesel_range_organics", "total_precipitation", "thawing_degrees_days")], use = "complete.obs", method = "spearman")
mw13_cor_matrix_dro <- cor(mw13_dro_correlations[, c("diesel_range_organics", "total_precipitation", "thawing_degrees_days")], use = "complete.obs", method = "spearman")
mw14_cor_matrix_dro <- cor(mw14_dro_correlations[, c("diesel_range_organics", "total_precipitation", "thawing_degrees_days")], use = "complete.obs", method = "spearman")
bmw6_cor_matrix_dro <- cor(bmw6_dro_correlations[, c("diesel_range_organics", "total_precipitation", "thawing_degrees_days")], use = "complete.obs", method = "spearman")

cor.test(mw12_dro_correlations$diesel_range_organics, mw12_dro_correlations$thawing_degrees_days)
cor.test(mw12_dro_correlations$diesel_range_organics, mw12_dro_correlations$total_precipitation)
cor.test(mw12_dro_correlations$total_precipitation, mw12_dro_correlations$thawing_degrees_days)

cor.test(mw13_dro_correlations$diesel_range_organics, mw13_dro_correlations$thawing_degrees_days)
cor.test(mw13_dro_correlations$diesel_range_organics, mw13_dro_correlations$total_precipitation)
cor.test(mw13_dro_correlations$total_precipitation, mw13_dro_correlations$thawing_degrees_days)

cor.test(mw14_dro_correlations$diesel_range_organics, mw14_dro_correlations$thawing_degrees_days)
cor.test(mw14_dro_correlations$diesel_range_organics, mw14_dro_correlations$total_precipitation)
cor.test(mw14_dro_correlations$total_precipitation, mw14_dro_correlations$thawing_degrees_days)

cor.test(bmw6_dro_correlations$diesel_range_organics, bmw6_dro_correlations$thawing_degrees_days)
cor.test(bmw6_dro_correlations$diesel_range_organics, bmw6_dro_correlations$total_precipitation)
cor.test(bmw6_dro_correlations$total_precipitation, bmw6_dro_correlations$thawing_degrees_days)

############################################################################################################


mw12gro_nextyear <- mw12_gro_correlations %>% arrange(year) %>% mutate(nextyear_gro = lead(gasoline_range_organics, 
                                                                                           n = 1))
mw13gro_nextyear <- mw13_gro_correlations %>% arrange(year) %>% mutate(nextyear_gro = lead(gasoline_range_organics, 
                                                                                           n = 1))
mw14gro_nextyear <- mw14_gro_correlations %>% arrange(year) %>% mutate(nextyear_gro = lead(gasoline_range_organics, 
                                                                                           n = 1))
bmw6gro_nextyear <- bmw6_gro_correlations %>% arrange(year) %>% mutate(nextyear_gro = lead(gasoline_range_organics, 
                                                                                           n = 1))
mw12_nextyear_matrix_gro <- cor(mw12gro_nextyear[, c("gasoline_range_organics", "total_precipitation", "thawing_degrees_days")], use = "complete.obs", method = "spearman")
mw13_nextyear_matrix_gro <- cor(mw13gro_nextyear[, c("gasoline_range_organics", "total_precipitation", "thawing_degrees_days")], use = "complete.obs", method = "spearman")
mw14_nextyear_matrix_gro <- cor(mw14gro_nextyear[, c("gasoline_range_organics", "total_precipitation", "thawing_degrees_days")], use = "complete.obs", method = "spearman")
bmw6_nextyear_matrix_gro <- cor(bmw6gro_nextyear[, c("gasoline_range_organics", "total_precipitation", "thawing_degrees_days")], use = "complete.obs", method = "spearman")


cor.test(mw12gro_nextyear$gasoline_range_organics, mw12gro_nextyear$thawing_degrees_days)
cor.test(mw12gro_nextyear$gasoline_range_organics, mw12gro_nextyear$total_precipitation)
cor.test(mw12gro_nextyear$total_precipitation, mw12gro_nextyear$thawing_degrees_days)

cor.test(mw13gro_nextyear$gasoline_range_organics, mw13gro_nextyear$thawing_degrees_days)
cor.test(mw13gro_nextyear$gasoline_range_organics, mw13gro_nextyear$total_precipitation)
cor.test(mw13gro_nextyear$total_precipitation, mw13gro_nextyear$thawing_degrees_days)

cor.test(mw14gro_nextyear$gasoline_range_organics, mw14gro_nextyear$thawing_degrees_days)
cor.test(mw14gro_nextyear$gasoline_range_organics, mw14gro_nextyear$total_precipitation)
cor.test(mw14gro_nextyear$total_precipitation, mw14gro_nextyear$thawing_degrees_days)

cor.test(bmw6gro_nextyear$gasoline_range_organics, bmw6gro_nextyear$thawing_degrees_days)
cor.test(bmw6gro_nextyear$gasoline_range_organics, bmw6gro_nextyear$total_precipitation)
cor.test(bmw6gro_nextyear$total_precipitation, bmw6gro_nextyear$thawing_degrees_days)

#### DRO

mw12dro_nextyear <- mw12_dro_correlations %>% arrange(year) %>% mutate(nextyear_dro = lead(diesel_range_organics, 
                                                                                           n = 1))
mw13dro_nextyear <- mw13_dro_correlations %>% arrange(year) %>% mutate(nextyear_dro = lead(diesel_range_organics, 
                                                                                           n = 1))
mw14dro_nextyear <- mw14_dro_correlations %>% arrange(year) %>% mutate(nextyear_dro = lead(diesel_range_organics, 
                                                                                           n = 1))
bmw6dro_nextyear <- bmw6_dro_correlations %>% arrange(year) %>% mutate(nextyear_dro = lead(diesel_range_organics, 
                                                                                           n = 1))

mw12_nextyear_matrix_dro <- cor(mw12dro_nextyear[, c("diesel_range_organics", "total_precipitation", "thawing_degrees_days")], use = "complete.obs", method = "spearman")
mw13_nextyear_matrix_dro <- cor(mw13dro_nextyear[, c("diesel_range_organics", "total_precipitation", "thawing_degrees_days")], use = "complete.obs", method = "spearman")
mw14_nextyear_matrix_dro <- cor(mw14dro_nextyear[, c("diesel_range_organics", "total_precipitation", "thawing_degrees_days")], use = "complete.obs", method = "spearman")
bmw6_nextyear_matrix_dro <- cor(bmw6dro_nextyear[, c("diesel_range_organics", "total_precipitation", "thawing_degrees_days")], use = "complete.obs", method = "spearman")


cor.test(mw12dro_nextyear$diesel_range_organics, mw12dro_nextyear$thawing_degrees_days)
cor.test(mw12dro_nextyear$diesel_range_organics, mw12dro_nextyear$total_precipitation)
cor.test(mw12dro_nextyear$total_precipitation, mw12dro_nextyear$thawing_degrees_days)

cor.test(mw13dro_nextyear$diesel_range_organics, mw13dro_nextyear$thawing_degrees_days)
cor.test(mw13dro_nextyear$diesel_range_organics, mw13dro_nextyear$total_precipitation)
cor.test(mw13dro_nextyear$total_precipitation, mw13dro_nextyear$thawing_degrees_days)

cor.test(mw14dro_nextyear$diesel_range_organics, mw14dro_nextyear$thawing_degrees_days)
cor.test(mw14dro_nextyear$diesel_range_organics, mw14dro_nextyear$total_precipitation)
cor.test(mw14dro_nextyear$total_precipitation, mw14dro_nextyear$thawing_degrees_days)

cor.test(bmw6dro_nextyear$diesel_range_organics, bmw6dro_nextyear$thawing_degrees_days)
cor.test(bmw6dro_nextyear$diesel_range_organics, bmw6dro_nextyear$total_precipitation)
cor.test(bmw6dro_nextyear$total_precipitation, bmw6dro_nextyear$thawing_degrees_days)

#############################################################################################################

means_correlations <- full_join(means, precip, by) %>% arrange(year)

means_cor_matrix_dro <- cor(means_correlations[, c("diesel_range_organics", "total_precipitation", "thawing_degrees_days")], use = "complete.obs", method = "spearman")
means_cor_matrix_gro <- cor(means_correlations[, c("gasoline_range_organics", "total_precipitation", "thawing_degrees_days")], use = "complete.obs", method = "spearman")



lag <- mw12_dro_correlations %>% ccf cor(mw12dro_nextyear[, c("diesel_range_organics", "total_precipitation", "thawing_degrees_days")], use = "complete.obs", method = "spearman")




ggplot(mw12, aes(year, gasoline_range_organics, color = "gro")) + geom_point() + geom_line() +
  geom_point(data = precip, aes(year, total_precipitation, color = "total_precipitation")) + 
  geom_line(data = precip, aes(year, total_precipitation, color = "total_precipitation")) +
  geom_point(data = precip, aes(year, thawing_degrees_days, color = "thawing degrees days")) +
  geom_line(data = precip, aes(year, thawing_degrees_days, color = "thawing degrees days")) +


  

  
