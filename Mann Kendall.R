# Mann Kendall

require(RobustLinearReg)
require(trend)



MK_data$Well <- factor(MK_data$Well)
MK_data$Date <- as.Date(MK_data$Date)
data <- MK_data %>% filter(!is.na(MK_data$`LANPL Thickness`))

DM <- data %>% filter(Well == "DM") 
mkDM <- mk.test(DM$`LANPL Thickness`)

DM$lnapl <- DM$`LANPL Thickness`

ggplot(DM, aes(Date,  lnapl)) + geom_point()

sen <- function(..., weights = NULL) {
  mblm::mblm(...)
}

ggplot(DM, aes(Date,  `LANPL Thickness`)) + geom_point() + geom_line() +
  geom_smooth(method = sen)

theil_sen_regression(lnapl ~ Date, DM)

t34 <- otfdata %>% filter(Well == "t34") 
mk34 <- mk.test(t34$`LANPL Thickness`)
t34$number <- seq(1, nrow(t34), 1)
theil_sen_regression(`LANPL Thickness` ~ number, t34)


shapiro.test(t34$`LANPL Thickness`)

wilcox.test(t34$`LANPL Thickness`, mu = 0.85, exact = FALSE)

#############################################################

mw15fgta <- fgta_clean %>% filter(MW == "MW-15") 
mk.test(mw15fgta$dro)

mw15fgta$d_benzene <- factor(mw15fgta$d_benzene)
mw15fgta$d_dro <- str_replace(mw15fgta$d_dro, "0", "No")
mw15fgta$d_dro <- str_replace(mw15fgta$d_dro, "1", "Yes")

ggplot(mw15fgta, aes(sample_date,  dro)) + geom_point(aes(shape = d_dro)) + geom_line() +
  geom_smooth(method = sen, aes(color = "Theil-Sen Trend Line")) +
  scale_color_manual(name = element_blank(), values= "blue") +
  ggtitle("Mann-Kendall Trend Test") + 
  labs(x = "Date", y = "DRO (mg/L)", shape = "Detection") +
  theme(legend.position = "bottom", plot.title = element_text(size = 13, hjust = 0.5))
