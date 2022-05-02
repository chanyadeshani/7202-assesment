library(dplyr)
library(broom)
library(ggpubr)

head(stealdf)
# code used 
# Regression line for total theft and handling cases
th_reg <- lm(total_theft_handling ~ no_months, data = stealdf)
summary(th_reg)
sd(stealdf$total_theft_handling)

# Regression line for total burglary cases 
b_reg <- lm(total_burglary ~ no_months, data = stealdf)
summary(b_reg)
sd(stealdf$total_burglary)
# Regression line for total robbery cases
r_reg <- lm(total_robbery ~ no_months, data = stealdf)
summary(r_reg)
sd(stealdf$total_robbery)

ggplot(stealdf, aes(total_burglary ))+geom_boxplot()+
  ggtitle("Number of theft and handling convictions in June 2015")+
  xlab("Number of theft and handling convictions")

#--------------------------
  #Todo
#Draw regression line

## Convert court variable to numeric to improve reggression line
#timeseries_df$court_num <- as.numeric(as.factor(timeseries_df$court))

