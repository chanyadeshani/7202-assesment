library(dplyr)
library(broom)
library(ggpubr)

head(stealdf)
# code used 
# Regression line for total theft and handling cases
th_reg <- lm(total_theft_handling ~ no_months, data = stealdf)
summary(th_reg)
# Regression line for total burglary cases 
b_reg <- lm(total_burglary ~ no_months, data = stealdf)
summary(b_reg)
# Regression line for total robbery cases
r_reg <- lm(total_robbery ~ no_months, data = stealdf)
summary(r_reg)


# not used
# Regression line for theft and handling conviction
simple_reg <- lm(N_THC ~ no_months, data = stealdf)
summary(simple_reg)
# Regression line for theft and handling unsuccessful
simple_reg <- lm(N_THU ~ no_months, data = stealdf)
summary(simple_reg)
# Regression line for burglary convictions
simple_reg <- lm(N_BC ~ no_months, data = stealdf)
summary(simple_reg)
# Regression line for burglary unsuccessful
simple_reg <- lm(N_BU ~ no_months, data = stealdf)
summary(simple_reg)
# Regression line for robbery convictions
simple_reg <- lm(N_RC ~ no_months, data = stealdf)
summary(simple_reg)
# Regression line for robbery unsuccessful
simple_reg <- lm(N_RU ~ no_months, data = stealdf)
summary(simple_reg)



ggplot(stealdf, aes(total_burglary ))+geom_boxplot()+
  ggtitle("Number of theft and handling convictions in June 2015")+
  xlab("Number of theft and handling convictions")

####
head(th.c_df)
simple_reg <- lm(N_SC ~ no_months, data = th.c_df)
summary(simple_reg)

head(sc.stealdf)
simple_reg <- lm(N_THC ~ no_months, data = sc.stealdf)
summary(simple_reg)
simple_reg <- lm(N_THU ~ no_months, data = sc.stealdf)
summary(simple_reg)

hist(r_reg$residuals, breaks = fitted)
--------------------------
  #Todo
fitd <- !(aggdat$start_speed ~ aggdat$game_num)

my.count <- seq(from=1, to=101, by=1)

predd <- predict(fitd, my.count, se=TRUE) plot(aggdat$start_speed ~ aggdat$game_num, pch=16, ylab=”Speed out of Hand (Fastballs, MPH)”, xlab=”Pitch Count”, main=”Pitch Speed by Pitch Count”)

lines(predd$fit, lty=”solid”, col=”darkred”, lwd=3)
