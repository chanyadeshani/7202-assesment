library(dplyr)
library(broom)
library(ggpubr)

head(stealdf)
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

# Regression line for total theft and handling cases
th_reg <- lm(total_theft_handling ~ no_months, data = stealdf)
summary(th_reg)
# Regression line for total burglary cases 
b_reg <- lm(total_burglary ~ no_months, data = stealdf)
summary(b_reg)
# Regression line for total robbery cases
r_reg <- lm(total_robbery ~ no_months, data = stealdf)
summary(r_reg)


####
head(th.c_df)
simple_reg <- lm(N_SC ~ no_months, data = th.c_df)
summary(simple_reg)

head(sc.stealdf)
simple_reg <- lm(N_THC ~ no_months, data = sc.stealdf)
summary(simple_reg)
simple_reg <- lm(N_THU ~ no_months, data = sc.stealdf)
summary(simple_reg)

