library(imputeTS)
library(zoo)
library(Hmisc) 
library(corrplot)
library(lattice)

# Select subset of national values with 7 columns
stealdf = nationaldf[, c('N_BC', 'N_BU', 'N_RC','N_RU','N_THC','N_THU','date','court')]
head(nationaldf)
hist(stealdf$"N_BU")
par(mfrow=c(3,2))
# Check for outliers
ggplot(nationaldf, aes(N_THC ))+geom_boxplot()+
  ggtitle("Number of national theft and handling convictions")+
  xlab("Number of theft and handling convictions")

ggplot(nationaldf, aes(N_THU ))+geom_boxplot()+
  ggtitle("Number of national theft and handling unsuccessfull")+
  xlab("Number of theft and handling unsuccessfull")

ggplot(nationaldf, aes(N_RC ))+geom_boxplot()+
  ggtitle("Number of national robbery convictions")+
  xlab("Number of robbery convictions")

ggplot(nationaldf, aes(N_RU ))+geom_boxplot()+
  ggtitle("Number of robbery unsuccessfull")+
  xlab("Number of robbery unsuccessfull")

ggplot(nationaldf, aes(N_BC ))+geom_boxplot()+
  ggtitle("Number of national burglary convictions")+
  xlab("Number of burglary convictions")

ggplot(nationaldf, aes(N_BU ))+geom_boxplot()+
  ggtitle("Number of national burglary unsuccessfull")+
  xlab("Number of burglary unsuccessfull")

is_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}
outlier_nthc <- is_outlier(nationaldf$N_THC)
aggregate(data.frame(count = outlier_nthc), list(value = outlier_nthc), length)
outlier_nthu <- is_outlier(nationaldf$N_THU)
aggregate(data.frame(count = outlier_nthu), list(value = outlier_nthu), length)
outlier_nbc <- is_outlier(nationaldf$N_BC)
aggregate(data.frame(count = outlier_nbc), list(value = outlier_nbc), length)
outlier_nbu <- is_outlier(nationaldf$N_BU)
aggregate(data.frame(count = outlier_nbu), list(value = outlier_nbu), length)
outlier_nrc <- is_outlier(nationaldf$N_RC)
aggregate(data.frame(count = outlier_nrc), list(value = outlier_nrc), length)
outlier_nru <- is_outlier(nationaldf$N_RU)
aggregate(data.frame(count = outlier_nru), list(value = outlier_nru), length)

# Calculate coorelation
cor(nationaldf$N_THC,as.numeric(nationaldf$date), method = c("pearson"))
cor(nationaldf$N_THU,as.numeric(nationaldf$date), method = c("pearson"))
cor(nationaldf$N_RC,as.numeric(nationaldf$date), method = c("pearson"))
cor(nationaldf$N_RU ,as.numeric(nationaldf$date), method = c("pearson"))
cor(nationaldf$N_BC ,as.numeric(nationaldf$date), method = c("pearson"))
cor(nationaldf$N_BU,as.numeric(nationaldf$date), method = c("pearson"))

cor(nationaldf$N_THC,as.numeric(nationaldf$date), method = c("spearman"))
cor(nationaldf$N_THU,as.numeric(nationaldf$date), method = c("spearman"))
cor(nationaldf$N_RC,as.numeric(nationaldf$date), method = c("spearman"))
cor(nationaldf$N_RU ,as.numeric(nationaldf$date), method = c("spearman"))
cor(nationaldf$N_BC ,as.numeric(nationaldf$date), method = c("spearman"))
cor(nationaldf$N_BU,as.numeric(nationaldf$date), method = c("spearman"))

# Add column for month
stealdf <- cbind(stealdf, "no_months" = sapply(stealdf[,7], function(x) interval(as.Date('1Jul2015', "%d%b%Y") , x) %/% months(1)))
no_months  <- data.frame ("no_months" = 1:32)
stealdf <- merge(no_months,stealdf, by = "no_months", all = TRUE)
stealdf$N_BC <- na_interpolation(stealdf$N_BC,option = "linear") 
stealdf$N_BU <- na_interpolation(stealdf$N_BU,option = "linear") 
stealdf$N_RC <- na_interpolation(stealdf$N_RC,option = "linear") 
stealdf$N_RU <- na_interpolation(stealdf$N_RU,option = "linear") 
stealdf$N_THC <- na_interpolation(stealdf$N_THC,option = "linear") 
stealdf$N_THU <- na_interpolation(stealdf$N_THU,option = "linear") 
stealdf$date <- as.Date(na.approx(stealdf$date))


sc.stealdf <- stealdf
# Total number of court cases
stealdf$total_theft_handling <- stealdf$N_THC + stealdf$N_THU
stealdf$total_burglary <- stealdf$N_BC + stealdf$N_BU
stealdf$total_robbery <- stealdf$N_RC + stealdf$N_RU
Total_national_robbery_cases = stealdf$total_robbery
Total_national_theft_handling_cases = stealdf$total_theft_handling
Total_national_burglary_cases = stealdf$total_burglary

hist(Total_national_theft_handling_cases)
hist(Total_national_burglary_cases)
hist(Total_national_robbery_cases)
summary(stealdf)
sd(stealdf$total_theft_handling)

# Scale the column except date
sc.stealdf[c(2:7)] <- scale(sc.stealdf[c(2:7)])
head(stealdf)


# Combining theft and handling and burglary
th.c_df <- sc.stealdf[,c('no_months','N_BC','N_BU', 'N_THC','N_THU')]
# Creating a new column for conviction values
th.c_df$N_SC <- th.c_df$N_BC + th.c_df$N_THC
# Creating a new column for unsuccessful values
th.c_df$N_SU <- th.c_df$N_BU + th.c_df$N_THU

# Creating a new column for conviction values
sc.stealdf$N_SC <- sc.stealdf$N_BC + sc.stealdf$N_RC + sc.stealdf$N_THC

# Creating a new column for unsuccessful values
sc.stealdf$N_SU <- sc.stealdf$N_BU + sc.stealdf$N_RU + sc.stealdf$N_THU



# Prepare the matrix for Bar plot selecting data frame with only N_SC and N_SU columns
# Convert it to a matrix and transpose the matrix
mx <- t(as.matrix(stealdf[,c('N_SC','N_SU')]))

# Date column read as character
colnames(mx) <- as.character(stealdf$date)

colours = c("lightblue","orange")
# Use ylim to give space for the legend
barplot(mx,main= "National stealing court cases from July 2015 to March 2018",ylab='Scaled number of cases', xlab="Month",beside = TRUE, 
        col=colours, ylim=c(min(mx)*1.2,max(mx)*1.2))
# to add a box around the plot
box()

# add a legend
legend("topright",fill=colours,legend=c('Scaled Stealing Convictions','Scaled Stealing Unsuccessful'))

# Scater plot matrix Figure 4.3
splom(~stealdf[c(1,9,10,11)], groups=NULL, data=stealdf,axis.line.tck = 0, axis.text.alpha = 0) 
