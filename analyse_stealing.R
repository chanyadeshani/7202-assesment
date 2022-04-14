library(imputeTS)
library(zoo)

# Select subset of national values with 7 columns
stealdf = nationaldf[, c('N_BC', 'N_BU', 'N_RC','N_RU','N_THC','N_THU','date')]

# Add column for month
#stealdf$no_months <- "no_months" = sapply(stealdf[,7], function(x) interval(as.Date('1Jul2015', "%d%b%Y") , x) %/% months(1))
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

# Scale the column except date
stealdf[c(2:7)] <- scale(stealdf[c(2:7)])
head(stealdf)

# Creating a new column for conviction values
stealdf$N_SC <- stealdf$N_BC + stealdf$N_RC + stealdf$N_THC

# Creating a new column for unsuccessful values
stealdf$N_SU <- stealdf$N_BU + stealdf$N_RU + stealdf$N_THU
head(stealdf)

# figure 1.2 
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