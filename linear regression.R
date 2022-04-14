
view(stealdf)

plot(na.interpolation(tsAirgap, option = "linear") - AirPassengers, ylim = c(-mean(AirPassengers), mean(AirPassengers)), ylab = "Difference", main = "Linear")
m3 <- mean((na.interpolation(tsAirgap, option = "linear") - AirPassengers)^2)

# Remove percentage sign in the data set and convert to numeric
stealdf$X <- data.frame(sapply(stealdf[,7], function(x) interval(as.Date('1Jul2015', "%d%b%Y") , x) %/% months(1)))

# Drop columns used to calculate N_SC and N_SU columns
stealdf <- stealdf[ , !(names(stealdf) %in% c("N_BC", "N_BU", "N_RC", "N_RU", "N_THC", "N_THU", "date"))]
colnames <- c("N_SC", "N_SU", "Month" )

# Change column names of full_df
colnames(stealdf) <- colnames
view(stealdf)
