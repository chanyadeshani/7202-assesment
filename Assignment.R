library(ggplot2) 
library(tidyverse)
library(lubridate)
library(data.table)
library(plotly)
library(reshape2)
library(ggpubr)

# Read data from csv to data frame
df1 <- read_csv('principal_offence_category_march_2017.csv', col_names = TRUE)
names(df1)

july_2015 <- read_csv('principal_offence_category_july_2015.csv', col_names = TRUE)
august_2015 <- read_csv('principal_offence_category_august_2015.csv', col_names = TRUE)
september_2015 <- read_csv('principal_offence_category_september_2015.csv', col_names = TRUE)
october_2015 <- read_csv('principal_offence_category_october_2015.csv', col_names = TRUE)
december_2015 <- read_csv('principal_offence_category_december_2015.csv', col_names = TRUE)
january_2016 <- read_csv('principal_offence_category_january_2016.csv', col_names = TRUE)
april_2016 <- read_csv('principal_offence_category_april_2016.csv', col_names = TRUE)
may_2016 <- read_csv('principal_offence_category_may_2016.csv', col_names = TRUE)
june_2016 <- read_csv('principal_offence_category_june_2016.csv', col_names = TRUE)
july_2016 <- read_csv('principal_offence_category_july_2016.csv', col_names = TRUE)
august_2016 <- read_csv('principal_offence_category_august_2016.csv', col_names = TRUE)
september_2016 <- read_csv('principal_offence_category_september_2016.csv', col_names = TRUE)
october_2016 <- read_csv('principal_offence_category_october_2016.csv', col_names = TRUE)
november_2016 <- read_csv('principal_offence_category_november_2016.csv', col_names = TRUE)
december_2016 <- read_csv('principal_offence_category_december_2016.csv', col_names = TRUE)
january_2017 <- read_csv('principal_offence_category_january_2017.csv', col_names = TRUE)
february_2017 <- read_csv('principal_offence_category_february_2017.csv', col_names = TRUE)
march_2017 <- read_csv('principal_offence_category_march_2017.csv', col_names = TRUE)

head(march_2017)
colnames(df1)
# check data type of rows

#Add data frames to a list name of the data frame is set so that it can change to a date later
listOfDataframes <- list("1Jul2015"=july_2015,"1Aug2015"=august_2015,"1Sep2015"=september_2015,"1Oct2015"=october_2015,"1Dec2015"=december_2015,
                         "1Jan2016" = january_2016,"1Apr2016"=april_2016,"1May2016"=may_2016,"1Jun2016" = june_2016,"1Jul2016"=july_2016,
                         "1Aug2016"=august_2016,"1Sep2016"=september_2016, "1Oct2016"=october_2016, "1Nov2016"=november_2016, "1Dec2016"=december_2016,
                         "1Jan2017"=january_2017,"1Feb2017"=february_2017,"1Mar2017"=march_2017)

# adding date as the first of the month that data published to convert data in to time series and merge
for (i in seq_along(listOfDataframes)){
  listOfDataframes[[i]] <- cbind(listOfDataframes[[i]], Date = rep(c(as.Date(names(listOfDataframes[(i)]), "%d%b%Y")),each=43))
}
listOfDataframes[[9]]$Date

timeseriesdf <- listOfDataframes[[1]]
for (i in 2:length(listOfDataframes)){
  timeseriesdf <- rbind(timeseriesdf,listOfDataframes[[i]])
}

# Defining abbreviated column names to rename the existing names
colnames <- c("Court", "N_HC", "P_HC", "N_HU", "P_HU", "N_OAPC", "P_OAPC", "N_OAPU", "P_OAPU","N_SOC", "P_SOC", "N_SOU", "P_SOU","N_BC",
              "P_BC","N_BU", "P_BU","N_RC","P_RC","N_RU","P_RU","N_THC","P_THC","N_THU","P_THU","N_FFC","P_FFC","N_FFU","P_FFU","N_CDC",
              "P_CDC","N_CDU","P_CDU","N_DOC","P_DOC","N_DOU","P_DOU","N_POOC","P_POOC","N_POOU","P_POOU","N_OTHERC","P_OTHERC","N_OTHERU",
              "P_OTHERU","N_MOC", "P_MOC","N_MOU","P_MOU","N_AFU","P_LMOU", "Date" )

# Change column names of timeseriesdf
colnames(timeseriesdf) <- colnames
colnames(july_2016) <- colnames
colnames(august_2016) <- colnames
#copy first column data to a vector
court_col <- timeseriesdf$Court
date_col <- timeseriesdf$Date

# Remove percentage sign in the data set and convert to numeric
suppressWarnings(timeseriesdf <- data.frame(sapply(timeseriesdf, function(x) as.numeric(gsub("%", "", x)))))

# Correct the first column data that was set to NA when converting data to NA
timeseriesdf <- cbind(timeseriesdf, data.frame(court = court_col))
timeseriesdf <- cbind(timeseriesdf, data.frame(date = date_col))

#remove columns with all NA and "P_LMOU" (as this column has values only 100 and NA)
timeseriesdf <- timeseriesdf[ , !names(timeseriesdf) %in% c("Court","Date","P_LMOU")]

view(timeseriesdf)

timeseriesdf$court  <- ifelse(timeseriesdf$court  %in% c('Metropolitan and City'), "Metropolitan & City",timeseriesdf$court)
timeseriesdf$court  <- ifelse(timeseriesdf$court %in% c('Avon and Somerset'), "Avon & Somerset",timeseriesdf$court)
timeseriesdf$court  <- ifelse(timeseriesdf$court %in% c('Devon and Cornwall'), "Devon & Cornwall",timeseriesdf$court)
write.csv(timeseriesdf,"timeseriesdf.csv", row.names = TRUE)

#copy national data to a new data frame
nationaldf <-filter(timeseriesdf, court == "National")

view(nationaldf)
#Drop the court column as it has value National for all the observations
nationaldf <- nationaldf[ , !names(nationaldf) %in% c("court")]

# Drop the rows with National data
  
timeseries_df <- timeseriesdf[!(timeseriesdf$court %in% c("National")), ]
view(timeseries_df)

# Bar graph for National values
# Homicide convictions
p1 <- ggplot(nationaldf,aes(x=date,y=N_HC))+geom_bar(stat="identity") + ggtitle("National number of homicide convictions from July 2015 to March 2017") +
  xlab("Month") + ylab("Number of homicide convictions")
ggsave(p1, filename = "Na_no_hc.png")

p2 <-ggplot(nationaldf,aes(x=date,y=N_HU))+geom_bar(stat="identity") + ggtitle("National number of homicide unsuccessful from July 2015 to March 2017")+ 
  xlab("Month") + ylab("Number of homicide unsuccessful")
ggsave(p2, filename = "Na_no_hu.png")

# Offences against the person convictions
p3 <- ggplot(nationaldf,aes(x=date,y=N_OAPC))+geom_bar(stat="identity") + ggtitle("National number of offences against the person convictions from July 2015 to March 2017") +
  xlab("Month") + ylab("Number of offences against the person convictions")
ggsave(p3, filename = "Na_no_oapc.png")

p4 <- ggplot(nationaldf,aes(x=date,y=N_OAPU))+geom_bar(stat="identity") + ggtitle("National number of offences against the person unsuccessful from July 2015 to March 2017")+ 
  xlab("Month") + ylab("Number of offences against the person unsuccessful")
ggsave(p4, filename = "Na_no_oapu.png")

# Sexual offences
p5 <- ggplot(nationaldf,aes(x=date,y=N_SOC))+geom_bar(stat="identity") + ggtitle("National number of sexual offences convictions from July 2015 to March 2017")+ 
  xlab("Month") + ylab("Number of sexual offences convictions")
ggsave(p5, filename = "Na_of_soc.png")

p6 <- ggplot(nationaldf,aes(x=date,y=N_SOU))+geom_bar(stat="identity") + ggtitle("National number of sexual offences unsuccessful from July 2015 to March 2017")+ 
  xlab("Month") + ylab("Number of sexual offences unsuccessful")
ggsave(p6, filename = "Na_of_sou.png")

# Burglary
p7 <- ggplot(nationaldf,aes(x=date,y=N_BC))+geom_bar(stat="identity") + ggtitle("National number of burglary convictions unsuccessful from July 2015 to March 2017")+ 
  xlab("Month") + ylab("Number of burglary convictions")
ggsave(p7, filename = "Na_of_bc.png")

p8 <- ggplot(nationaldf,aes(x=date,y=N_BU))+geom_bar(stat="identity") + ggtitle("National number of burglary unsuccessful from July 2015 to March 2017")+ 
  xlab("Month") + ylab("Number of burglary unsuccessful")
ggsave(p8, filename = "Na_of_bu.png")

# Robbery
p9 <- ggplot(nationaldf,aes(x=date,y=N_RC))+geom_bar(stat="identity") + ggtitle("National number of robbery convictions from July 2015 to March 2017")+ 
  xlab("Month") + ylab("Number of robbery convictions")
ggsave(p9, filename = "Na_of_rc.png")

p10 <- ggplot(nationaldf,aes(x=date,y=N_RU))+geom_bar(stat="identity") + ggtitle("National number of robbery unsuccessful from July 2015 to March 2017")+ 
  xlab("Month") + ylab("Number of robbery unsuccessful")
ggsave(p10, filename = "Na_no_ru.png")

# Theft And Handling
p11 <- ggplot(nationaldf,aes(x=date,y=N_THC))+geom_bar(stat="identity") + ggtitle("National number of theft and handling convictions from July 2015 to March 2017")+ 
  xlab("Month") + ylab("Number of theft and handling convictions")
ggsave(p11, filename = "Na_no_tahc.png")

p12 <- ggplot(nationaldf,aes(x=date,y=N_THU))+geom_bar(stat="identity") + ggtitle("National number of theft and handling unsuccessful from July 2015 to March 2017")+ 
  xlab("Month") + ylab("Number of robbery unsuccessful")
ggsave(p12, filename = "Na_no_tahu.png")

# Fraud And Forgery
p13 <- ggplot(nationaldf,aes(x=date,y=N_FFC))+geom_bar(stat="identity") + ggtitle("National number of fraud and forgery convictions from July 2015 to March 2017")+ 
  xlab("Month") + ylab("Number of fraud and forgery convictions")
ggsave(p13, filename = "Na_of_fafc.png")

p14 <- ggplot(nationaldf,aes(x=date,y=N_FFU))+geom_bar(stat="identity") + ggtitle("National number of fraud and forgery unsuccessful from July 2015 to March 2017")+ 
  xlab("Month") + ylab("Number of fraud and forgery unsuccessful")
ggsave(p14, filename = "Na_no_fafu.png")

# Criminal Damage
p15 <- ggplot(nationaldf,aes(x=date,y=N_CDC))+geom_bar(stat="identity") + ggtitle("National number of criminal damage convictions from July 2015 to March 2017")+ 
  xlab("Month") + ylab("Number of criminal damage convictions")
ggsave(p15, filename = "Na_of_cdc.png")

p16 <- ggplot(nationaldf,aes(x=date,y=N_CDU))+geom_bar(stat="identity") + ggtitle("National number of criminal damage unsuccessful from July 2015 to March 2017")+ 
  xlab("Month") + ylab("Number of criminal damage unsuccessful")
ggsave(p16, filename = "Na_no_cdu.png")

# Drugs Offences
p17 <- ggplot(nationaldf,aes(x=date,y=N_DOC))+geom_bar(stat="identity") + ggtitle("National number of drugs offences convictions from July 2015 to March 2017")+ 
  xlab("Month") + ylab("Number of drugs offences convictions")
ggsave(p17, filename = "Na_no_doc.png")

p18 <- ggplot(nationaldf,aes(x=date,y=N_DOU))+geom_bar(stat="identity") + ggtitle("National number of drugs offences unsuccessful from July 2015 to March 2017")+ 
  xlab("Month") + ylab("Number of drugs offences unsuccessful")
ggsave(p18, filename = "Na_no_dou.png")

# Public Order Offences
p19 <- ggplot(nationaldf,aes(x=date,y=N_POOC))+geom_bar(stat="identity") + ggtitle("National number of public order offences convictions from July 2015 to March 2017")+ 
  xlab("Month") + ylab("Number of public order offences convictions")
ggsave(p19, filename = "Na_no_pooc.png")

p20 <- ggplot(nationaldf,aes(x=date,y=N_POOU))+geom_bar(stat="identity") + ggtitle("National number of public order offencess unsuccessful from July 2015 to March 2017")+ 
  xlab("Month") + ylab("Number of public order offences unsuccessful")
ggsave(p20, filename = "Na_no_poou.png")

# Other
p21 <- ggplot(nationaldf,aes(x=date,y=N_OTHERC))+geom_bar(stat="identity") + ggtitle("National number of other convictions from July 2015 to March 2017")+ 
  xlab("Month") + ylab("Number of other convictions")
ggsave(p21, filename = "Na_of_otherc.png")

p22 <- ggplot(nationaldf,aes(x=date,y=N_OTHERU))+geom_bar(stat="identity") + ggtitle("National number of other unsuccessful from July 2015 to March 2017")+ 
  xlab("Month") + ylab("Number of other unsuccessful")
ggsave(p22, filename = "Na_of_otheru.png")

# Motoring Offences
p23 <- ggplot(nationaldf,aes(x=date,y=N_MOC))+geom_bar(stat="identity") + ggtitle("National number of motoring offences convictions from July 2015 to March 2017")+ 
  xlab("Month") + ylab("Number of motoring offences convictions")
ggsave(p23, filename = "Na_of_moc.png")

p24 <- ggplot(nationaldf,aes(x=date,y=N_MOU))+geom_bar(stat="identity") + ggtitle("National number of motoring offences unsuccessful from July 2015 to March 2017")+ 
  xlab("Month") + ylab("Number of motoring offences unsuccessful")
ggsave(p24, filename = "Na_of_mou.png")

# Admin Finalised Unsuccessful
p25 <- ggplot(nationaldf,aes(x=date,y=N_AFU))+geom_bar(stat="identity") + ggtitle("National number of admin finalised unsuccessful from July 2015 to March 2017")+ 
  xlab("Month") + ylab("Number of admin finalised unsuccessful")
ggsave(p25, filename = "Na_of_afu.png")



#dfm <- melt(nationaldf[,c('N_OAPC','N_MOC','date')],id.vars = 1)
#ggplot(dfm,aes(x = N_OAPC, y = N_MOC)) + 
  #geom_bar(aes(fill = date),stat = "identity",position = "dodge")

res <- cor.test(july_2016[13], july_2016[13], method = "pearson")
res

g1507 <- filter(timeseries_df, date == "2015-07-01" & court == "Gloucestershire")
g1607 <- filter(timeseries_df, date == "2016-07-01" & court == "Gloucestershire")

colnames(timeseries_df)
timeseries_df$court
df_grp_court <- timeseries_df %>% group_by(court)  %>%
  summarise(total_N_HC = sum(N_HC),
            total_N_OAPC = sum(N_OAPC),
            total_N_SOC = sum(N_SOC),
            total_N_SOU = sum(N_SOU),
            total_N_THC = sum(N_THC),
            total_N_BC = sum(N_BC),
            total_N_RC = sum(N_RC),
            total_N_FFC = sum(N_FFC),
            total_N_CDC = sum(N_CDC),
            total_N_DOC = sum(N_DOC),
            total_N_POOC = sum(N_POOC),
            total_N_MOC = sum(N_MOC),
            total_N_OTHERC = sum(N_OTHERC),
            .groups = 'drop')
view(df_grp_court)

df_total_cases <- timeseries_df %>% group_by(court)  %>%
  summarise(total_N_HC = sum(N_HC)+sum(N_HU),
            total_N_OAPC = sum(N_OAPC)+sum(N_OAPU),
            total_N_SOC = sum(N_SOC)+sum(N_SOU),
            total_N_BC = sum(N_BC)+sum(N_BU),
            total_N_RC = sum(N_RC)+sum(N_RU),
            total_N_THC = sum(N_THC)+sum(N_THU),
            total_N_FFC = sum(N_FFC)+sum(N_FFU),
            total_N_CDC = sum(N_CDC)+sum(N_CDU),
            total_N_DOC = sum(N_DOC)+sum(N_DOU),
            total_N_POOC = sum(N_POOC)+sum(N_POOU),
            total_N_MOC = sum(N_MOC)+sum(N_MOU),
            total_N_OTHERC = sum(N_OTHERC)+sum(N_OTHERU),
            .groups = 'drop')
df <- melt(df_total_cases , id.vars = 'court', variable.name = 'series')
ggplot(df, aes(court,value, group = 1)) + geom_line(aes(colour = series))

# or plot on different plots
ggplot(df, aes(court,log(value), group = 1)) + geom_line() + facet_grid(series ~ .)
ggplot(df, aes(x=court, y=value))+
  geom_bar(stat='identity', fill="blue")+
  facet_wrap(series ~ .,  ncol=1, scale="free_y")+
  ggtitle("Total cases vs court") + theme(axis.text.x=element_text(angle=30,vjust =1, hjust=1))
view(df_total_cases)

# Plot the bar chart 
barplot(df_total_cases$total_N_MOC,names.arg=df_total_cases$court,xlab="Month",ylab="case count",col="blue",
        main="N_MO chart",border="purple")
barplot(df_total_cases$total_N_THC,names.arg=df_total_cases$court,xlab="Month",ylab="case count",col="blue",
        main="N_TH chart",border="purple")

View(df_grp_region)

df_bfs <-filter(timeseriesdf, court == "Bedfordshire")
summary(df_bfs)
ggplot(df_bfs, aes(date,N_MOC, group = 1)) + geom_point()
x <- as.POSIXct(df_bfs$date, format="%Y-%m-%d")
x
y <- as.numeric(df_bfs$date)
as.numeric(x)
ggplot(df_bfs, aes(x=date, y=N_MOC))
ggscatter(df_bfs, x = "date", y = "N_MOC", 
            add = "reg.line", conf.int = TRUE, 
            cor.coef = TRUE, cor.method = "pearson",
            xlab = "Month", ylab = "Number of Motoring offence convictions")
# check if the distribution is normally distributed
ggqqplot(df_bfs$N_MOC, distribution = "norm", ylab = "Number of Motoring offence convictions")

#check if the distribution is uniformally distributed

#corelation
res <- cor.test(df_bfs$N_MOC, y, method = "pearson")
res
# Heat map
df_bfs_numeric <- subset(df_bfs, , -c(court, date, P_HC, N_HU,P_HU,P_OAPC,N_OAPU,P_OAPU,P_SOC,N_SOU,P_SOU,P_BC,N_BU,P_BU,P_RC,N_RU,P_RU,P_THC,N_THU,P_THU,P_FFC,N_FFU,P_FFU,
                                      P_CDC,N_CDU,P_CDU,P_DOC,N_DOU,P_DOU,P_POOC,N_POOU,P_POOU,P_MOU,N_AFU,P_MOC,P_OTHERC,N_OTHERU,P_OTHERU))
cov(df_bfs_numeric)

listOfDataframes[[1]] %>% ggplot(aes(y = ...1, x=listOfDataframes[[1]][22] , group = 1)) + 
  geom_bar(color="#69b3a2",stat='identity')+ 
  xlab("Number of Theft And Handling Convictions") + ylab("Number of court cases ") +
  ggtitle("Number of Theft And Handling Convictions by the Court for July 2015")


# x scale copied from https://ggplot2-book.org/scale-position.html#date-labels
p <- timeseriesdf %>%
  ggplot( aes(x=Date, y=P_HC, group=court, fill=court)) +
  geom_area(stat='identity') +
  ggtitle("Percentage of Homicide Convictions from 2014 to 2015") +
  scale_x_date(limits = as.Date(c("2014-01-01", "2015-12-01")), 
               labels = scales::label_date_short())+ 
  theme(
    legend.position="center",
  ) + facet_wrap(~court, scale="free_y") 

                                                         
ggsave(p, filename = "Percentage of Homicide Convictions.png")
names(timeseriesdf)
unique(timeseriesdf$Police)
names(df1)
mean_vec <- vector()
median_vec <- vector()
sd_vec <- vector()
x <- vector()
for (i in 2:(ncol(timeseriesdf) -1)){
  x[i] <- colnames(timeseriesdf)[i]
  mean_vec[i] <- mean(timeseriesdf[ ,i],na.rm=TRUE)
  median_vec[i] <- median(timeseriesdf[ ,i],na.rm=TRUE)
  sd_vec[i] <- sd(timeseriesdf[ ,i],na.rm=TRUE)
}
stats_pc <- data.frame(Principal_crime = x, 
                       mean_values = mean_vec, 
                       median_values = median_vec, 
                       sd_values = sd_vec)
stats_pc <- stats_pc[ -1, ] 

colnames(stats_pc)

p1 <- ggplot(stats_pc, aes(Principal_crime, mean_values, group = 1)) + 
  geom_line()+
  ggtitle("Mean value of the number of court cases finalised by principal offence") +
  theme(axis.text.x=element_text(angle=90,vjust =1))

ggplotly(p1)

p2 <- ggplot(stats_pc, aes(Principal_crime, median_values, group = 1)) + 
  geom_line()+
  ggtitle("Mean value of the number of court cases finalised by principal offence") +
  theme(axis.text.x=element_text(angle=90,vjust =1))

ggplotly(p2)
p3 <- ggplot(stats_pc, aes(Principal_crime, sd_values, group = 1)) + 
  geom_line()+
  ggtitle("Standard deviation value of the number of court cases finalised by principal offence") +
  theme(axis.text.x=element_text(angle=90,vjust =1))

ggplotly(p3)




