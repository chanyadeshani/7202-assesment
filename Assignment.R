library(ggplot2) 
library(tidyverse)
library(lubridate)
library(data.table)
library(plotly)

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

# Defining abbriviated column names to rename the existing names
colnames <- c("Court", "N_HC", "P_HC", "N_HU", "P_HU", "N_OAPC", "P_OAPC", "N_OAPU", "P_OAPU","N_SOC", "P_SOC", "N_SOU", "P_SOU","N_BC",
              "P_BC","N_BU", "P_BU","N_RC","P_RC","N_RU","P_RU","N_THC","P_THC","N_THU","P_THU","N_FFC","P_FFC","N_FFU","P_FFU","N_CDC",
              "P_CDC","N_CDU","P_CDU","N_DOC","P_DOC","N_DOU","P_DOU","N_POOC","P_POOC","N_POOU","P_POOU","N_OTHERC","P_OTHERC","N_OTHERU",
              "P_OTHERU","N_MOC", "P_MOC","N_MOU","P_MOU","N_AFU","P_LMOU", "Date" )

# Change column names of timeseriesdf
colnames(timeseriesdf) <- colnames

#copy first column data to a vector
court_col <- timeseriesdf$Court
date_col <- timeseriesdf$Date
view(timeseriesdf)

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






for (i in seq_along(listOfDataframes)){
  # Change column names
  colnames(listOfDataframes[[i]]) <- colnames
  
  # Remove percentage sign in the data set and convert to numeric
  suppressWarnings(listOfDataframes[[i]] <- data.frame(sapply(listOfDataframes[[i]], function(x) as.numeric(gsub("%", "", x)))))
  
  # Correct the first column data that was set to NA when converting data to NA
  for (j in 1:nrow(listOfDataframes[[i]])){
    listOfDataframes[[i]][j,1]<-df1[j,1]
  }
}

#copy national data to a new data frame
national <-  data.frame(c(listOfDataframes[[i]][1,]))
national[national == "National"] <- names(listOfDataframes)[1]

for (i in 2:length(listOfDataframes)){
  national <- rbind(national, listOfDataframes[[i]][1,])
  national[national == "National"] <- names(listOfDataframes)[i]
}

#view(national)
# Change the first column name without changing others
names(national)[names(national) == 'court'] <- 'period'

# Drop the row with National data
for (i in seq_along(listOfDataframes)){
  
  listOfDataframes[[i]] <- listOfDataframes[[i]][!(listOfDataframes[[i]]$court  %in% c("National")), ]
}

# Find total number of offences 
#df_Total <- 

#bar graph for N_OAPC from April 2015 ~ March 2017 for National values
national$Period <- factor(national$Period, levels = national$Period)
ggplot(national,aes(x=Period,y=N_THC))+geom_bar(stat="identity") + ggtitle("Number of National Theft And Handling Convictions")

ggplot(listOfDataframes[[4]], aes(N_THC ))+geom_boxplot()+ggtitle("Number of Offences Against The Person Convictions in January 2017")

view(listOfDataframes[1])
names(listOfDataframes[1])

names(listOfDataframes[[1]])
view(listOfDataframes[[1]])

#ggplot(feb_2017,aes(x = N_MOU)) + geom_density(fill = "blue")

print(listOfDataframes[[2]])
summary(feb_2017)
df_N_OAPC <- listOfDataframes[[1]][ , ("court ")]
for (i in seq_along(listOfDataframes)){
  df_N_OAPC <- cbind(df_N_OAPC, listOfDataframes[[i]][,6])
  }
colnames(df_N_OAPC) <- c("court ", "Jan_2014", "Feb_2014","Mar_2014","Apr_2014","May_2014","Jun_2014","Jul_2014","Aug_2014"
                         ,"Sep_2014","Oct_2014","Nov_2014","Dec_2014","Jan_2015","Feb_2015","Mar_2015","Apr_2015","May_2015",
                         "Jun_2015", "Jul_2015","Aug_2015","Sep_2015","Oct_2015","Dec_2015" ) 
view(df_N_OAPC)

#transpose data frame
dft_N_OAPC = t(df_N_OAPC)
#redefine row and column names
dft_N_OAPC = dft_N_OAPC[-c(1), ]
colnames(dft_N_OAPC) = df_N_OAPC[,1]

colnames(dft_N_OAPC)[colnames(dft_N_OAPC) == "Avon and Somerset"] <- "Avon_and_Somerset"
colnames(dft_N_OAPC)[colnames(dft_N_OAPC) == "Devon and Cornwall"] <- "Devon_and_Cornwall"
colnames(dft_N_OAPC)[colnames(dft_N_OAPC) == "Metropolitan and City"] <- "Metropolitan_and_City"
colnames(dft_N_OAPC)[colnames(dft_N_OAPC) == "North Yorkshire"] <- "North_Yorkshire"
colnames(dft_N_OAPC)[colnames(dft_N_OAPC) == "North Wales"] <- "North_Wales"
colnames(dft_N_OAPC)[colnames(dft_N_OAPC) == "South Wales"] <- "South_Wales"
colnames(dft_N_OAPC)[colnames(dft_N_OAPC) == "South Yorkshire"] <- "South_Yorkshire"
colnames(dft_N_OAPC)[colnames(dft_N_OAPC) == "West Midlands"] <- "West_Midlands"
colnames(dft_N_OAPC)[colnames(dft_N_OAPC) == "West Mercia"] <- "West_Mercia"
colnames(dft_N_OAPC)[colnames(dft_N_OAPC) == "Thames Valley"] <- "Thames_Valley"
colnames(dft_N_OAPC)[colnames(dft_N_OAPC) == "West Yorkshire"] <- "West_Yorkshire"
colnames(dft_N_OAPC)[colnames(dft_N_OAPC) == "Dyfed Powys"] <- "Dyfed_Powys"


rownames(dft_N_OAPC)
dft_N_OAPC=as.data.frame(dft_N_OAPC)
dft_N_OAPC <- dft_N_OAPC %>% rownames_to_column(var="Period")
#rownames(dft_N_OAPC) = c("Jan_2014", "Feb_2014", "Mar_2014", "Apr_2014", "May_2014", "Jun_2014", "Jul_2014", "Aug_2014",
#                         "Sep_2014", "Oct_2014", "Nov_2014", "Dec_2014", "Jan_2015", "Feb_2015", "Mar_2015", "Apr_2015",
#                        "May_2015", "Jun_2015", "Jul_2015", "Aug_2015", "Sep_2015", "Oct_2015", "Dec_2015" ) 
view(dft_N_OAPC)
names(dft_N_OAPC)

ggplot(data=dft_N_OAPC, aes(x=Period, y=Avon_and_Somerset)) +
  geom_bar()
ggplot(dft_N_OAPC, aes(x=Period, y= Avon_and_Somerset)) + geom_bar(stat='identity')

listOfDataframes[[4]]%>%ggplot(aes(y = Area, x=P_OAPC , group = 1)) + 
  geom_bar(color="#69b3a2",stat='identity')+ 
  xlab("Percentage of Offences Against The Person Convictions") + ylab("Court ") +
  ggtitle("Percentage of offences against the person convictions by the Court for January 2014")







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

dfbfs <-filter(timeseriesdf, Police == "Bedfordshire")
summary(dfbfs)



