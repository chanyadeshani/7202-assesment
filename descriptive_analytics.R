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
july_2017 <- read_csv('principal_offence_category_Jul_2017.csv', col_names = TRUE)
august_2017 <- read_csv('principal_offence_category_Aug_2017.csv', col_names = TRUE)
september_2017 <- read_csv('principal_offence_category_Sep_2017.csv', col_names = TRUE)
october_2017 <- read_csv('principal_offence_category_Oct_2017.csv', col_names = TRUE)
november_2017 <- read_csv('principal_offence_category_Nov_2017.csv', col_names = TRUE)
december_2017 <- read_csv('principal_offence_category_Dec_2017.csv', col_names = TRUE)
january_2018 <- read_csv('principal_offence_category_Jan_2018.csv', col_names = TRUE)
february_2018 <- read_csv('principal_offence_category_Feb_2018.csv', col_names = TRUE)
march_2018 <- read_csv('principal_offence_category_Mar_2018.csv', col_names = TRUE)

listOfDataframes <- list("1Jul2015" = july_2015, "1Aug2015"= august_2015, "1Sep2015" = september_2015, "1Oct2015" = october_2015, "1Dec2015" = december_2015,
                         "1Jan2016" = january_2016, "1Apr2016" = april_2016, "1May2016" = may_2016, "1Jun2016" = june_2016, "1Jul2016" = july_2016,
                         "1Aug2016" = august_2016, "1Sep2016" = september_2016, "1Oct2016" = october_2016, "1Nov2016" = november_2016, "1Dec2016"= december_2016,
                         "1Jan2017" = january_2017,"1Feb2017" = february_2017, "1Mar2017" = march_2017, "1Jul2017" = july_2017, "1Aug2017" = august_2017, 
                         "1Sep2017" = september_2017, "1Oct2017" = october_2017, "1Nov2017" = november_2017, "1Dec2017" = december_2017, "1Jan2018" = january_2018,
                         "1Feb2018" = february_2018, "1Mar2018" = march_2018)

# adding date as the first of the month that data published to convert data in to time series and merge
for (i in seq_along(listOfDataframes)){
  listOfDataframes[[i]] <- cbind(listOfDataframes[[i]], Date = rep(c(as.Date(names(listOfDataframes[(i)]), "%d%b%Y")),each=43))
}
listOfDataframes[[9]]$Date

full_df <- listOfDataframes[[1]]
for (i in 2:length(listOfDataframes)){
  full_df <- rbind(full_df,listOfDataframes[[i]])
}

# Defining abbreviated column names to rename the existing names
colnames <- c("Court", "N_HC", "P_HC", "N_HU", "P_HU", "N_OAPC", "P_OAPC", "N_OAPU", "P_OAPU","N_SOC", "P_SOC", "N_SOU", "P_SOU","N_BC",
              "P_BC","N_BU", "P_BU","N_RC","P_RC","N_RU","P_RU","N_THC","P_THC","N_THU","P_THU","N_FFC","P_FFC","N_FFU","P_FFU","N_CDC",
              "P_CDC","N_CDU","P_CDU","N_DOC","P_DOC","N_DOU","P_DOU","N_POOC","P_POOC","N_POOU","P_POOU","N_OTHERC","P_OTHERC","N_OTHERU",
              "P_OTHERU","N_MOC", "P_MOC","N_MOU","P_MOU","N_AFU","P_LMOU", "Date" )
colnames(full_df) <- colnames
#copy first column data to a vector
court_col <- full_df$Court
date_col <- full_df$Date
# Check missing values
full_df[!complete.cases(full_df),]

# Remove percentage sign in the data set and convert to numeric
suppressWarnings(full_df <- data.frame(sapply(full_df, function(x) as.numeric(gsub("%", "", x)))))

# Correct the first column data that was set to NA when converting data to NA
full_df <- cbind(full_df, data.frame(court = court_col))
full_df <- cbind(full_df, data.frame(date = date_col))

# Remove columns with all NA and "P_LMOU" (as this column has values only 100 and NA)
full_df <- full_df[ , !names(full_df) %in% c("Court","Date","P_LMOU")]

view(full_df)
full_df$court  <- ifelse(full_df$court  %in% c('Metropolitan and City'), "Metropolitan & City",full_df$court)
full_df$court  <- ifelse(full_df$court %in% c('Avon and Somerset'), "Avon & Somerset",full_df$court)
full_df$court  <- ifelse(full_df$court %in% c('Devon and Cornwall'), "Devon & Cornwall",full_df$court)