library(ggplot2) 
library(tidyverse)


# Read data from csv to data frame
df1 = read_csv('principal_offence_category_march_2017.csv', col_names = TRUE)
march_2017 = read_csv('principal_offence_category_march_2017.csv', col_names = TRUE)
february_2017 = read_csv('principal_offence_category_february_2017.csv', col_names = TRUE)
january_2017 = read_csv('principal_offence_category_january_2017.csv', col_names = TRUE)
december_2016 = read_csv('principal_offence_category_december_2016.csv', col_names = TRUE)
november_2016 = read_csv('principal_offence_category_november_2016.csv', col_names = TRUE)
october_2016 = read_csv('principal_offence_category_october_2016.csv', col_names = TRUE)
september_2016 = read_csv('principal_offence_category_september_2016.csv', col_names = TRUE)
august_2016 = read_csv('principal_offence_category_august_2016.csv', col_names = TRUE)
july_2016 = read_csv('principal_offence_category_july_2016.csv', col_names = TRUE)
june_2016 = read_csv('principal_offence_category_june_2016.csv', col_names = TRUE)
may_2016 = read_csv('principal_offence_category_may_2016.csv', col_names = TRUE)
april_2016 = read_csv('principal_offence_category_april_2016.csv', col_names = TRUE)
january_2016 = read_csv('principal_offence_category_january_2016.csv', col_names = TRUE)
december_2015 = read_csv('principal_offence_category_december_2015.csv', col_names = TRUE)
october_2015 = read_csv('principal_offence_category_october_2015.csv', col_names = TRUE)
september_2015 = read_csv('principal_offence_category_september_2015.csv', col_names = TRUE)
august_2015 = read_csv('principal_offence_category_august_2015.csv', col_names = TRUE)
july_2015 = read_csv('principal_offence_category_july_2015.csv', col_names = TRUE)
june_2015 = read_csv('principal_offence_category_june_2015.csv', col_names = TRUE)
may_2015 = read_csv('principal_offence_category_may_2015.csv', col_names = TRUE)
april_2015 = read_csv('principal_offence_category_april_2015.csv', col_names = TRUE)


# check data type of rows
class(february_2017$N_AFU)
head(february_2017)

#Add dataframes to a list
listOfDataframes = list("Mar_2017" = march_2017, "Feb_2017" = february_2017,"Jan_2017" =january_2017,"Dec_2016"=december_2016,"Nov_2016"=november_2016,"Oct_2016" = october_2016,
                        "Sep_2016"=september_2016,"Aug_2016"=august_2016,"Jul_2016"=july_2016,"Jun_2016"=june_2016,"May_2016"=may_2016,"Apr_2016"=april_2016,
                        "Jan_2016"=january_2016,"Dec_2015"=december_2015,"Oct_2015"=october_2015,"Sep_2015"=september_2015,"Aug_2015"=august_2015,"Jul_2015"=july_2015,
                        "Jun_2015"=june_2015,"May_2015"=may_2015,"Apr_2015"=april_2015)

colnames = c("Area", "N_HC", "P_HC", "N_HU", "P_HU", "N_OAPC", "P_OAPC", "N_OAPU", "P_OAPU","N_SOC", "P_SOC", "N_SOU", "P_SOU","N_BC",
    "P_BC","N_BU", "P_BU","N_RC","P_RC","N_RU","P_RU","N_THC","P_THC","N_THU","P_THU","N_FFC","P_FFC","N_FFU","P_FFU","N_CDC",
    "P_CDC","N_CDU","P_CDU","N_DOC","P_DOC","N_DOU","P_DOU","N_POOC","P_POOC","N_POOU","P_POOU","N_OTHERC","P_OTHERC","N_OTHERU",
    "P_OTHERU","N_MOC", "P_MOC","N_MOU","P_MOU","N_AFU","P_LMOU" )
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
national <-  data.frame(
  c(listOfDataframes[[i]][1,])
)
national[national == "National"] <- "Mar_2017"

for (i in 2:length(listOfDataframes)){
  national <- rbind(national, listOfDataframes[[i]][1,])
  national[national == "National"] <- names(listOfDataframes)[i]
}
view(national)
# Change the first column name without changing others
colnames(national) <- c("Period", "N_HC", "P_HC", "N_HU", "P_HU", "N_OAPC", "P_OAPC", "N_OAPU", "P_OAPU","N_SOC", "P_SOC", "N_SOU", "P_SOU","N_BC",
                          "P_BC","N_BU", "P_BU","N_RC","P_RC","N_RU","P_RU","N_THC","P_THC","N_THU","P_THU","N_FFC","P_FFC","N_FFU","P_FFU","N_CDC",
                          "P_CDC","N_CDU","P_CDU","N_DOC","P_DOC","N_DOU","P_DOU","N_POOC","P_POOC","N_POOU","P_POOU","N_OTHERC","P_OTHERC","N_OTHERU",
                          "P_OTHERU","N_MOC", "P_MOC","N_MOU","P_MOU","N_AFU","P_LMOU" ) 

# Drop the row with National data
for (i in seq_along(listOfDataframes)){
  
  listOfDataframes[[i]] = listOfDataframes[[i]][!(listOfDataframes[[i]]$Area %in% c("National")), ]
}

#bar graph for N_OAPC from April 2015 ~ March 2017 for National values

em <- ggplot(national, aes(Period, N_OAPC))
em + geom_col()

em <- ggplot(feb_2017, aes(N_OAPC, Area))
em + geom_col()

#ggplot(feb_2017,aes(x = N_MOU)) + geom_density(fill = "blue")


summary(feb_2017)

feb_2017
