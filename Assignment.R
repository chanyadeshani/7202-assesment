library(ggplot2) 
library(tidyverse)
library(lubridate)
library(data.table)

# Read data from csv to data frame
df1 <- read_csv('principal_offence_category_march_2017.csv', col_names = TRUE)
names(df1)

january_2014 <- read_csv('principal_offence_category_january_2014.csv', col_names = TRUE)
february_2014 <- read_csv('principal_offence_category_february_2014.csv', col_names = TRUE)
march_2014 <- read_csv('principal_offence_category_march_2014.csv', col_names = TRUE)
april_2014 <- read_csv('principal_offence_category_april_2014.csv', col_names = TRUE)
may_2014 <- read_csv('principal_offence_category_may_2014.csv', col_names = TRUE)
june_2014 <- read_csv('principal_offence_category_june_2014.csv', col_names = TRUE)
july_2014 <- read_csv('principal_offence_category_july_2014.csv', col_names = TRUE)
august_2014 <- read_csv('principal_offence_category_august_2014.csv', col_names = TRUE)
september_2014 <- read_csv('principal_offence_category_september_2014.csv', col_names = TRUE)
october_2014 <- read_csv('principal_offence_category_october_2014.csv', col_names = TRUE)
november_2014 <- read_csv('principal_offence_category_november_2014.csv', col_names = TRUE)
december_2014 <- read_csv('principal_offence_category_december_2014.csv', col_names = TRUE)
january_2015 <- read_csv('principal_offence_category_january_2015.csv', col_names = TRUE)
february_2015 <- read_csv('principal_offence_category_february_2015.csv', col_names = TRUE)
march_2015 <- read_csv('principal_offence_category_march_2015.csv', col_names = TRUE)
april_2015 <- read_csv('principal_offence_category_april_2015.csv', col_names = TRUE)
may_2015 <- read_csv('principal_offence_category_may_2015.csv', col_names = TRUE)
june_2015 <- read_csv('principal_offence_category_june_2015.csv', col_names = TRUE)
july_2015 <- read_csv('principal_offence_category_july_2015.csv', col_names = TRUE)
august_2015 <- read_csv('principal_offence_category_august_2015.csv', col_names = TRUE)
september_2015 <- read_csv('principal_offence_category_september_2015.csv', col_names = TRUE)
october_2015 <- read_csv('principal_offence_category_october_2015.csv', col_names = TRUE)
december_2015 <- read_csv('principal_offence_category_december_2015.csv', col_names = TRUE)


# check data type of rows
head(february_2014)

#Add data frames to a list
listOfDataframes <- list("Jan_2014" = january_2014, "Feb_2014" = february_2014,"Mar_2014" =march_2014,"Apr_2014"=april_2014,"May_2014"=may_2014,"Jun_2014" = june_2014,
                        "Jul_2014"=july_2014,"Aug_2014"=august_2014,"Sep_2014"=september_2014,"Oct_2014"=october_2014,"Nov_2014"=november_2014,"Dec_2014"=december_2014,
                        "Jan_2015"=january_2015,"Feb_2015"=february_2015,"Mar_2015"=march_2015,"Apr_2015"=april_2015,"May_2015"=may_2015,"Jun_2015"=june_2015,
                        "Jul_2015"=july_2015,"Aug_2015"=august_2015,"Sep_2015"=september_2015,"Oct_2015"=october_2015,"Dec_2015"=december_2015)

colnames <- c("Area", "N_HC", "P_HC", "N_HU", "P_HU", "N_OAPC", "P_OAPC", "N_OAPU", "P_OAPU","N_SOC", "P_SOC", "N_SOU", "P_SOU","N_BC",
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
national <-  data.frame(c(listOfDataframes[[i]][1,]))
national[national == "National"] <- names(listOfDataframes)[1]

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
  
  listOfDataframes[[i]] <- listOfDataframes[[i]][!(listOfDataframes[[i]]$Area %in% c("National")), ]
}

# Find total number of offences 
df_Total <- 

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
df_N_OAPC <- listOfDataframes[[1]][ , ("Area")]
for (i in 1:length(listOfDataframes)){
  df_N_OAPC <- cbind(df_N_OAPC, listOfDataframes[[i]][,6])
  }
colnames(df_N_OAPC) <- c("Area", "Jan_2014", "Feb_2014","Mar_2014","Apr_2014","May_2014","Jun_2014","Jul_2014","Aug_2014"
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


