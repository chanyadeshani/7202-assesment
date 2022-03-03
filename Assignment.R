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


# Change column names
colnames(march_2017) <- c("Area", "N_HC", "P_HC", "N_HU", "P_HU", "N_OAPC", "P_OAPC", "N_OAPU", "P_OAPU","N_SOC", "P_SOC", "N_SOU", "P_SOU","N_BC",
                          "P_BC","N_BU", "P_BU","N_RC","P_RC","N_RU","P_RU","N_THC","P_THC","N_THU","P_THU","N_FFC","P_FFC","N_FFU","P_FFU","N_CDC",
                          "P_CDC","N_CDU","P_CDU","N_DOC","P_DOC","N_DOU","P_DOU","N_POOC","P_POOC","N_POOU","P_POOU","N_OTHERC","P_OTHERC","N_OTHERU",
                          "P_OTHERU","N_MOC", "P_MOC","N_MOU","P_MOU","N_AFU","P_LMOU" ) 

colnames(february_2017) <- c("Area", "N_HC", "P_HC", "N_HU", "P_HU", "N_OAPC", "P_OAPC", "N_OAPU", "P_OAPU","N_SOC", "P_SOC", "N_SOU", "P_SOU","N_BC",
                             "P_BC","N_BU", "P_BU","N_RC","P_RC","N_RU","P_RU","N_THC","P_THC","N_THU","P_THU","N_FFC","P_FFC","N_FFU","P_FFU","N_CDC",
                             "P_CDC","N_CDU","P_CDU","N_DOC","P_DOC","N_DOU","P_DOU","N_POOC","P_POOC","N_POOU","P_POOU","N_OTHERC","P_OTHERC","N_OTHERU",
                             "P_OTHERU","N_MOC", "P_MOC","N_MOU","P_MOU","N_AFU","P_LMOU" )        


colnames(january_2017) <- c("Area", "N_HC", "P_HC", "N_HU", "P_HU", "N_OAPC", "P_OAPC", "N_OAPU", "P_OAPU","N_SOC", "P_SOC", "N_SOU", "P_SOU","N_BC",
                            "P_BC","N_BU", "P_BU","N_RC","P_RC","N_RU","P_RU","N_THC","P_THC","N_THU","P_THU","N_FFC","P_FFC","N_FFU","P_FFU","N_CDC",
                            "P_CDC","N_CDU","P_CDU","N_DOC","P_DOC","N_DOU","P_DOU","N_POOC","P_POOC","N_POOU","P_POOU","N_OTHERC","P_OTHERC","N_OTHERU",
                            "P_OTHERU","N_MOC", "P_MOC","N_MOU","P_MOU","N_AFU","P_LMOU" )

colnames(december_2016) <- c("Area", "N_HC", "P_HC", "N_HU", "P_HU", "N_OAPC", "P_OAPC", "N_OAPU", "P_OAPU","N_SOC", "P_SOC", "N_SOU", "P_SOU","N_BC",
                            "P_BC","N_BU", "P_BU","N_RC","P_RC","N_RU","P_RU","N_THC","P_THC","N_THU","P_THU","N_FFC","P_FFC","N_FFU","P_FFU","N_CDC",
                            "P_CDC","N_CDU","P_CDU","N_DOC","P_DOC","N_DOU","P_DOU","N_POOC","P_POOC","N_POOU","P_POOU","N_OTHERC","P_OTHERC","N_OTHERU",
                            "P_OTHERU","N_MOC", "P_MOC","N_MOU","P_MOU","N_AFU","P_LMOU" ) 

colnames(november_2016) <- c("Area", "N_HC", "P_HC", "N_HU", "P_HU", "N_OAPC", "P_OAPC", "N_OAPU", "P_OAPU","N_SOC", "P_SOC", "N_SOU", "P_SOU","N_BC",
                             "P_BC","N_BU", "P_BU","N_RC","P_RC","N_RU","P_RU","N_THC","P_THC","N_THU","P_THU","N_FFC","P_FFC","N_FFU","P_FFU","N_CDC",
                             "P_CDC","N_CDU","P_CDU","N_DOC","P_DOC","N_DOU","P_DOU","N_POOC","P_POOC","N_POOU","P_POOU","N_OTHERC","P_OTHERC","N_OTHERU",
                             "P_OTHERU","N_MOC", "P_MOC","N_MOU","P_MOU","N_AFU","P_LMOU" ) 

colnames(october_2016) <- c("Area", "N_HC", "P_HC", "N_HU", "P_HU", "N_OAPC", "P_OAPC", "N_OAPU", "P_OAPU","N_SOC", "P_SOC", "N_SOU", "P_SOU","N_BC",
                             "P_BC","N_BU", "P_BU","N_RC","P_RC","N_RU","P_RU","N_THC","P_THC","N_THU","P_THU","N_FFC","P_FFC","N_FFU","P_FFU","N_CDC",
                             "P_CDC","N_CDU","P_CDU","N_DOC","P_DOC","N_DOU","P_DOU","N_POOC","P_POOC","N_POOU","P_POOU","N_OTHERC","P_OTHERC","N_OTHERU",
                             "P_OTHERU","N_MOC", "P_MOC","N_MOU","P_MOU","N_AFU","P_LMOU" ) 

colnames(september_2016) <- c("Area", "N_HC", "P_HC", "N_HU", "P_HU", "N_OAPC", "P_OAPC", "N_OAPU", "P_OAPU","N_SOC", "P_SOC", "N_SOU", "P_SOU","N_BC",
                            "P_BC","N_BU", "P_BU","N_RC","P_RC","N_RU","P_RU","N_THC","P_THC","N_THU","P_THU","N_FFC","P_FFC","N_FFU","P_FFU","N_CDC",
                            "P_CDC","N_CDU","P_CDU","N_DOC","P_DOC","N_DOU","P_DOU","N_POOC","P_POOC","N_POOU","P_POOU","N_OTHERC","P_OTHERC","N_OTHERU",
                            "P_OTHERU","N_MOC", "P_MOC","N_MOU","P_MOU","N_AFU","P_LMOU" ) 

colnames(august_2016) <- c("Area", "N_HC", "P_HC", "N_HU", "P_HU", "N_OAPC", "P_OAPC", "N_OAPU", "P_OAPU","N_SOC", "P_SOC", "N_SOU", "P_SOU","N_BC",
                              "P_BC","N_BU", "P_BU","N_RC","P_RC","N_RU","P_RU","N_THC","P_THC","N_THU","P_THU","N_FFC","P_FFC","N_FFU","P_FFU","N_CDC",
                              "P_CDC","N_CDU","P_CDU","N_DOC","P_DOC","N_DOU","P_DOU","N_POOC","P_POOC","N_POOU","P_POOU","N_OTHERC","P_OTHERC","N_OTHERU",
                              "P_OTHERU","N_MOC", "P_MOC","N_MOU","P_MOU","N_AFU","P_LMOU" ) 
colnames(july_2016) <- c("Area", "N_HC", "P_HC", "N_HU", "P_HU", "N_OAPC", "P_OAPC", "N_OAPU", "P_OAPU","N_SOC", "P_SOC", "N_SOU", "P_SOU","N_BC",
                          "P_BC","N_BU", "P_BU","N_RC","P_RC","N_RU","P_RU","N_THC","P_THC","N_THU","P_THU","N_FFC","P_FFC","N_FFU","P_FFU","N_CDC",
                          "P_CDC","N_CDU","P_CDU","N_DOC","P_DOC","N_DOU","P_DOU","N_POOC","P_POOC","N_POOU","P_POOU","N_OTHERC","P_OTHERC","N_OTHERU",
                          "P_OTHERU","N_MOC", "P_MOC","N_MOU","P_MOU","N_AFU","P_LMOU" ) 

colnames(june_2016) <- c("Area", "N_HC", "P_HC", "N_HU", "P_HU", "N_OAPC", "P_OAPC", "N_OAPU", "P_OAPU","N_SOC", "P_SOC", "N_SOU", "P_SOU","N_BC",
                         "P_BC","N_BU", "P_BU","N_RC","P_RC","N_RU","P_RU","N_THC","P_THC","N_THU","P_THU","N_FFC","P_FFC","N_FFU","P_FFU","N_CDC",
                         "P_CDC","N_CDU","P_CDU","N_DOC","P_DOC","N_DOU","P_DOU","N_POOC","P_POOC","N_POOU","P_POOU","N_OTHERC","P_OTHERC","N_OTHERU",
                         "P_OTHERU","N_MOC", "P_MOC","N_MOU","P_MOU","N_AFU","P_LMOU" ) 

colnames(may_2016) <- c("Area", "N_HC", "P_HC", "N_HU", "P_HU", "N_OAPC", "P_OAPC", "N_OAPU", "P_OAPU","N_SOC", "P_SOC", "N_SOU", "P_SOU","N_BC",
                         "P_BC","N_BU", "P_BU","N_RC","P_RC","N_RU","P_RU","N_THC","P_THC","N_THU","P_THU","N_FFC","P_FFC","N_FFU","P_FFU","N_CDC",
                         "P_CDC","N_CDU","P_CDU","N_DOC","P_DOC","N_DOU","P_DOU","N_POOC","P_POOC","N_POOU","P_POOU","N_OTHERC","P_OTHERC","N_OTHERU",
                         "P_OTHERU","N_MOC", "P_MOC","N_MOU","P_MOU","N_AFU","P_LMOU" ) 

colnames(april_2016) <- c("Area", "N_HC", "P_HC", "N_HU", "P_HU", "N_OAPC", "P_OAPC", "N_OAPU", "P_OAPU","N_SOC", "P_SOC", "N_SOU", "P_SOU","N_BC",
                        "P_BC","N_BU", "P_BU","N_RC","P_RC","N_RU","P_RU","N_THC","P_THC","N_THU","P_THU","N_FFC","P_FFC","N_FFU","P_FFU","N_CDC",
                        "P_CDC","N_CDU","P_CDU","N_DOC","P_DOC","N_DOU","P_DOU","N_POOC","P_POOC","N_POOU","P_POOU","N_OTHERC","P_OTHERC","N_OTHERU",
                        "P_OTHERU","N_MOC", "P_MOC","N_MOU","P_MOU","N_AFU","P_LMOU" ) 

colnames(january_2016) <- c("Area", "N_HC", "P_HC", "N_HU", "P_HU", "N_OAPC", "P_OAPC", "N_OAPU", "P_OAPU","N_SOC", "P_SOC", "N_SOU", "P_SOU","N_BC",
                          "P_BC","N_BU", "P_BU","N_RC","P_RC","N_RU","P_RU","N_THC","P_THC","N_THU","P_THU","N_FFC","P_FFC","N_FFU","P_FFU","N_CDC",
                          "P_CDC","N_CDU","P_CDU","N_DOC","P_DOC","N_DOU","P_DOU","N_POOC","P_POOC","N_POOU","P_POOU","N_OTHERC","P_OTHERC","N_OTHERU",
                          "P_OTHERU","N_MOC", "P_MOC","N_MOU","P_MOU","N_AFU","P_LMOU" ) 

colnames(december_2015) <- c("Area", "N_HC", "P_HC", "N_HU", "P_HU", "N_OAPC", "P_OAPC", "N_OAPU", "P_OAPU","N_SOC", "P_SOC", "N_SOU", "P_SOU","N_BC",
                            "P_BC","N_BU", "P_BU","N_RC","P_RC","N_RU","P_RU","N_THC","P_THC","N_THU","P_THU","N_FFC","P_FFC","N_FFU","P_FFU","N_CDC",
                            "P_CDC","N_CDU","P_CDU","N_DOC","P_DOC","N_DOU","P_DOU","N_POOC","P_POOC","N_POOU","P_POOU","N_OTHERC","P_OTHERC","N_OTHERU",
                            "P_OTHERU","N_MOC", "P_MOC","N_MOU","P_MOU","N_AFU","P_LMOU" ) 

colnames(october_2015) <- c("Area", "N_HC", "P_HC", "N_HU", "P_HU", "N_OAPC", "P_OAPC", "N_OAPU", "P_OAPU","N_SOC", "P_SOC", "N_SOU", "P_SOU","N_BC",
                             "P_BC","N_BU", "P_BU","N_RC","P_RC","N_RU","P_RU","N_THC","P_THC","N_THU","P_THU","N_FFC","P_FFC","N_FFU","P_FFU","N_CDC",
                             "P_CDC","N_CDU","P_CDU","N_DOC","P_DOC","N_DOU","P_DOU","N_POOC","P_POOC","N_POOU","P_POOU","N_OTHERC","P_OTHERC","N_OTHERU",
                             "P_OTHERU","N_MOC", "P_MOC","N_MOU","P_MOU","N_AFU","P_LMOU" ) 

colnames(september_2015) <- c("Area", "N_HC", "P_HC", "N_HU", "P_HU", "N_OAPC", "P_OAPC", "N_OAPU", "P_OAPU","N_SOC", "P_SOC", "N_SOU", "P_SOU","N_BC",
                            "P_BC","N_BU", "P_BU","N_RC","P_RC","N_RU","P_RU","N_THC","P_THC","N_THU","P_THU","N_FFC","P_FFC","N_FFU","P_FFU","N_CDC",
                            "P_CDC","N_CDU","P_CDU","N_DOC","P_DOC","N_DOU","P_DOU","N_POOC","P_POOC","N_POOU","P_POOU","N_OTHERC","P_OTHERC","N_OTHERU",
                            "P_OTHERU","N_MOC", "P_MOC","N_MOU","P_MOU","N_AFU","P_LMOU" ) 

colnames(august_2015) <- c("Area", "N_HC", "P_HC", "N_HU", "P_HU", "N_OAPC", "P_OAPC", "N_OAPU", "P_OAPU","N_SOC", "P_SOC", "N_SOU", "P_SOU","N_BC",
                              "P_BC","N_BU", "P_BU","N_RC","P_RC","N_RU","P_RU","N_THC","P_THC","N_THU","P_THU","N_FFC","P_FFC","N_FFU","P_FFU","N_CDC",
                              "P_CDC","N_CDU","P_CDU","N_DOC","P_DOC","N_DOU","P_DOU","N_POOC","P_POOC","N_POOU","P_POOU","N_OTHERC","P_OTHERC","N_OTHERU",
                              "P_OTHERU","N_MOC", "P_MOC","N_MOU","P_MOU","N_AFU","P_LMOU" ) 

colnames(july_2015) <- c("Area", "N_HC", "P_HC", "N_HU", "P_HU", "N_OAPC", "P_OAPC", "N_OAPU", "P_OAPU","N_SOC", "P_SOC", "N_SOU", "P_SOU","N_BC",
                           "P_BC","N_BU", "P_BU","N_RC","P_RC","N_RU","P_RU","N_THC","P_THC","N_THU","P_THU","N_FFC","P_FFC","N_FFU","P_FFU","N_CDC",
                           "P_CDC","N_CDU","P_CDU","N_DOC","P_DOC","N_DOU","P_DOU","N_POOC","P_POOC","N_POOU","P_POOU","N_OTHERC","P_OTHERC","N_OTHERU",
                           "P_OTHERU","N_MOC", "P_MOC","N_MOU","P_MOU","N_AFU","P_LMOU" ) 

colnames(june_2015) <- c("Area", "N_HC", "P_HC", "N_HU", "P_HU", "N_OAPC", "P_OAPC", "N_OAPU", "P_OAPU","N_SOC", "P_SOC", "N_SOU", "P_SOU","N_BC",
                         "P_BC","N_BU", "P_BU","N_RC","P_RC","N_RU","P_RU","N_THC","P_THC","N_THU","P_THU","N_FFC","P_FFC","N_FFU","P_FFU","N_CDC",
                         "P_CDC","N_CDU","P_CDU","N_DOC","P_DOC","N_DOU","P_DOU","N_POOC","P_POOC","N_POOU","P_POOU","N_OTHERC","P_OTHERC","N_OTHERU",
                         "P_OTHERU","N_MOC", "P_MOC","N_MOU","P_MOU","N_AFU","P_LMOU" ) 

colnames(may_2015) <- c("Area", "N_HC", "P_HC", "N_HU", "P_HU", "N_OAPC", "P_OAPC", "N_OAPU", "P_OAPU","N_SOC", "P_SOC", "N_SOU", "P_SOU","N_BC",
                         "P_BC","N_BU", "P_BU","N_RC","P_RC","N_RU","P_RU","N_THC","P_THC","N_THU","P_THU","N_FFC","P_FFC","N_FFU","P_FFU","N_CDC",
                         "P_CDC","N_CDU","P_CDU","N_DOC","P_DOC","N_DOU","P_DOU","N_POOC","P_POOC","N_POOU","P_POOU","N_OTHERC","P_OTHERC","N_OTHERU",
                         "P_OTHERU","N_MOC", "P_MOC","N_MOU","P_MOU","N_AFU","P_LMOU" ) 

colnames(april_2015) <- c("Area", "N_HC", "P_HC", "N_HU", "P_HU", "N_OAPC", "P_OAPC", "N_OAPU", "P_OAPU","N_SOC", "P_SOC", "N_SOU", "P_SOU","N_BC",
                        "P_BC","N_BU", "P_BU","N_RC","P_RC","N_RU","P_RU","N_THC","P_THC","N_THU","P_THU","N_FFC","P_FFC","N_FFU","P_FFU","N_CDC",
                        "P_CDC","N_CDU","P_CDU","N_DOC","P_DOC","N_DOU","P_DOU","N_POOC","P_POOC","N_POOU","P_POOU","N_OTHERC","P_OTHERC","N_OTHERU",
                        "P_OTHERU","N_MOC", "P_MOC","N_MOU","P_MOU","N_AFU","P_LMOU" ) 
# Check column names
names(march_2017)  


view(february_2017)
view(march_2017)
# Remove percentage sign in the data set and convert to numeric
suppressWarnings(march_2017 <- data.frame(sapply(march_2017, function(x) as.numeric(gsub("%", "", x)))))
suppressWarnings(february_2017 <- data.frame(sapply(february_2017, function(x) as.numeric(gsub("%", "", x)))))
suppressWarnings(january_2017 <- data.frame(sapply(january_2017, function(x) as.numeric(gsub("%", "", x)))))
suppressWarnings(december_2016 <- data.frame(sapply(december_2016, function(x) as.numeric(gsub("%", "", x)))))
suppressWarnings(november_2016 <- data.frame(sapply(november_2016, function(x) as.numeric(gsub("%", "", x)))))
suppressWarnings(october_2016 <- data.frame(sapply(october_2016, function(x) as.numeric(gsub("%", "", x)))))
suppressWarnings(september_2016 <- data.frame(sapply(september_2016, function(x) as.numeric(gsub("%", "", x)))))
suppressWarnings(august_2016 <- data.frame(sapply(march_2017, function(x) as.numeric(gsub("%", "", x)))))
suppressWarnings(july_2016 <- data.frame(sapply(july_2016, function(x) as.numeric(gsub("%", "", x)))))
suppressWarnings(june_2016 <- data.frame(sapply(june_2016, function(x) as.numeric(gsub("%", "", x)))))
suppressWarnings(may_2016 <- data.frame(sapply(may_2016, function(x) as.numeric(gsub("%", "", x)))))
suppressWarnings(april_2016 <- data.frame(sapply(april_2016, function(x) as.numeric(gsub("%", "", x)))))
suppressWarnings(january_2016 <- data.frame(sapply(january_2016, function(x) as.numeric(gsub("%", "", x)))))
suppressWarnings(december_2015 <- data.frame(sapply(december_2015, function(x) as.numeric(gsub("%", "", x)))))
suppressWarnings(october_2015 <- data.frame(sapply(october_2015, function(x) as.numeric(gsub("%", "", x)))))
suppressWarnings(september_2015 <- data.frame(sapply(september_2015, function(x) as.numeric(gsub("%", "", x)))))
suppressWarnings(august_2015 <- data.frame(sapply(august_2015, function(x) as.numeric(gsub("%", "", x)))))
suppressWarnings(july_2015 <- data.frame(sapply(july_2015, function(x) as.numeric(gsub("%", "", x)))))
suppressWarnings(june_2015 <- data.frame(sapply(june_2015, function(x) as.numeric(gsub("%", "", x)))))
suppressWarnings(may_2015 <- data.frame(sapply(may_2015, function(x) as.numeric(gsub("%", "", x)))))
suppressWarnings(april_2015 <- data.frame(sapply(april_2015, function(x) as.numeric(gsub("%", "", x)))))

# Correct the first column data that was set to NA when converting data to NA
for (i in 1:nrow(march_2017)){
  
  march_2017[i,1]<-df1[i,1]
}
for (i in 1:nrow(february_2017)){
  
  february_2017[i,1]<-df1[i,1]
}
for (i in 1:nrow(january_2017)){
  
  january_2017[i,1]<-df1[i,1]
}
for (i in 1:nrow(december_2016)){
  
  december_2016[i,1]<-df1[i,1]
}
for (i in 1:nrow(november_2016)){
  
  november_2016[i,1]<-df1[i,1]
}
for (i in 1:nrow(october_2016)){
  
  october_2016[i,1]<-df1[i,1]
}
for (i in 1:nrow(september_2016)){
  
  september_2016[i,1]<-df1[i,1]
}
for (i in 1:nrow(august_2016)){
  
  august_2016[i,1]<-df1[i,1]
}
for (i in 1:nrow(july_2016)){
  
  july_2016[i,1]<-df1[i,1]
}
for (i in 1:nrow(june_2016)){
  
  june_2016[i,1]<-df1[i,1]
}
for (i in 1:nrow(may_2016)){
  
  may_2016[i,1]<-df1[i,1]
}
for (i in 1:nrow(april_2016)){
  
  april_2016[i,1]<-df1[i,1]
}
for (i in 1:nrow(january_2016)){
  
  january_2016[i,1]<-df1[i,1]
}
for (i in 1:nrow(december_2015)){
  
  december_2015[i,1]<-df1[i,1]
}
for (i in 1:nrow(october_2015)){
  
  october_2015[i,1]<-df1[i,1]
}
for (i in 1:nrow(september_2015)){
  
  september_2015[i,1]<-df1[i,1]
}
for (i in 1:nrow(august_2015)){
  
  august_2015[i,1]<-df1[i,1]
}
for (i in 1:nrow(july_2015)){
  
  july_2015[i,1]<-df1[i,1]
}
for (i in 1:nrow(june_2015)){
  
  june_2015[i,1]<-df1[i,1]
}
for (i in 1:nrow(may_2015)){
  
  may_2015[i,1]<-df1[i,1]
}
for (i in 1:nrow(april_2015)){
  
  april_2015[i,1]<-df1[i,1]
}
#copy national data to a new data frame
national_all = old[['A', 'C', 'D']].copy()
# Drop the row with National data
mar_2017 = march_2017[!(march_2017$Area %in% c("National")), ]
feb_2017 = february_2017[!(february_2017$Area %in% c("National")), ]
view(feb_2017)

view(mar_2017)

#bar graph for N_OAPC from April 2015 ~ March 2017



em <- ggplot(mar_2017, aes(N_OAPC, Area))
em + geom_col()

em <- ggplot(feb_2017, aes(N_OAPC, Area))
em + geom_col()

#ggplot(feb_2017,aes(x = N_MOU)) + geom_density(fill = "blue")


summary(feb_2017)

feb_2017
