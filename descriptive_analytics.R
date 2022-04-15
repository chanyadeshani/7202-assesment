# Bar graph for National values
# Homicide convictions
p1 <- ggplot(nationaldf,aes(x=date,y=N_HC))+geom_bar(stat="identity") + ggtitle("National number of homicide convictions from July 2015 to March 2018") +
  xlab("Month") + ylab("Number of homicide convictions")+
  theme(plot.title = element_text(size = 10, face = "bold"))
ggsave(p1, filename = "Na_no_hc.png")

p2 <-ggplot(nationaldf,aes(x=date,y=N_HU))+geom_bar(stat="identity") + ggtitle("National number of homicide unsuccessful from July 2015 to March 2018")+ 
  xlab("Month") + ylab("Number of homicide unsuccessful")+
  theme(plot.title = element_text(size = 10, face = "bold"))
ggsave(p2, filename = "Na_no_hu.png")


# Offences against the person convictions
p3 <- ggplot(nationaldf,aes(x=date,y=N_OAPC))+geom_bar(stat="identity") + ggtitle("National number of offences against the person convictions from July 2015 to March 2018") +
  xlab("Month") + ylab("Number of offences against the person convictions")+
  theme(plot.title = element_text(size = 10, face = "bold"))
ggsave(p3, filename = "Na_no_oapc.png")

p4 <- ggplot(nationaldf,aes(x=date,y=N_OAPU))+geom_bar(stat="identity") + ggtitle("National number of offences against the person unsuccessful from July 2015 to March 2018")+ 
  xlab("Month") + ylab("Number of offences against the person unsuccessful")+
  theme(plot.title = element_text(size = 10, face = "bold"))
ggsave(p4, filename = "Na_no_oapu.png")

# Sexual offences
p5 <- ggplot(nationaldf,aes(x=date,y=N_SOC))+geom_bar(stat="identity") + ggtitle("National number of sexual offences convictions from July 2015 to March 2018")+ 
  xlab("Month") + ylab("Number of sexual offences convictions")+
  theme(plot.title = element_text(size = 10, face = "bold"))
ggsave(p5, filename = "Na_no_soc.png")

p6 <- ggplot(nationaldf,aes(x=date,y=N_SOU))+geom_bar(stat="identity") + ggtitle("National number of sexual offences unsuccessful from July 2015 to March 2018")+ 
  xlab("Month") + ylab("Number of sexual offences unsuccessful")+
  theme(plot.title = element_text(size = 10, face = "bold"))
ggsave(p6, filename = "Na_no_sou.png")

# Burglary
p7 <- ggplot(nationaldf,aes(x=date,y=N_BC))+geom_bar(stat="identity") + ggtitle("National number of burglary convictions unsuccessful from July 2015 to March 2018")+ 
  xlab("Month") + ylab("Number of burglary convictions")+
  theme(plot.title = element_text(size = 10, face = "bold"))
ggsave(p7, filename = "Na_no_bc.png")

p8 <- ggplot(nationaldf,aes(x=date,y=N_BU))+geom_bar(stat="identity") + ggtitle("National number of burglary unsuccessful from July 2015 to March 2018")+ 
  xlab("Month") + ylab("Number of burglary unsuccessful")+
  theme(plot.title = element_text(size = 10, face = "bold"))
ggsave(p8, filename = "Na_no_bu.png")

# prepare the matrix for Bar plot selecting dataframe with only burglary data and convert it to a matrix and transpose the matrix
mx <- t(as.matrix(nationaldf[,c("N_BC","N_BU")]))
# Date column read as character
colnames(mx) <- as.character(nationaldf$date)

colours = c("lightblue","orange")
# using ylim to give 30% space for the legend
barplot(mx,main= "National burglary court outcomes from July 2015 to March 2018",ylab="Month", xlab="Number of cases",beside = TRUE, 
        col=colours, ylim=c(0,max(mx)*1.3))
# to add a box around the plot
box()

# add a legend
legend("topright",fill=colours,legend=c("Burglary Convictions","Burglary Unsuccessful"))

# Robbery
p9 <- ggplot(nationaldf,aes(x=date,y=N_RC))+geom_bar(stat="identity") + ggtitle("National number of robbery convictions from July 2015 to March 2018")+ 
  xlab("Month") + ylab("Number of robbery convictions")+
  theme(plot.title = element_text(size = 10, face = "bold"))
ggsave(p9, filename = "Na_no_rc.png")

p10 <- ggplot(nationaldf,aes(x=date,y=N_RU))+geom_bar(stat="identity") + ggtitle("National number of robbery unsuccessful from July 2015 to March 2018")+ 
  xlab("Month") + ylab("Number of robbery unsuccessful")+
  theme(plot.title = element_text(size = 10, face = "bold"))
ggsave(p10, filename = "Na_no_ru.png")

# Theft And Handling
p11 <- ggplot(nationaldf,aes(x=date,y=N_THC))+geom_bar(stat="identity") + ggtitle("National number of theft and handling convictions from July 2015 to March 2018")+ 
  xlab("Month") + ylab("Number of theft and handling convictions")+
  theme(plot.title = element_text(size = 10, face = "bold"))
ggsave(p11, filename = "Na_no_tahc.png")

p12 <- ggplot(nationaldf,aes(x=date,y=N_THU))+geom_bar(stat="identity") + ggtitle("National number of theft and handling unsuccessful from July 2015 to March 2018")+ 
  xlab("Month") + ylab("Number of theft and handling unsuccessful")+
  theme(plot.title = element_text(size = 10, face = "bold"))
ggsave(p12, filename = "Na_no_tahu.png")
--------

#prepare the matrix for barplot selecting data frame theft and handling data and convert it to a matrix and transpose the matrix
mx <- t(as.matrix(nationaldf[,c("N_THC","N_THU")]))
# Date column read as character
colnames(mx) <- as.character(nationaldf$date)

colours = c("lightblue","orange")
# using ylim to give pace for the legend
barplot(mx,main="National number of theft and handling court outcomes from July 2015 to March 2018",ylab="Number of cases", xlab="Month",beside = TRUE, 
        col=colours, ylim=c(0,max(mx)*1.1))
# to add a box around the plot
box()

# add a legend
legend('topright',fill=colours,legend=c('Theft and handling Convictions','Theft and handling Unsuccessful'))
--------
# Fraud And Forgery
p13 <- ggplot(nationaldf,aes(x=date,y=N_FFC))+geom_bar(stat="identity") + ggtitle("National number of fraud and forgery convictions from July 2015 to March 2018")+ 
  xlab("Month") + ylab("Number of fraud and forgery convictions")+
  theme(plot.title = element_text(size = 10, face = "bold"))
ggsave(p13, filename = "Na_no_fafc.png")

p14 <- ggplot(nationaldf,aes(x=date,y=N_FFU))+geom_bar(stat="identity") + ggtitle("National number of fraud and forgery unsuccessful from July 2015 to March 2018")+ 
  xlab("Month") + ylab("Number of fraud and forgery unsuccessful")+
  theme(plot.title = element_text(size = 10, face = "bold"))
ggsave(p14, filename = "Na_no_fafu.png")

# Criminal Damage
p15 <- ggplot(nationaldf,aes(x=date,y=N_CDC))+geom_bar(stat="identity") + ggtitle("National number of criminal damage convictions from July 2015 to March 2018")+ 
  xlab("Month") + ylab("Number of criminal damage convictions")+
  theme(plot.title = element_text(size = 10, face = "bold"))
ggsave(p15, filename = "Na_no_cdc.png")

p16 <- ggplot(nationaldf,aes(x=date,y=N_CDU))+geom_bar(stat="identity") + ggtitle("National number of criminal damage unsuccessful from July 2015 to March 2018")+ 
  xlab("Month") + ylab("Number of criminal damage unsuccessful")+
  theme(plot.title = element_text(size = 10, face = "bold"))
ggsave(p16, filename = "Na_no_cdu.png")

# Drugs Offences
p17 <- ggplot(nationaldf,aes(x=date,y=N_DOC))+geom_bar(stat="identity") + ggtitle("National number of drugs offences convictions from July 2015 to March 2018")+ 
  xlab("Month") + ylab("Number of drugs offences convictions")+
  theme(plot.title = element_text(size = 10, face = "bold"))
ggsave(p17, filename = "Na_no_doc.png")

p18 <- ggplot(nationaldf,aes(x=date,y=N_DOU))+geom_bar(stat="identity") + ggtitle("National number of drugs offences unsuccessful from July 2015 to March 2018")+ 
  xlab("Month") + ylab("Number of drugs offences unsuccessful")+
  theme(plot.title = element_text(size = 10, face = "bold"))
ggsave(p18, filename = "Na_no_dou.png")

# Public Order Offences
p19 <- ggplot(nationaldf,aes(x=date,y=N_POOC))+geom_bar(stat="identity") + ggtitle("National number of public order offences convictions from July 2015 to March 2018")+ 
  xlab("Month") + ylab("Number of public order offences convictions")+
  theme(plot.title = element_text(size = 10, face = "bold"))
ggsave(p19, filename = "Na_no_pooc.png")

p20 <- ggplot(nationaldf,aes(x=date,y=N_POOU))+geom_bar(stat="identity") + ggtitle("National number of public order offencess unsuccessful from July 2015 to March 2018")+ 
  xlab("Month") + ylab("Number of public order offences unsuccessful")+
  theme(plot.title = element_text(size = 10, face = "bold"))
ggsave(p20, filename = "Na_no_poou.png")

# Other
p21 <- ggplot(nationaldf,aes(x=date,y=N_OTHERC))+geom_bar(stat="identity") + ggtitle("National number of other convictions from July 2015 to March 2018")+ 
  xlab("Month") + ylab("Number of other convictions")+
  theme(plot.title = element_text(size = 10, face = "bold"))
ggsave(p21, filename = "Na_no_otherc.png")

p22 <- ggplot(nationaldf,aes(x=date,y=N_OTHERU))+geom_bar(stat="identity") + ggtitle("National number of other unsuccessful from July 2015 to March 2018")+ 
  xlab("Month") + ylab("Number of other unsuccessful")+
  theme(plot.title = element_text(size = 10, face = "bold"))
ggsave(p22, filename = "Na_no_otheru.png")

# Motoring Offences
p23 <- ggplot(nationaldf,aes(x=date,y=N_MOC))+geom_bar(stat="identity") + ggtitle("National number of motoring offences convictions from July 2015 to March 2018")+ 
  xlab("Month") + ylab("Number of motoring offences convictions")+
  theme(plot.title = element_text(size = 10, face = "bold"))
ggsave(p23, filename = "Na_no_moc.png")

p24 <- ggplot(nationaldf,aes(x=date,y=N_MOU))+geom_bar(stat="identity") + ggtitle("National number of motoring offences unsuccessful from July 2015 to March 2018")+ 
  xlab("Month") + ylab("Number of motoring offences unsuccessful")+
  theme(plot.title = element_text(size = 10, face = "bold"))
ggsave(p24, filename = "Na_no_mou.png")

# Admin Finalised Unsuccessful
p25 <- ggplot(nationaldf,aes(x=date,y=N_AFU))+geom_bar(stat="identity") + ggtitle("National number of admin finalised unsuccessful from July 2015 to March 2018")+ 
  xlab("Month") + ylab("Number of admin finalised unsuccessful")+
  theme(plot.title = element_text(size = 10, face = "bold"))
ggsave(p25, filename = "Na_no_afu.png")

# Percentages of offences

# Homicide convictions
p1_p <- ggplot(nationaldf,aes(x=date,y=P_HC))+geom_bar(stat="identity") + ggtitle("National percentage of homicide convictions from July 2015 to March 2018") +
  xlab("Month") + ylab("Percentage of homicide convictions")
ggsave(p1_p, filename = "Na_p_hc.png")

p2_p <-ggplot(nationaldf,aes(x=date,y=P_HU))+geom_bar(stat="identity") + ggtitle("National percentage of homicide unsuccessful from July 2015 to March 2018")+ 
  xlab("Month") + ylab("Percentage of homicide unsuccessful")
ggsave(p2_p, filename = "Na_p_hu.png")

# Offences against the person convictions
p3_p <- ggplot(nationaldf,aes(x=date,y=P_OAPC))+geom_bar(stat="identity") + ggtitle("National percentage of offences against the person convictions from July 2015 to March 2018") +
  xlab("Month") + ylab("Percentage of offences against the person convictions")
ggsave(p3_p, filename = "Na_p_oapc.png")

p4_p <- ggplot(nationaldf,aes(x=date,y=P_OAPU))+geom_bar(stat="identity") + ggtitle("National percentage of offences against the person unsuccessful from July 2015 to March 2018")+ 
  xlab("Month") + ylab("Percentage of offences against the person unsuccessful")
ggsave(p4_p, filename = "Na_p_oapu.png")

# Sexual offences
p5_p <- ggplot(nationaldf,aes(x=date,y=P_SOC))+geom_bar(stat="identity") + ggtitle("National percentage of sexual offences convictions from July 2015 to March 2018")+ 
  xlab("Month") + ylab("Percentage of sexual offences convictions")
ggsave(p5_p, filename = "Na_p_soc.png")

p6_p <- ggplot(nationaldf,aes(x=date,y=P_SOU))+geom_bar(stat="identity") + ggtitle("National percentage of sexual offences unsuccessful from July 2015 to March 2018")+ 
  xlab("Month") + ylab("Percentage of sexual offences unsuccessful")
ggsave(p6_p, filename = "Na_p_sou.png")

# Burglary
p7_p <- ggplot(nationaldf,aes(x=date,y=P_BC))+geom_bar(stat="identity") + ggtitle("National percentage of burglary convictions unsuccessful from July 2015 to March 2018")+ 
  xlab("Month") + ylab("Percentage of burglary convictions")
ggsave(p7_p, filename = "Na_p_bc.png")

p8_p <- ggplot(nationaldf,aes(x=date,y=P_BU))+geom_bar(stat="identity") + ggtitle("National percentage of burglary unsuccessful from July 2015 to March 2018")+ 
  xlab("Month") + ylab("Percentage of burglary unsuccessful")
ggsave(p8_p, filename = "Na_p_bu.png")

# Robbery
p9_p <- ggplot(nationaldf,aes(x=date,y=P_RC))+geom_bar(stat="identity") + ggtitle("National percentage of robbery convictions from July 2015 to March 2018")+ 
  xlab("Month") + ylab("Percentage of robbery convictions")
ggsave(p9_p, filename = "Na_p_rc.png")

p10_p <- ggplot(nationaldf,aes(x=date,y=P_RU))+geom_bar(stat="identity") + ggtitle("National percentage of robbery unsuccessful from July 2015 to March 2018")+ 
  xlab("Month") + ylab("Percentage of robbery unsuccessful")
ggsave(p10_p, filename = "Na_p_ru.png")

# Theft And Handling
p11_p <- ggplot(nationaldf,aes(x=date,y=P_THC))+geom_bar(stat="identity") + ggtitle("National percentage of theft and handling convictions from July 2015 to March 2018")+ 
  xlab("Month") + ylab("Percentage of theft and handling convictions")
ggsave(p11, filename = "Na_p_tahc.png")

p12_p <- ggplot(nationaldf,aes(x=date,y=P_THU))+geom_bar(stat="identity") + ggtitle("National percentage of theft and handling unsuccessful from July 2015 to March 2018")+ 
  xlab("Month") + ylab("Percentage of theft and handling unsuccessful")
ggsave(p12_p, filename = "Na_p_tahu.png")

# Fraud And Forgery
p13_p <- ggplot(nationaldf,aes(x=date,y=P_FFC))+geom_bar(stat="identity") + ggtitle("National percentage of fraud and forgery convictions from July 2015 to March 2018")+ 
  xlab("Month") + ylab("Percentage of fraud and forgery convictions")
ggsave(p13_p, filename = "Na_p_fafc.png")

p14_p <- ggplot(nationaldf,aes(x=date,y=P_FFU))+geom_bar(stat="identity") + ggtitle("National percentage of fraud and forgery unsuccessful from July 2015 to March 2018")+ 
  xlab("Month") + ylab("Percentage of fraud and forgery unsuccessful")
ggsave(p14_p, filename = "Na_p_fafu.png")

# Criminal Damage
p15_p <- ggplot(nationaldf,aes(x=date,y=P_CDC))+geom_bar(stat="identity") + ggtitle("National percentage of criminal damage convictions from July 2015 to March 2018")+ 
  xlab("Month") + ylab("Percentage of criminal damage convictions")
ggsave(p15_p, filename = "Na_p_cdc.png")

p16_p <- ggplot(nationaldf,aes(x=date,y=P_CDU))+geom_bar(stat="identity") + ggtitle("National percentage of criminal damage unsuccessful from July 2015 to March 2018")+ 
  xlab("Month") + ylab("Percentage of criminal damage unsuccessful")
ggsave(p16_p, filename = "Na_p_cdu.png")

# Drugs Offences
p17_p <- ggplot(nationaldf,aes(x=date,y=P_DOC))+geom_bar(stat="identity") + ggtitle("National percentage of drugs offences convictions from July 2015 to March 2018")+ 
  xlab("Month") + ylab("Percentage of drugs offences convictions")
ggsave(p17_p, filename = "Na_p_doc.png")

p18_p <- ggplot(nationaldf,aes(x=date,y=P_DOU))+geom_bar(stat="identity") + ggtitle("National percentage of drugs offences unsuccessful from July 2015 to March 2018")+ 
  xlab("Month") + ylab("Percentage of drugs offences unsuccessful")
ggsave(p18_p, filename = "Na_p_dou.png")

# Public Order Offences
p19_p <- ggplot(nationaldf,aes(x=date,y=P_POOC))+geom_bar(stat="identity") + ggtitle("National percentage of public order offences convictions from July 2015 to March 2018")+ 
  xlab("Month") + ylab("Percentage of public order offences convictions")
ggsave(p19_p, filename = "Na_p_pooc.png")

p20_p <- ggplot(nationaldf,aes(x=date,y=P_POOU))+geom_bar(stat="identity") + ggtitle("National percentage of public order offencess unsuccessful from July 2015 to March 2018")+ 
  xlab("Month") + ylab("Percentage of public order offences unsuccessful")
ggsave(p20_p, filename = "Na_p_poou.png")

# Other
p21_p <- ggplot(nationaldf,aes(x=date,y=P_OTHERC))+geom_bar(stat="identity") + ggtitle("National percentage of other convictions from July 2015 to March 2018")+ 
  xlab("Month") + ylab("Percentage of other convictions")
ggsave(p21_p, filename = "Na_p_otherc.png")

p22_p <- ggplot(nationaldf,aes(x=date,y=P_OTHERU))+geom_bar(stat="identity") + ggtitle("National percentage of other unsuccessful from July 2015 to March 2018")+ 
  xlab("Month") + ylab("Percentage of other unsuccessful")
ggsave(p22_p, filename = "Na_p_otheru.png")

# Motoring Offences
p23_p <- ggplot(nationaldf,aes(x=date,y=P_MOC))+geom_bar(stat="identity") + ggtitle("National percentage of motoring offences convictions from July 2015 to March 2018")+ 
  xlab("Month") + ylab("Percentage of motoring offences convictions")
ggsave(p23_p, filename = "Na_p_moc.png")

p24_p <- ggplot(nationaldf,aes(x=date,y=P_MOU))+geom_bar(stat="identity") + ggtitle("National percentage of motoring offences unsuccessful from July 2015 to March 2018")+ 
  xlab("Month") + ylab("Percentage of motoring offences unsuccessful")
ggsave(p24, filename = "Na_p_mou.png")

# Admin Finalised Unsuccessful
p25_p <- ggplot(nationaldf,aes(x=date,y=P_AFU))+geom_bar(stat="identity") + ggtitle("National percentage of admin finalised unsuccessful from July 2015 to March 2018")+ 
  xlab("Month") + ylab("Percentage of admin finalised unsuccessful")
ggsave(p25, filename = "Na_p_afu.png")


df_june2015 <- full_df %>% group_by(date)  %>% filter(date == "2015-07-01")
ggplot(df_jan2017, aes(N_THC ))+geom_boxplot()+
  ggtitle("Number of theft and handling convictions in June 2015")+
  xlab("Number of theft and handling convictions")






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

df_bfs <-filter(full_df, court == "Bedfordshire")
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
p <- full_df %>%
  ggplot( aes(x=Date, y=P_HC, group=court, fill=court)) +
  geom_area(stat='identity') +
  ggtitle("Percentage of Homicide Convictions from 2014 to 2015") +
  scale_x_date(limits = as.Date(c("2014-01-01", "2015-12-01")), 
               labels = scales::label_date_short())+ 
  theme(
    legend.position="center",
  ) + facet_wrap(~court, scale="free_y") 


ggsave(p, filename = "Percentage of Homicide Convictions.png")
names(full_df)
unique(full_df$Police)
names(df1)
mean_vec <- vector()
median_vec <- vector()
sd_vec <- vector()
x <- vector()
for (i in 2:(ncol(full_df) -1)){
  x[i] <- colnames(full_df)[i]
  mean_vec[i] <- mean(full_df[ ,i],na.rm=TRUE)
  median_vec[i] <- median(full_df[ ,i],na.rm=TRUE)
  sd_vec[i] <- sd(full_df[ ,i],na.rm=TRUE)
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




