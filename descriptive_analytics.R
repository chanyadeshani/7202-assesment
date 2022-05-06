# Bar graph for National values

# Offences against the person convictions
ggplot(nationaldf,aes(x=date,y=N_OAPC))+geom_bar(stat='identity') + ggtitle('National number of offences against the person convictions from July 2015 to March 2018') +
  xlab('Month') + ylab('Number of offences against the person convictions')+
  theme(plot.title = element_text(size = 10, face = 'bold'))

ggplot(nationaldf,aes(x=date,y=N_OAPU))+geom_bar(stat='identity') + ggtitle('National number of offences against the person unsuccessful from July 2015 to March 2018')+ 
  xlab('Month') + ylab('Number of offences against the person unsuccessful')+
  theme(plot.title = element_text(size = 10, face = 'bold'))

# Sexual offences
ggplot(nationaldf,aes(x=date,y=N_BC))+geom_bar(stat='identity') + ggtitle('National number of sexual offences convictions from July 2015 to March 2018')+ 
  xlab('Month') + ylab('Number of sexual offences convictions')+
  theme(plot.title = element_text(size = 10, face = 'bold'))

ggplot(nationaldf,aes(x=date,y=N_SOU))+geom_bar(stat='identity') + ggtitle('National number of sexual offences unsuccessful from July 2015 to March 2018')+ 
  xlab('Month') + ylab('Number of sexual offences unsuccessful')+
  theme(plot.title = element_text(size = 10, face = 'bold'))

# prepare the matrix for Bar plot selecting dataframe with only burglary data and convert it to a matrix and transpose the matrix

# Burglary

ggplot(nationaldf,aes(x=date,y=N_BC))+geom_bar(stat='identity', fill ='darkblue' ) + ggtitle('National number of burglary convictions from July 2015 to March 2018')+ 
  xlab('Month') + ylab('Number of burglary convictions')+scale_x_date(date_labels = "%b %y",date_breaks  ="3 month")+
  theme(plot.title = element_text(size = 10, face = 'bold'))

ggplot(nationaldf,aes(x=date,y=N_BC))+geom_bar(stat='identity', fill ='pink' ) + ggtitle('National number of burglary convictions from July 2015 to March 2018')+ 
  xlab('Month') + ylab('Number of burglary convictions')+scale_x_date(date_labels = "%b %y",date_breaks  ="3 month")+
  theme(plot.title = element_text(size = 10, face = 'bold'))
# can use the data.table package to melt the data into long format.

# prepare the matrix for Bar plot selecting dataframe with only burglary data and convert it to a matrix and transpose the matrix
mx <- t(as.matrix(nationaldf[,c('N_BC','N_BU')]))
# Date column read as character
colnames(mx) <- as.character(nationaldf$date)

colours = c('lightblue','orange')
# using ylim to give 30% space for the legend
barplot(mx,main= 'National number of burglary court outcomes from July 2015 to March 2018',xlab='Month', ylab='Number of cases',beside = TRUE, 
        col=colours,ylim=c(0,max(mx)*1.1))
# to add a box around the plot
box()
# add a legend
legend('topright',fill=colours,legend=c('Burglary Convictions','Burglary Unsuccessful'))
view(nationaldf)
# Robbery
ggplot(nationaldf,aes(x=date,y=N_RC))+geom_bar(stat='identity', fill ='darkblue' ) + ggtitle('National number of robbery convictions from July 2015 to March 2018')+ 
  xlab('Month') + ylab('Number of robbery convictions')+scale_x_date(date_labels = "%b %y",date_breaks  ="3 month")+
  theme(plot.title = element_text(size = 10, face = 'bold'))


ggplot(nationaldf,aes(x=date,y=N_RU))+geom_bar(stat='identity',fill ='pink') + ggtitle('National number of robbery unsuccessful from July 2015 to March 2018')+ 
  xlab('Month') + ylab('Number of robbery unsuccessful')+scale_x_date(date_labels = "%b %y",date_breaks  ="3 month")+
  theme(plot.title = element_text(size = 10, face = 'bold'))

shapiro.test(nationaldf$N_RC)
mx <- t(as.matrix(nationaldf[,c('N_RC','N_RU')]))
# Date column read as character
colnames(mx) <- as.character(nationaldf$date)

colours = c('lightblue','orange')
# using ylim to give 30% space for the legend
barplot(mx,main= 'National number of robbery court outcomes from July 2015 to March 2018',xlab='Month', ylab='Number of cases',beside = FALSE, 
        col=colours,ylim=c(0,max(mx)*1.35))
# to add a box around the plot
box()
# add a legend
legend('topright',fill=colours,legend=c('Robbery Convictions','Robbery Unsuccessful'))

# Theft And Handling
ggplot(nationaldf,aes(x=date,y=N_THC))+geom_bar(stat='identity', fill='darkblue') + ggtitle('National number of theft and handling convictions from July 2015 to March 2018')+ 
  xlab('Month') + ylab('Number of theft and handling convictions')+scale_x_date(date_labels = "%b %y",date_breaks  ="3 month")+
  theme(plot.title = element_text(size = 12, face = 'bold'))


ggplot(nationaldf,aes(x=date,y=N_THU))+geom_bar(stat='identity',fill ='pink') + ggtitle('National number of theft and handling unsuccessful from July 2015 to March 2018')+ 
  xlab('Month') + ylab('Number of theft and handling unsuccessful')+
  theme(plot.title = element_text(size = 10, face = 'bold'))



#prepare the matrix for barplot selecting data frame theft and handling data and convert it to a matrix and transpose the matrix
mx <- t(as.matrix(nationaldf[,c('N_THC','N_THU')]))
# Date column read as character
colnames(mx) <- as.character(nationaldf$date)

colours = c('lightblue','orange')
# using ylim to give pace for the legend
barplot(mx,main='National number of theft and handling court outcomes from July 2015 to March 2018',ylab='Number of cases', xlab='Month',beside = TRUE, 
        col=colours, ylim=c(0,max(mx)*1.1))
# to add a box around the plot
box()

# add a legend
legend('topright',fill=colours,legend=c('Theft and handling Convictions','Theft and handling Unsuccessful'))

# Criminal Damage
ggplot(nationaldf,aes(x=date,y=N_CDC))+geom_bar(stat='identity', fill = '#88ff00') + ggtitle('National number of criminal damage convictions from July 2015 to March 2018')+ 
  xlab('Month') + ylab('Number of criminal damage convictions')+scale_x_date(date_labels = "%b %y",date_breaks  ="3 month")+
  theme(plot.title = element_text(size = 10, face = 'bold'))

ggplot(nationaldf,aes(x=date,y=N_CDU))+geom_bar(stat='identity') + ggtitle('National number of criminal damage unsuccessful from July 2015 to March 2018')+ 
  xlab('Month') + ylab('Number of criminal damage unsuccessful')+scale_x_date(date_labels = "%b %y",date_breaks  ="3 month")+
  theme(plot.title = element_text(size = 10, face = 'bold'))

# Drugs Offences
ggplot(nationaldf,aes(x=date,y=N_DOC))+geom_bar(stat='identity', fill = 'darkgreen') + ggtitle('National number of drugs offences convictions from July 2015 to March 2018')+ 
  xlab('Month') + ylab('Number of drugs offences convictions')+scale_x_date(date_labels = "%b %y",date_breaks  ="3 month")+
  theme(plot.title = element_text(size = 10, face = 'bold'))

ggplot(nationaldf,aes(x=date,y=N_DOU))+geom_bar(stat='identity', fill = 'orange') + ggtitle('National number of drugs offences unsuccessful from July 2015 to March 2018')+ 
  xlab('Month') + ylab('Number of drugs offences unsuccessful')+
  theme(plot.title = element_text(size = 10, face = 'bold'))

# Public Order Offences
ggplot(nationaldf,aes(x=date,y=N_POOC))+geom_bar(stat='identity') + ggtitle('National number of public order offences convictions from July 2015 to March 2018')+ 
  xlab('Month') + ylab('Number of public order offences convictions')+
  theme(plot.title = element_text(size = 10, face = 'bold'))

ggplot(nationaldf,aes(x=date,y=N_POOU))+geom_bar(stat='identity') + ggtitle('National number of public order offencess unsuccessful from July 2015 to March 2018')+ 
  xlab('Month') + ylab('Number of public order offences unsuccessful')+
  theme(plot.title = element_text(size = 10, face = 'bold'))


# Motoring Offences
ggplot(nationaldf,aes(x=date,y=N_MOC))+geom_bar(stat='identity') + ggtitle('National number of motoring offences convictions from July 2015 to March 2018')+ 
  xlab('Month') + ylab('Number of motoring offences convictions')+scale_x_date(date_labels = "%b %y",date_breaks  ="3 month")+
  theme(plot.title = element_text(size = 10, face = 'bold'))


ggplot(nationaldf,aes(x=date,y=N_MOU))+geom_bar(stat='identity') + ggtitle('National number of motoring offences unsuccessful from July 2015 to March 2018')+ 
  xlab('Month') + ylab('Number of motoring offences unsuccessful')+
  theme(plot.title = element_text(size = 10, face = 'bold'))

# Admin Finalised Unsuccessful
ggplot(nationaldf,aes(x=date,y=N_AFU))+geom_bar(stat='identity') + ggtitle('National number of admin finalised unsuccessful from July 2015 to March 2018')+ 
  xlab('Month') + ylab('Number of admin finalised unsuccessful')+
  theme(plot.title = element_text(size = 10, face = 'bold'))

# Percentages of offences

# Homicide convictions
ggplot(nationaldf,aes(x=date,y=P_HC))+geom_bar(stat='identity') + ggtitle('National percentage of homicide convictions from July 2015 to March 2018') +
  xlab('Month') + ylab('Percentage of homicide convictions')

ggplot(nationaldf,aes(x=date,y=P_HU))+geom_bar(stat='identity') + ggtitle('National percentage of homicide unsuccessful from July 2015 to March 2018')+ 
  xlab('Month') + ylab('Percentage of homicide unsuccessful')

# Offences against the person convictions
ggplot(nationaldf,aes(x=date,y=P_OAPC))+geom_bar(stat='identity') + ggtitle('National percentage of offences against the person convictions from July 2015 to March 2018') +
  xlab('Month') + ylab('Percentage of offences against the person convictions')

ggplot(nationaldf,aes(x=date,y=P_OAPU))+geom_bar(stat='identity') + ggtitle('National percentage of offences against the person unsuccessful from July 2015 to March 2018')+ 
  xlab('Month') + ylab('Percentage of offences against the person unsuccessful')

# Sexual offences
ggplot(nationaldf,aes(x=date,y=P_SOC))+geom_bar(stat='identity') + ggtitle('National percentage of sexual offences convictions from July 2015 to March 2018')+ 
  xlab('Month') + ylab('Percentage of sexual offences convictions')

ggplot(nationaldf,aes(x=date,y=P_SOU))+geom_bar(stat='identity') + ggtitle('National percentage of sexual offences unsuccessful from July 2015 to March 2018')+ 
  xlab('Month') + ylab('Percentage of sexual offences unsuccessful')

# Burglary
ggplot(nationaldf,aes(x=date,y=P_BC))+geom_bar(stat='identity') + ggtitle('National percentage of burglary convictions unsuccessful from July 2015 to March 2018')+ 
  xlab('Month') + ylab('Percentage of burglary convictions')

ggplot(nationaldf,aes(x=date,y=P_BU))+geom_bar(stat='identity') + ggtitle('National percentage of burglary unsuccessful from July 2015 to March 2018')+ 
  xlab('Month') + ylab('Percentage of burglary unsuccessful')

# Robbery
ggplot(nationaldf,aes(x=date,y=P_RC))+geom_bar(stat='identity') + ggtitle('National percentage of robbery convictions from July 2015 to March 2018')+ 
  xlab('Month') + ylab('Percentage of robbery convictions')

ggplot(nationaldf,aes(x=date,y=P_RU))+geom_bar(stat='identity') + ggtitle('National percentage of robbery unsuccessful from July 2015 to March 2018')+ 
  xlab('Month') + ylab('Percentage of robbery unsuccessful')

# Theft And Handling
ggplot(nationaldf,aes(x=date,y=P_THC))+geom_bar(stat='identity') + ggtitle('National percentage of theft and handling convictions from July 2015 to March 2018')+ 
  xlab('Month') + ylab('Percentage of theft and handling convictions')

ggplot(nationaldf,aes(x=date,y=P_THU))+geom_bar(stat='identity') + ggtitle('National percentage of theft and handling unsuccessful from July 2015 to March 2018')+ 
  xlab('Month') + ylab('Percentage of theft and handling unsuccessful')

# Fraud And Forgery
ggplot(nationaldf,aes(x=date,y=P_FFC))+geom_bar(stat='identity') + ggtitle('National percentage of fraud and forgery convictions from July 2015 to March 2018')+ 
  xlab('Month') + ylab('Percentage of fraud and forgery convictions')


ggplot(nationaldf,aes(x=date,y=P_FFU))+geom_bar(stat='identity') + ggtitle('National percentage of fraud and forgery unsuccessful from July 2015 to March 2018')+ 
  xlab('Month') + ylab('Percentage of fraud and forgery unsuccessful')


# Criminal Damage
ggplot(nationaldf,aes(x=date,y=P_CDC))+geom_bar(stat='identity') + ggtitle('National percentage of criminal damage convictions from July 2015 to March 2018')+ 
  xlab('Month') + ylab('Percentage of criminal damage convictions')

ggplot(nationaldf,aes(x=date,y=P_CDU))+geom_bar(stat='identity') + ggtitle('National percentage of criminal damage unsuccessful from July 2015 to March 2018')+ 
  xlab('Month') + ylab('Percentage of criminal damage unsuccessful')

# Drugs Offences
ggplot(nationaldf,aes(x=date,y=P_DOC))+geom_bar(stat='identity') + ggtitle('National percentage of drugs offences convictions from July 2015 to March 2018')+ 
  xlab('Month') + ylab('Percentage of drugs offences convictions')

ggplot(nationaldf,aes(x=date,y=P_DOU))+geom_bar(stat='identity') + ggtitle('National percentage of drugs offences unsuccessful from July 2015 to March 2018')+ 
  xlab('Month') + ylab('Percentage of drugs offences unsuccessful')


# Public Order Offences
ggplot(nationaldf,aes(x=date,y=P_POOC))+geom_bar(stat='identity') + ggtitle('National percentage of public order offences convictions from July 2015 to March 2018')+ 
  xlab('Month') + ylab('Percentage of public order offences convictions')


ggplot(nationaldf,aes(x=date,y=P_POOU))+geom_bar(stat='identity') + ggtitle('National percentage of public order offencess unsuccessful from July 2015 to March 2018')+ 
  xlab('Month') + ylab('Percentage of public order offences unsuccessful')

# Other
ggplot(nationaldf,aes(x=date,y=P_OTHERC))+geom_bar(stat='identity') + ggtitle('National percentage of other convictions from July 2015 to March 2018')+ 
  xlab('Month') + ylab('Percentage of other convictions')

ggplot(nationaldf,aes(x=date,y=P_OTHERU))+geom_bar(stat='identity') + ggtitle('National percentage of other unsuccessful from July 2015 to March 2018')+ 
  xlab('Month') + ylab('Percentage of other unsuccessful')

# Motoring Offences
ggplot(nationaldf,aes(x=date,y=P_MOC))+geom_bar(stat='identity',fill = 'blue') + ggtitle('National percentage of motoring offences convictions from July 2015 to March 2018')+ 
  xlab('Month') + ylab('Percentage of motoring offences convictions')

ggplot(nationaldf,aes(x=date,y=P_MOU))+geom_bar(stat='identity') + ggtitle('National percentage of motoring offences unsuccessful from July 2015 to March 2018')+ 
  xlab('Month') + ylab('Percentage of motoring offences unsuccessful')


df1_july2015 <- full_df %>%  select(contains('N_RC'),'date','court')%>% group_by(date)  %>% filter(date == '2015-07-01')
ggplot(df1_july2015, aes(N_RC ))+geom_boxplot()+
  ggtitle('Number of robbery court cases in July 2015')+
  xlab('Number of robbery convictions')

df2_july2015 <- timeseries_df %>% group_by(court)  %>% filter(date == '2015-07-01')
ggplot(df2_july2015, aes(x= N_RC, y=court ))+geom_bar(stat='identity', fill = 'darkblue')+
  ggtitle('Number of robbery court cases in July 2015')+
  xlab('Court')+ylab('Number of robbery convictions')

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






