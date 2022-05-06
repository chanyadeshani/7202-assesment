library(ggpubr)
library(factoextra)
library(plyr) 
library(ggplot2) 
library(cluster) 
library(lattice) 
library(gridExtra)

# Create a subset of columns with number of drug offence court cases
drug_cluster_df = timeseries_df[, c(33,35,50,51,52,53)]

# Calculate total drug court cases
drug_cluster_df$total_cases <- drug_cluster_df$N_DOC + drug_cluster_df$N_DOU

head(drug_cluster_df)
colnames(drug_cluster_df)

# Plot total cases and court
ggplot(drug_cluster_df,aes(x=total_cases,y=court))+geom_bar(stat='identity',fill='darkblue') + ggtitle('Total drug offence court cases from July 2015 to March 2018')+ 
  xlab('Total cases') + ylab('Court')+
  theme(plot.title = element_text(size = 10, face = 'bold'))

ggplot(drug_cluster_df,aes(x=date, y = total_cases))+geom_bar(stat='identity',fill='darkblue') + ggtitle('Total drug offence court cases from July 2015 to March 2018')+ 
  xlab('Month') + ylab('Total cases')+scale_x_date(date_labels = "%b %y",date_breaks  ="3 month")+
  theme(plot.title = element_text(size = 10, face = 'bold'))

ggplot(drug_cluster_df,aes(x=month,y=total_cases))+geom_bar(stat='identity',fill='darkblue') + ggtitle('Total drug offence court cases from July 2015 to March 2018')+ 
  xlab('Month') + ylab('Mean total cases')+scale_x_continuous(breaks = c(0,2,4,6,8,10,12))+
  theme(plot.title = element_text(size = 10, face = 'bold'))

month_count = aggregate(data.frame(count = month), list(value = month), length)
drug_cluster_df$mean_total_cases <- drug_cluster_df$total_cases

# for-loop over rows
for(i in 1:nrow(drug_cluster_df)) {       
  drug_cluster_df[i,8] <- drug_cluster_df[i,8 ] / month_count[drug_cluster_df[i,5],2]
}

# Plot total cases and month
ggplot(drug_cluster_df,aes(x=month,y=mean_total_cases))+geom_bar(stat='identity',fill='darkblue') + ggtitle('Total drug offence court cases from July 2015 to March 2018')+ 
  xlab('Month') + ylab('Mean total cases')+scale_x_continuous(breaks = c(0,2,4,6,8,10,12))+
  theme(plot.title = element_text(size = 10, face = 'bold'))

# Histogram for month
month = drug_cluster_df$month
hist(month)

# Total drug offences stores in a vector
total_drug_offence_cases = drug_cluster_df$total_cases
summary(total_drug_offence_cases)
sd(total_drug_offence_cases)
# Histogram of the total cout cases
hist(total_drug_offence_cases)

# scatter plot matrix
splom(~drug_cluster_df[c(5,6,7)], groups=NULL, data=drug_cluster_df,axis.line.tck = 0, axis.text.alpha = 0)

# Select columns for clustering
drug_df_num <- drug_cluster_df[, c('total_cases', 'month', 'court_num')]

#drug_df_num$motor_num <- ifelse(drug_cluster_df$P_DOC < 80, 1, ifelse(drug_cluster_df$P_DOC  < 90, 2, 3))

wss <- numeric(15)
for (k in 1:15)
  wss[k] <- kmeans(scale(drug_df_num, center = TRUE, scale = TRUE),centers = k,nstart = 15)$tot.withinss

plot(1:15, wss, type ='b', xlab = 'Number of clusters', ylab = 'Within number of squares')

res.km4 <- kmeans(scale(drug_df_num, center = TRUE, scale = TRUE),centers = 4,nstart = 25)

fviz_cluster(res.km4, data = drug_df_num,
             palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#2E00AF"),
             geom = "point",
             ellipse.type = "convex",
             ggtheme = theme_bw()
)

res.km4$centers
res.km4$tot.withinss
res.km4$betweenss

res.km3 <- kmeans(scale(drug_df_num, center = TRUE, scale = TRUE),centers = 3,nstart = 25)
fviz_cluster(res.km3, data = drug_df_num,
             palette = c("#2E9FDF", "#00AFBB", "#E7B800"),
             geom = "point",
             ellipse.type = "convex",
             ggtheme = theme_bw()
)

res.km3$centers
res.km3$tot.withinss
res.km3$betweenss

res.km5 <- kmeans(scale(drug_df_num, center = TRUE, scale = TRUE),centers = 5,nstart = 25)
fviz_cluster(res.km3, data = drug_df_num,
             palette = c("#2E9FDF", "#00AFBB", "#E7B800","#009F00","#2E00DF"),
             geom = "point",
             ellipse.type = "convex",
             ggtheme = theme_bw()
)

res.km5$centers
res.km5$tot.withinss
res.km5$betweenss

sd_court <- sd(drug_df_num$court_num)
m_court <- mean(drug_df_num$court_num)
# Court number for cluster center one
m_court + (res.km4$centers[1,3]*sd_court)
# Court number for cluster center two
m_court + (res.km4$centers[2,3]*sd_court)
# Court number for cluster center three
m_court + (res.km4$centers[3,3]*sd_court)
# Court number for cluster center four
m_court + (res.km4$centers[4,3]*sd_court)

sd_month <- sd(drug_df_num$month)
m_month <- mean(drug_df_num$month)
# Month for cluster center one
m_month + (res.km4$centers[1,2]*sd_month)
# Month for cluster center two
m_month + (res.km4$centers[2,2]*sd_month)
# Month for cluster center three
m_month + (res.km4$centers[3,2]*sd_month)
# Month for cluster center four
m_month + (res.km4$centers[4,2]*sd_month)

sd_total_cases <- sd(drug_df_num$total_cases)
m_total_cases <- mean(drug_df_num$total_cases)
# Total cases for cluster center one
m_total_cases + (res.km4$centers[1,1]*sd_total_cases)
# Total cases for cluster center two
m_total_cases + (res.km4$centers[2,1]*sd_total_cases)
# Total cases for cluster center three
m_total_cases + (res.km4$centers[3,1]*sd_total_cases)
# Total cases for cluster center four
m_total_cases + (res.km4$centers[4,1]*sd_total_cases)
view(drug_cluster_df)



