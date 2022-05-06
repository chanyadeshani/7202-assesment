library(party)
library(randomForest)
library(rpart)
library(rattle)
library(tree)
library(class)
library(caret)


# Create a subset of columns related to motoring offence
motor_offence_df = timeseries_df[, c(45,46,47,50,51)]

# Calculate total motoring court cases
motor_offence_df$total_motoring <- motor_offence_df$N_MOC+motor_offence_df$N_MOU

#Histogram of total motoring
success_rate_motoring_offence <- motor_offence_df$P_MOC
hist(success_rate_motoring_offence)
summary(success_rate_motoring_offence)
sd(success_rate_motoring_offence)

# Extracting the month from the date
motor_offence_df <- cbind(motor_offence_df,month = c(month(as.POSIXlt(motor_offence_df$date, format="%d%b%Y"))))
motor_offence_df$motor_cat <- ifelse(motor_offence_df$P_MOC < 80, 'average', ifelse(motor_offence_df$P_MOC  < 90, 'high', 'very high'))
motor_offence_df$motor_num <- ifelse(motor_offence_df$P_MOC < 80, 1, ifelse(motor_offence_df$P_MOC  < 90, 2, 3))

view(motor_offence_df)

ggplot(motor_offence_df, aes(x=month, y=court, shape=motor_cat, color=motor_cat,size = motor_cat)) +
  geom_point()+
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10,11,12)) +
  scale_shape_manual(values=c(3, 16, 17))+ 
  scale_color_manual(values=c('#990000','#E69F00', '#56B4E9'))+
  scale_size_manual(values=c(2,3,4))+
  theme(legend.position="right")+
  ggtitle("Motoring court case succcess rate")

motor_offence_df$court <- as.numeric(as.factor(motor_offence_df$court))
motor_offence_df %>%
  ggplot(aes(x=motor_cat, y=total_motoring, fill = motor_cat)) +
  geom_boxplot() +theme_bw()+
  ggtitle("Box Plot")


## 80% of the sample size
smp_size <- floor(0.8 * nrow(motor_offence_df))

## set the seed to make the partition reproducible
set.seed(70)
train_ind <- sample(seq_len(nrow(motor_offence_df)), size = smp_size)

train <- motor_offence_df[train_ind, ]
test <- motor_offence_df[-train_ind, ]

tree1 <- rpart(formula = motor_cat ~ court + month+total_motoring, data = train, method = "class", 
              parms = list(split = "information"))
tree1
# Plot decision tree
fancyRpartPlot(tree1, main="Motoring court cases")
summary((tree1))

predict_unseen1 <-predict(tree, test, type = 'class')

cofused_mat1 <- table(test$motor_cat, predict_unseen1)
cofused_mat1
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(cofused_mat1)

tree2 <- rpart(formula = motor_cat ~ court + month+total_motoring, data = train, method = "class", 
              parms = list(split = "information"), control = rpart.control(minsplit = 90))

tree2
# Plot decision tree
fancyRpartPlot(tree2, main="Motoring court cases")
summary((tree2))

predict_unseen2 <-predict(tree2, test, type = 'class')

cofused_mat2 <- table(test$motor_cat, predict_unseen2)
cofused_mat2

accuracy(cofused_mat2)
plotcp(tree2)


