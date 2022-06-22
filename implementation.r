library(readxl)

data <- read_excel("C:\\Users\\argir\\Υπολογιστής\\BSc_Bussiness_Analytics\\Statistics 2\\project2\\project_I_2021-2022.xls")
View(data)

#Checking for missing values
missing_val<-data.frame(apply(data,2,function(x){sum(is.na(x))}))
missing_val

str(data)

#change the data type of the columns
data$age <- as.numeric(data$age)
data$job <- as.factor(data$job)
data$marital <- as.factor(data$marital)
data$education <- as.factor(data$education)
data$default <- as.factor(data$default)
data$housing <- as.factor(data$housing)
data$loan <- as.factor(data$loan)
data$contact <- as.factor(data$contact)
data$month <- as.factor(data$month)
data$day_of_week <- as.factor(data$day_of_week)
data$duration <- as.numeric(data$duration)
data$campaign <- as.numeric(data$campaign)
data$pdays <- as.numeric(data$pdays)
data$previous <- as.numeric(data$previous)
data$poutcome <- as.factor(data$poutcome)
data$emp.var.rate <- as.numeric(data$emp.var.rate)
data$cons.price.idx <- as.numeric(data$cons.price.idx)
data$euribor3m <- as.numeric(data$euribor3m)
data$nr.employed <- as.numeric(data$nr.employed)
data$SUBSCRIBED <- as.factor(data$SUBSCRIBED)

#replace the value 999 with the value 0 in the column pdays
data$pdays <- replace(data$pdays, data$pdays==999, 0)
View(data)


#scale the datadet
scale_data <- data
scale_data[] <- lapply(scale_data, function(x) if(is.numeric(x)){scale(x, center=TRUE, scale=TRUE)} else x)
View(scale_data)


#Split the data in train, test and validation
set.seed(12)
splitSample <- sample(1:3, size=nrow(scale_data), prob=c(0.7,0.2,0.1), replace = TRUE)
train_scale <- scale_data[splitSample==1,]
validation <- scale_data[splitSample==2,]
test_scale <- scale_data[splitSample==3,]
View(train_scale)
View(test_scale)
View(validation)


################################
########   MODELING   ##########
################################


##########################################################
################  Naive Bayes  ###########################
##########################################################
library(caret)
library(pROC)

control <- trainControl(method='repeatedcv', 
                        number=10, 
                        repeats=3,
                        classProbs= TRUE,
                        summaryFunction = twoClassSummary,
                        search = "grid")

naive_bayes_model <- train(SUBSCRIBED ~ .,
                           data = train_scale,
                           method = "naive_bayes",
                           trControl = control,
                           metric = "ROC")  

naive_bayes_model
plot(naive_bayes_model)

naive_bayes_pred <- predict(naive_bayes_model, test_scale)
confusionMatrix(naive_bayes_pred, test_scale$SUBSCRIBED, mode = "prec_recall", positive = "yes")


naive_bayes_pred_validation <- predict(naive_bayes_model, validation)
confusionMatrix(naive_bayes_pred_validation, validation$SUBSCRIBED, mode = "prec_recall", positive = "yes")

#Plot Variable performance
#naive_bayes_model_var_importance <- varImp(naive_bayes_model)
#naive_bayes_model_var_importance
#plot(naive_bayes_model_var_importance)


#I tried to tune the parameters but the result was the same
control <- trainControl(method='repeatedcv', number=10, repeats=3, classProbs= TRUE,
                        summaryFunction = twoClassSummary, search = "grid")

naive_bayes_model <- train(SUBSCRIBED ~ .,
                           data = train_scale,
                           method = "naive_bayes",
                           trControl = control,
                           metric = "ROC",
                           tuneGrid=expand.grid(usekernel=c(TRUE,FALSE), laplace=c(0,1,2),adjust=c(0,0.5,1.0)))


##########################################################
################    ΚΝΝ    ###############################
##########################################################

#knn 1st model
control <- trainControl(method='repeatedcv', 
                        number=10, 
                        repeats=3,
                        classProbs= TRUE,
                        summaryFunction = twoClassSummary,
                        search = "grid")

knn_model <- train(SUBSCRIBED ~ .,
                   data = train_scale,
                   method = "knn",
                   trControl = control,
                   metric = "ROC")

knn_model
knn_pred <- predict(knn_model, test_scale)
confusionMatrix(knn_pred, test_scale$SUBSCRIBED, mode = "prec_recall", positive = "yes")
plot(knn_model)

knn_pred_validation <- predict(knn_model, validation)
confusionMatrix(knn_pred_validation, validation$SUBSCRIBED, mode = "prec_recall", positive = "yes")

#knn 2nd model
control <- trainControl(method='repeatedcv', 
                        number=10, 
                        repeats=3,
                        classProbs= TRUE,
                        summaryFunction = twoClassSummary,
                        search = "grid")

knn_model2 <- train(SUBSCRIBED ~ .,
                    data = train_scale,
                    method = "knn",
                    tuneLength = 12,
                    preProc = c("center", "scale"),
                    trControl = control,
                    metric = "ROC",
                    tuneGrid = expand.grid(k = 9:20))

knn_model2
knn_pred2 <- predict(knn_model2, validation)
confusionMatrix(knn_pred2, validation$SUBSCRIBED, mode = "prec_recall", positive = "yes")

plot(knn_model2)

#knn 3rd model
control <- trainControl(method='repeatedcv', 
                        number=10, 
                        repeats=3,
                        classProbs= TRUE,
                        summaryFunction = twoClassSummary,
                        search = "grid")

knn_model3 <- train(SUBSCRIBED ~ .,
                    data = train_scale,
                    method = "knn",
                    tuneLength = 20,
                    preProc = c("center", "scale"),
                    trControl = control,
                    metric = "ROC",
                    tuneGrid = expand.grid(k = 9:20))
knn_model3
knn_pred3 <- predict(knn_model3, validation)
confusionMatrix(knn_pred3, validation$SUBSCRIBED, mode = "prec_recall", positive = "yes")

plot(knn_model3)

###################################################################
#######################    Random Forest   ########################
###################################################################

#random forest 1st model

library(randomForest)
control <- trainControl(method='repeatedcv', 
                        number=10, 
                        repeats=3,
                        classProbs= TRUE,
                        summaryFunction = twoClassSummary,
                        search = "grid")

random_forest_model <- train(SUBSCRIBED ~ .,
                             data = train_scale,
                             method = "rf",
                             trControl = control,
                             metric = "ROC")

random_forest_model
plot(random_forest_model)

random_forest_pred1 <- predict(random_forest_model, test_scale)
confusionMatrix(random_forest_pred1, test_scale$SUBSCRIBED, mode = "prec_recall", positive = "yes")

random_forest_pred_validation <- predict(random_forest_model, validation)
confusionMatrix(random_forest_pred_validation, validation$SUBSCRIBED, mode = "prec_recall", positive = "yes")

plot(random_forest_model)
varImpPlot(random_forest_model)
importance(random_forest_model)

##random forest 2nd model
library(randomForest)
control <- trainControl(method='repeatedcv', 
                        number=10, 
                        repeats=3,
                        classProbs= TRUE,
                        summaryFunction = twoClassSummary,
                        search = "random")

random_forest_model <- train(SUBSCRIBED ~ .,
                             data = train_scale,
                             method = "rf",
                             trControl = control,
                             metric = "ROC",
                             importance = TRUE)

random_forest_model
plot(random_forest_model)

random_forest_pred1 <- predict(random_forest_model, test_scale)
confusionMatrix(random_forest_pred1, test_scale$SUBSCRIBED, mode = "prec_recall", positive = "yes")

random_forest_pred_validation <- predict(random_forest_model, validation)
confusionMatrix(random_forest_pred_validation, validation$SUBSCRIBED, mode = "prec_recall", positive = "yes")

plot(random_forest_model)
varImpPlot(random_forest_model)
importance(random_forest_model)

#random forest 3rd model
control <- trainControl(method='repeatedcv', 
                        number=10, 
                        repeats=3,
                        classProbs= TRUE,
                        summaryFunction = twoClassSummary,
                        search = "grid")

random_forest_model2 <- train(SUBSCRIBED ~ .,
                              data = train_scale_dummies,
                              method = "rf",
                              trControl = control,
                              metric = "ROC",
                              tuneGrid = expand.grid(ntree = c(100,200,300,400,500)))

random_forest_model2
plot(random_forest_model2)

random_forest_pred2 <- predict(random_forest_model, test_scale_dummies)
confusionMatrix(random_forest_pred2, test_scale_dummies$SUBSCRIBED, mode = "prec_recall", positive = "yes")


library(nnet)
library(MASS)
m1<-lda(SUBSCRIBED ~ .,
        data = train_scale)
m2<-predict(m1)

mult <- multinom(SUBSCRIBED ~ .,
                 data = train_scale)
summary(mult)
plot(mult$fitted, m2$SUBSCRIBED)
mult.class<- apply(mult$fitted,1,which.max)
table(mult.class,m2$class)

#################################################################################
###########################    Clustering     ###################################
#################################################################################
library(cluster)
library(dplyr)

data2 <- data[,c('age','job', 'marital', 'education', 'default', 'housing', 'loan', 'campaign', 'pdays', 'previous', 'poutcome')]
View(data2)

cut_data <-data2[sample(nrow(data2),10000),]
View(cut_data)

str(cut_data)


library(cluster)

#compute the gower distance
gower.dissimilarity.mtrx <- daisy(cut_data[,], metric = c("gower"))

#make the gower distacne, matrix because pam requaire a distacne matrix
gowerdist = as.matrix(gower.dissimilarity.mtrx)
dist <- gower.dissimilarity.mtrx

#for 2 clusters
pamx2 <- pam(dist, 2)
sil2 = silhouette (pamx2$clustering, dist)

#visualize the clustering and the silhouette value
plot(sil2,col=1:2,border=NA, main="Silhouette plot for 2 clusters")

#for 3 clusters
pamx3 <- pam(dist, 3)
sil3 = silhouette (pamx3$clustering, dist)
plot(sil3,col=1:3,border=NA)

#for 4 clusters
pamx4 <- pam(dist, 4)
sil4 = silhouette (pamx4$clustering, dist)
plot(sil4,col=1:4,border=NA)

#for 5 clusters
pamx5 <- pam(dist, 5)
sil5 = silhouette (pamx5$clustering, dist)
plot(sil5,col=1:5,border=NA)

#for 6 clusters
pamx6 <- pam(dist, 6)
sil6 = silhouette (pamx6$clustering, dist)
plot(sil6,col=1:6,border=NA)

#for 6 clusters
pamx7 <- pam(dist, 7)
sil7 = silhouette (pamx7$clustering, dist)
plot(sil7,col=1:7,border=NA, main="Silhouette plot for 7 clusters")


#Visualize the clusters
library(Rtsne)
library(ggplot2)
tsne_obj <- Rtsne(gower.dissimilarity.mtrx, is_distance = TRUE)

tsne_data <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(pamx2$clustering))

ggplot(aes(x = X, y = Y), data = tsne_data) +
  geom_point(aes(color = cluster)) +
  ggtitle("Visualization of the 2 clusters")



library(Rtsne)
tsne_obj <- Rtsne(gower.dissimilarity.mtrx, is_distance = TRUE)

tsne_data <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(pamx3$clustering))

ggplot(aes(x = X, y = Y), data = tsne_data) +
  geom_point(aes(color = cluster))+
  ggtitle("Visualization of the 3 clusters")


library(Rtsne)
tsne_obj <- Rtsne(gower.dissimilarity.mtrx, is_distance = TRUE)

tsne_data <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(pamx4$clustering))

ggplot(aes(x = X, y = Y), data = tsne_data) +
  geom_point(aes(color = cluster)) + 
  ggtitle("Visualization of the 4 clusters")






###############################################
############## Other attempts #################
###############################################
library(fastAdaboost)
control <- trainControl(method='repeatedcv', 
                        number=10, 
                        repeats=3,
                        classProbs= TRUE,
                        summaryFunction = twoClassSummary,
                        search = "grid")

adaboost_model <- train(SUBSCRIBED ~ .,
                        data = train_scale,
                        method = "adaboost",
                        trControl = control,
                        metric = "F1")

adaboost_pred <- predict(adaboost_model, test_scale_dummies)
confusionMatrix(random_forest_pred2, test_scale_dummies$SUBSCRIBED, mode = "prec_recall", positive = "yes")


#-------------------------------------------------
control <- trainControl(method='repeatedcv', 
                        number=10, 
                        repeats=3,
                        classProbs= TRUE,
                        summaryFunction = twoClassSummary,
                        search = "grid")

svmlin <- train(SUBSCRIBED ~ .,
                data = train_scale,
                method = "svmLinear",
                trControl = control,
                metric = "ROC")



control <- trainControl(method='repeatedcv', 
                        number=10, 
                        repeats=3,
                        classProbs= TRUE,
                        summaryFunction = twoClassSummary,
                        search = "grid")

svmrad <- train(SUBSCRIBED ~ .,
                data = train_scale,
                method = "svmRadial",
                trControl = control,
                metric = "ROC")


control <- trainControl(method='repeatedcv', 
                        number=10, 
                        repeats=3,
                        classProbs= TRUE,
                        summaryFunction = twoClassSummary,
                        search = "grid")

svmpol <- train(SUBSCRIBED ~ .,
                data = train_scale,
                method = "svmPoly",
                trControl = control,
                metric = "ROC")
