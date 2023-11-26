#No 1
#Explorasi Data
Training <- read_excel("Training_ETS.xlsx")
View(Training)

#Uji Signifikansi
cor.test(Training$Amount,Training$V1, method = "pearson")
cor.test(Training$Amount,Training$V2, method = "pearson")
cor.test(Training$Amount,Training$V3, method = "pearson")
cor.test(Training$Amount,Training$V4, method = "pearson")
cor.test(Training$Amount,Training$V5, method = "pearson")
cor.test(Training$Amount,Training$V6, method = "pearson")
cor.test(Training$Amount,Training$V7, method = "pearson")
cor.test(Training$Amount,Training$V8, method = "pearson")
cor.test(Training$Amount,Training$V9, method = "pearson")
cor.test(Training$Amount,Training$V10, method = "pearson")
cor.test(Training$Amount,Training$V11, method = "pearson")
cor.test(Training$Amount,Training$V12, method = "pearson")
cor.test(Training$Amount,Training$V13, method = "pearson")
cor.test(Training$Amount,Training$V14, method = "pearson")
cor.test(Training$Amount,Training$V15, method = "pearson")
cor.test(Training$Amount,Training$V16, method = "pearson")
cor.test(Training$Amount,Training$V17, method = "pearson")
cor.test(Training$Amount,Training$V18, method = "pearson")
cor.test(Training$Amount,Training$V19, method = "pearson")
cor.test(Training$Amount,Training$V20, method = "pearson")
cor.test(Training$Amount,Training$V21, method = "pearson")
cor.test(Training$Amount,Training$V21, method = "pearson")
cor.test(Training$Amount,Training$V22, method = "pearson")
cor.test(Training$Amount,Training$V23, method = "pearson")
cor.test(Training$Amount,Training$V24, method = "pearson")
cor.test(Training$Amount,Training$V25, method = "pearson")
cor.test(Training$Amount,Training$V26, method = "pearson")
cor.test(Training$Amount,Training$V27, method = "pearson")
cor.test(Training$Amount,Training$V28, method = "pearson")

#karena .... memiliki nilai p value dari t test lebih dari 0.05, 
#artinya variabel-variabel tersebut tidak memberikan pengaruh yang signifikan terhadap Amount(Y)
#sehingga 13 variabel tersebut tidak digunakan untuk membentuk model, hanya ada 15 variabel
  
#Mengeluarkan variabel yang tidak signifikan atau tidak memiliki korelasi yang tinggi
Training_Prediksi <- subset(Training, select = -c(No., Time, V9, V12, V13, V14, V16, V17, V18, V19, V21, V22, V24, V25, V28, Class))
Testing_Prediksi <- subset(Testing_ETS, select = -c(No., Time, V9, V12, V13, V14, V16, V17, V18, V19, V21, V22, V24, V25, V28, Class))

#Split Data Menjadi Training dan Validation
set.seed(123)
SplitData <- sample.split(Training_Prediksi$Amount, SplitRatio = 0.75)
Training_set <- subset(Training_Prediksi, SplitData == TRUE)
View(Training_set)
Validation_set <- subset(Training_Prediksi, SplitData == FALSE)
View(Validation_set)

#Metode Regresi Berganda
library(lmtest)
Regresi_Berganda <- lm(Amount ~ V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V10 + 
                       V11 + V15 + V20 + V23 + V26 + V27, data = Training_set)

summary(Regresi_Berganda)

Prediksi_Regresi <- predict(Regresi_Berganda, Validation_set)
Error_Regresi <- Prediksi_Regresi - Validation_set$Amount
RMSE_Regresi <- sqrt(mean(Error_Regresi^2))
RMSE_Regresi

#Metode SVR
library(caret)
model_svr <- svm(Amount ~ V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V10 + 
                + V11 + V15 + V20 + V23 + V26 + V27, data = Training_set)

Prediksi_svr <- predict(model_svr, newdata = Validation_set[, -which(names(Validation_set) == "Amount")])
View(Prediksi_svr)

Error_svr <- Prediksi_svr - Validation_set$Amount
Error_svr
RMSE_svr <- sqrt(mean(Error_svr^2))
RMSE_svr

#Metode Random Forest
library(randomForest)
RF_Model <- randomForest(Amount~V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V10 + 
                         + V11 + V15 + V20 + V23 + V26 + V27, data = Training_set)

Prediksi_RF <- predict(RF_Model, newdata = Validation_set)
Error_RF <- cor(Prediksi_RF,Validation_set$Amount)
RMSE_RF <- sqrt(mean((Prediksi_RF - Validation_set$Amount)^2))
RMSE_RF

##Testing Model Terbaik##
model_svr_testing <- svm(Amount ~ V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V10 + 
                   + V11 + V15 + V20 + V23 + V26 + V27, data = Training_set)

Prediksi_svr_testing <- predict(model_svr_testing, newdata = Testing_Prediksi[, -which(names(Testing_Prediksi) == "Amount")])
View(Prediksi_svr_testing)

Error_svr_testing <- Prediksi_svr_testing - Testing_Prediksi$Amount
RMSE_svr_testing <- sqrt(mean(Error_svr_testing^2))
RMSE_svr_testing


#No 2
#Mengeluarkan variabel yang tidak signifikan atau tidak memiliki korelasi yang tinggi
Training_Klasifikasi <- subset(Training, select = -c(No., Time, V9, V12, V13, V14, V16, V17, V18, V19, V21, V22, V24, V25, V28, Amount))
Testing_Klasifikasi <- subset(Testing_ETS, select = -c(No., Time, V9, V12, V13, V14, V16, V17, V18, V19, V21, V22, V24, V25, V28, Amount))

#Penanganan imbalance data
prop.table(Training_Klasifikasi$Class)
table(Training_Klasifikasi$Class)
library(smotefamily)
library(DMwR2)
set.seed(123)
balance_data = ovun.sample(Class~.,data=Training_Klasifikasi,method="over")
balance_data = balance_data$data
table(balance_data$Class)

#Split Data Menjadi Training dan Validation
set.seed(123)
data_split <- initial_split(balance_data, prop = 0.75)
train_data <- training(data_split)
test_data <- testing(data_split)

#Regresi Logistik
str(train_data)
library(glmnet)
model_reglog <- glm(Class~V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V10 + 
              + V11 + V15 + V20 + V23 + V26 + V27, data = train_data, family = binomial(link="logit"))
summary(model_reglog)

library(dplyr)
prediksi_reglog <- model_reglog %>% predict(test_data, type = "response")
prediksi_reglog_factor <-  as.factor(ifelse(prediksi_reglog > 0.5,1,0))
#as.factor(round(prediksi_reglog))
cm_reglog <- confusionMatrix(table(Predicted = prediksi_reglog_factor, Actual=test_data$Class))
cm_reglog

#Support Vector Machine
library(caret)
model_svm <- svm(Class~V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V10 + 
              + V11 + V15 + V20 + V23 + V26 + V27, data=train_data)
summary(model_svm)

prediksi_svm <- model_svm %>% predict(test_data, type = "response")
prediksi_svm_factor <- as.factor(ifelse(prediksi_svm> 0.5,1,0))

cm_svm <- confusionMatrix(table(Predicted=prediksi_svm_factor, Actual=test_data$Class))
cm_svm

#Decision Tree
library(party)
model_mytree <- ctree(Class~V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V10 + 
                + V11 + V15 + V20 + V23 + V26 + V27, data=train_data, controls=ctree_control(mincriterion=0.9, minsplit=50))

plot(mytree,type="simple")

prediksi_mytree <- mytree%>% predict(test_data, type = "response")
prediksi_mytree_factor <- as.factor(ifelse(prediksi_mytree > 0.5,1,0))

cm_mytree <- confusionMatrix(table(Predicted=prediksi_mytree_factor, Actual=test_data$Class))
cm_mytree

#Naive Bayes Clasification
library(e1071)
library(caTools)
library(caret)

model_naive <- naiveBayes(Class ~ V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V10 + 
                          + V11 + V15 + V20 + V23 + V26 + V27, data = train_data)

summary(model_naive)
prediksi_naive <- predict(model_naive, newdata = test_data)

cm_naive <- table(test_data$Class, prediksi_naive)
confusionMatrix(cm)

#Dengan menggunakan data validation, diperoleh 2 metode dengan tingkat akurasi yang sama yaitu regresi logistik biner dan SVM. 
#Selanjutnya menguji kedua model tersebut menggunakan data Testing sebanyak 1000

##Testing Model Terbaik (RegLog)##
model_reglog_testing <- glm(Class~V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V10 + 
                            + V11 + V15 + V20 + V23 + V26 + V27, data = train_data, family = binomial(link="logit"))
summary(model_reglog_testing)

library(dplyr)
prediksi_reglog_testing <- model_reglog_testing %>% predict(Testing_Klasifikasi, type = "response")
prediksi_reglog_testing_factor <- as.factor(ifelse(prediksi_reglog_testing > 0.5,1,0))
cm_reglog_testing <- confusionMatrix(table(Predicted = prediksi_reglog_testing_factor, Actual=Testing_Klasifikasi$Class))
cm_reglog_testing
view(prediksi_reglog_testing_factor)

##Testing Model Terbaik (SVM)##
model_svm_testing <- svm(Class~V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V10 + 
                         + V11 + V15 + V20 + V23 + V26 + V27, data=train_data)
summary(model_svm_testing)

prediksi_svm_testing <- model_svm %>% predict(Testing_Klasifikasi, type = "response")
prediksi_svm_testing_factor <- as.factor(round(prediksi_svm_testing))

cm_svm_testing <- confusionMatrix(table(Predicted=prediksi_svm_testing_factor, Actual=Testing_Klasifikasi$Class))
cm_svm_testing

#Dari hasil analisis menggunakan data Testing, dapat disimpulkan bahwa model Regresi Logistik Biner 
#lebih baik dibanding model SVM karena memiliki nilai akurasi yang lebih tinggi yaitu 99%

##Impor data hasil prediksi##
results_prediksi <- data.frame(Actual = Testing_Prediksi$Amount, Predicted = Prediksi_svr_testing)
write.csv(results_prediksi, file = "hasil prediksi svr.csv", row.names = FALSE)

##Impor data hasil klasifikasi##
results_klasifikasi <- data.frame(Actual = Testing_Klasifikasi$Class, Predicted = prediksi_reglog_testing_factor)
write.csv(results_klasifikasi, file = "hasil klasifikasi reglog.csv", row.names = FALSE)

