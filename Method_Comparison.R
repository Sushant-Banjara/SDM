library('raster')
library ('dismo')
library ('caret')
library('pROC') #For auc and roc

#read in the saved file
setwd ('C:/Users/Sushant/Desktop/Data_Science_R/SDM/2-SDM-Data')
pa = read.csv('pres_abs1.csv')

#get the list of all the necessary raster files
rasters = list.files(pattern = '.tif$')

reference_raster = raster('altitude.tif')
crsx = crs (reference_raster)
#create an empty stack object
stck = stack ()
extn = extent(99.64028, 104.5514, 1.265278, 6.726389)
#create a single stack of all raster files
for (files in rasters){
  tempraster = raster (files)
  new_temp = resample(tempraster, reference_raster, method = 'ngb')
  new_temp = setExtent(new_temp, ext = extn)
  stck = stack(stck, new_temp)
  print(new_temp)
}

#set seed for pseudo repeatability and divide the data
set.seed(15)
partition_index = createDataPartition(pa$pb, p = 0.75, times = 1, list = FALSE)
training = pa[partition_index, ]
testing = pa[-partition_index, ]

##################################GLM Model##########################################
train_control = trainControl(method = 'cv', number = 10)
pb = as.factor(training$pb)
land = as.factor(training$land)
logistic = train (pb ~ ., data = training, trControl = train_control,
                method = 'glm', family = 'binomial' )
summary (logistic)
varImp(logistic)

p1 = predict(logistic, testing, type = 'raw')


roc_model = pROC::roc(testing [, 'pb'], p1)
auc = pROC::auc(roc_model)
plot(roc_model)
auc

map = predict(stck, logistic)
plot(map, main = 'GLM Speices Distribution Map')
#######################################################################################

###################################SVM_Model###########################################
svm = train (pb ~ ., data = training, trControl = train_control,
                  method = 'svmRadial')
summary (svm)
varImp(svm)

p1 = predict(svm, testing, type = 'raw')


roc_model = pROC::roc(testing [, 'pb'], p1)
auc = pROC::auc(roc_model)
plot(roc_model)
auc

map = predict(stck, svm)
plot(map, main = 'RBF Kernel Speices Distribution Map')
#########################################################################################

#######################################KNN_Model#########################################
kNN = train (pb ~ ., data = training, trControl = train_control,
             method = 'knn')
summary (kNN)
varImp(kNN)

p1 = predict(kNN, testing, type = 'raw')


roc_model = pROC::roc(testing [, 'pb'], p1)
auc = pROC::auc(roc_model)
plot(roc_model)
auc

map = predict(stck, kNN)
plot(map, main = 'KNN Speices Distribution Map')
#########################################################################################

###############################Random Forest############################################
rf = train (pb ~ ., data = training, trControl = train_control,
             method = 'rf', importance = TRUE)
summary (rf)
varImp(rf)

p1 = predict(rf, testing, type = 'raw')


roc_model = pROC::roc(testing [, 'pb'], p1)
auc = pROC::auc(roc_model)
plot(roc_model)
text(0.5, 0.5, paste('AUC = ', format(auc, digit = 10, scientific = FALSE)))
auc

map = predict(stck, rf)
plot(map, main = 'Random Forest Speices Distribution Map')
##########################################################################################

###############################Gradient Boosting Machine##################################
gbm = train (pb ~ ., data = training, trControl = train_control,
            method = 'gbm')
summary (gbm)

p1 = predict(gbm, testing, type = 'raw')


roc_model = pROC::roc(testing [, 'pb'], p1)
auc = pROC::auc(roc_model)
plot(roc_model)
text(0.5, 0.5, paste('AUC = ', format(auc, digit = 10, scientific = FALSE)))
auc

map = predict(stck, gbm)
plot(map, main = 'GBM Species Distribution Map')
