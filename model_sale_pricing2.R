
library(caret)
library(dplyr)
library(magrittr)
#######read data file#########
train <- read.csv("C:/Users/212489345/Documents/class/Kaggle/house_pricing/train.csv", stringsAsFactors = FALSE) 
test <- read.csv("C:/Users/212489345/Documents/class/Kaggle/house_pricing/test.csv", stringsAsFactors = FALSE)

# View(dat_all[dat_all$Id %in% c(251,633,1324),])

train$cat <- "train"
test$cat <- "test"

#data transformation
train_price <- data.frame(train$Id, train$SalePrice)
colnames(train_price) <- c("Id", "SalePrice")

train$SalePrice <- NULL

dat_all <- rbind(train, test)

num_var <- names(dat_all)[sapply(dat_all, class)=="integer"][-1]

######categorize not true missing values#############
notrue_miss <- c("Alley", "BsmtQual", "BsmtCond", "BsmtExposure", "BsmtFinType1", "BsmtFinType2", 
                   "FireplaceQu", "GarageType", "GarageFinish", "GarageQual", "GarageCond", "PoolQC", "Fence", 
                   "MiscFeature")


for (y in (1:length(notrue_miss))){
  dat_all[is.na(dat_all[notrue_miss[y]])==TRUE, notrue_miss[y]] <- "NA_f"
}

#####check missing values proportion##################
apply(dat_all, 2, function(x) sum(is.na(x)))

missing <- as.data.frame(
  apply(dat_all, 2, function(x) round(sum(is.na(x))/nrow(dat_all),2))) %>% 
  set_colnames("missing") 
missing$var <- row.names(missing)
missing <- arrange(missing, desc(missing))


##### missing value imputation: only 4 missing value##############################
##### only can campute numerical missing values
# library(missForest)
# set.seed(100)
# dat_all_dv <- missForest(dat_all_dv, ntree = 100) ##take looong time

# library(caret)
# colnames(dat_all_dv)[3] <- "MSZoningC_all"
# imp_caret <- preProcess(dat_all_dv, method = c("bagImpute")) ##take looong time too. also condition on the names format
# dat_all_dv <- predict(imp_caret, dat_all_dv)

###probably the best way is to knn or use median/mean
# x[is.na(x)] <.] mean(x, na.rm=T)
#zoo::na.aggregate(x, FUN = mean) # from zoo
## [1] 1.00 2.00 3.00 2.75 5.00
# last obs carried forward

library(zoo)
dat_all <- arrange(dat_all, Neighborhood, GrLivArea, LotArea, OverallQual, YearBuilt)
class_dt <- as.data.frame(sapply(dat_all, class))
dat_all <- na.locf(dat_all)

#class_dt[,1] <- gsub("character", "factor", class_dt[,1])
#apply back original class after "zoo" transformation
for (c in (1:nrow(class_dt))){
  class(dat_all[,c]) <- as.character(class_dt[c,1])
}

apply(dat_all, 2, function(x) sum(is.na(x)))


##add new variables##
dat_all$ExterQual_n <- ifelse(dat_all$ExterQual == "Ex", 5, 
                              ifelse(dat_all$ExterQual == "Gd", 4, 
                                     ifelse(dat_all$ExterQual == "TA", 3, 
                                            ifelse(dat_all$ExterQual == "Fa", 2,1))))

dat_all$ExterCond_n <- ifelse(dat_all$ExterCond == "Ex", 5, 
                              ifelse(dat_all$ExterCond == "Gd", 4, 
                                     ifelse(dat_all$ExterCond == "TA", 3, 
                                            ifelse(dat_all$ExterCond == "Fa", 2,1))))

dat_all$BsmtQual_n <- ifelse(dat_all$BsmtQual == "Ex", 5, 
                             ifelse(dat_all$BsmtQual == "Gd", 4, 
                                    ifelse(dat_all$BsmtQual == "TA", 3, 
                                           ifelse(dat_all$BsmtQual == "Fa", 2,
                                                  ifelse(dat_all$BsmtQual == "Po",1,0)))))
dat_all$BsmtCond_n <- ifelse(dat_all$BsmtCond == "Ex", 5, 
                             ifelse(dat_all$BsmtCond == "Gd", 4, 
                                    ifelse(dat_all$BsmtCond == "TA", 3, 
                                           ifelse(dat_all$BsmtCond == "Fa", 2,
                                                  ifelse(dat_all$BsmtCond == "Po",1,0)))))

dat_all$BsmtExposure_n <- ifelse(dat_all$BsmtExposure == "Gd", 5, 
                                 ifelse(dat_all$BsmtExposure == "Av", 4, 
                                        ifelse(dat_all$BsmtExposure == "Mn", 3, 
                                               ifelse(dat_all$BsmtExposure == "No", 2,1
                                               ))))

dat_all$BsmtFinType1_n <- ifelse(dat_all$BsmtFinType1 == "GLQ", 6, 
                                 ifelse(dat_all$BsmtFinType1 == "ALQ", 5, 
                                        ifelse(dat_all$BsmtFinType1 == "BLQ", 4, 
                                               ifelse(dat_all$BsmtFinType1 == "Rec", 3,
                                                      ifelse(dat_all$BsmtFinType1 == "LwQ",2,
                                                             ifelse(dat_all$BsmtFinType1 == "Unf",1,0))))))

dat_all$BsmtFinType2_n <- ifelse(dat_all$BsmtFinType2 == "GLQ", 6, 
                                 ifelse(dat_all$BsmtFinType2 == "ALQ", 5, 
                                        ifelse(dat_all$BsmtFinType2 == "BLQ", 4, 
                                               ifelse(dat_all$BsmtFinType2 == "Rec", 3,
                                                      ifelse(dat_all$BsmtFinType2 == "LwQ",2,
                                                             ifelse(dat_all$BsmtFinType2 == "Unf",1,0))))))

dat_all$HeatingQC_n <- ifelse(dat_all$HeatingQC == "Ex", 5, 
                              ifelse(dat_all$HeatingQC == "Gd", 4, 
                                     ifelse(dat_all$HeatingQC == "TA", 3, 
                                            ifelse(dat_all$HeatingQC == "Fa", 2,
                                                   ifelse(dat_all$HeatingQC == "Po",1,
                                                          0)))))
dat_all$KitchenQual_n <- ifelse(dat_all$KitchenQual == "Ex", 5, 
                                ifelse(dat_all$KitchenQual == "Gd", 4, 
                                       ifelse(dat_all$KitchenQual == "TA", 3, 
                                              ifelse(dat_all$KitchenQual == "Fa", 2,
                                                     ifelse(dat_all$KitchenQual == "Po",1,
                                                            0)))))
dat_all$FireplaceQu_n <- ifelse(dat_all$FireplaceQu == "Ex", 5, 
                                ifelse(dat_all$FireplaceQu == "Gd", 4, 
                                       ifelse(dat_all$FireplaceQu == "TA", 3, 
                                              ifelse(dat_all$FireplaceQu == "Fa", 2,
                                                     ifelse(dat_all$FireplaceQu == "Po",1,
                                                            0)))))
dat_all$GarageQual_n <- ifelse(dat_all$GarageQual == "Ex", 5, 
                               ifelse(dat_all$GarageQual == "Gd", 4, 
                                      ifelse(dat_all$GarageQual == "TA", 3, 
                                             ifelse(dat_all$GarageQual == "Fa", 2,
                                                    ifelse(dat_all$GarageQual == "Po",1,
                                                           0)))))
dat_all$GarageCond_n <- ifelse(dat_all$GarageCond == "Ex", 5, 
                               ifelse(dat_all$GarageCond == "Gd", 4, 
                                      ifelse(dat_all$GarageCond == "TA", 3, 
                                             ifelse(dat_all$GarageCond == "Fa", 2,
                                                    ifelse(dat_all$GarageCond == "Po",1,
                                                           0)))))
dat_all$GarageFinish_n <- ifelse(dat_all$GarageFinish == "Fin", 4, 
                                 ifelse(dat_all$GarageFinish == "RFn", 3, 
                                        ifelse(dat_all$GarageFinish == "Unf", 2, 
                                               1)))
dat_all$PoolQC_n <- ifelse(dat_all$PoolQC == "Ex", 5, 
                           ifelse(dat_all$PoolQC == "Gd", 4, 
                                  ifelse(dat_all$PoolQC == "TA", 3, 
                                         ifelse(dat_all$PoolQC == "Fa", 2,
                                                ifelse(dat_all$PoolQC == "Po",1,
                                                       0)))))
dat_all$PavedDrive_n <- ifelse(dat_all$PavedDrive == "Y", 4, 
                               ifelse(dat_all$PavedDrive == "P", 3, 
                                      ifelse(dat_all$PavedDrive == "N", 2, 
                                             1)))
dat_all$Fence_n <- ifelse(dat_all$Fence == "GdPrv", 5, 
                          ifelse(dat_all$Fence == "MnPrv", 4, 
                                 ifelse(dat_all$Fence == "GdWo", 3, 
                                        ifelse(dat_all$Fence == "MnWw", 2,
                                               1))))


####add new variable: categorize frequent variables####
dat_all$BldgType_n <- ifelse(dat_all$BldgType == "Reg",1,0)
dat_all$LandContour_n <- ifelse(dat_all$LandContour == "Lv1",1,0)
dat_all$LandSlope_n <- ifelse(dat_all$LandSlope=="Gtl",3,
                              ifelse(dat_all$LandSlope=="Mod",2,1))
dat_all$Electrical_n <- ifelse(dat_all$Electrical=="SBrkr",1,0)
dat_all$GarageType_n <- ifelse(dat_all$GarageType == "Detchd",1,0)
dat_all$MiscFeature_n <- ifelse(dat_all$MiscFeature == "Shed",1,0)
dat_all$LotShape_n <- ifelse(dat_all$LotShape == "Reg",1,0)
dat_all$Utilities_n <- ifelse(dat_all$Utilities == "AllPub", 1,0)
dat_all$Condition1_n <- ifelse(dat_all$Condition1 == "Norm",1,0)
dat_all$Condition2_n <- ifelse(dat_all$Condition2 == "Norm",1,0)
dat_all$HouseStyle_n <- ifelse(dat_all$HouseStyle %in% c("1.5Unf","2.5Unf"),0,1)
dat_all$CentralAir_n <- ifelse(dat_all$CentralAir == "Yes",1,0)
dat_all$Functional_n <- ifelse(dat_all$Functional == "Typ",1,0)
dat_all$SaleCondition_n <- ifelse(dat_all$SaleCondition %in% c("Normal","Partial"),1,0)
dat_all$MSSubClass_n <- ifelse(dat_all$MSSubClass %in% c(20,60,120,160),1,0)

dat_all$remodeled <- ifelse(dat_all$YearRemodAdd != dat_all$YearBuilt,1,0)
dat_all$RecentRemodel <- ifelse(dat_all$YearRemodAdd != dat_all$YrSold,1,0)
dat_all$VeryNewHouse <- ifelse(dat_all$YearBuilt == dat_all$YrSold,1,0)


####add new variable: categorize numeircal variables####
dat_all$X2ndFlrSF_n <- ifelse(dat_all$X2ndFlrSF >0 ,1,0) #has 2nd floor or not
dat_all$MasVnrArea_n <- ifelse(dat_all$MasVnrArea >0 ,1,0)
dat_all$WoodDeckSF_n <- ifelse(dat_all$WoodDeckSF >0 ,1,0)
dat_all$OpenPorchSF_n <- ifelse(dat_all$OpenPorchSF >0 ,1,0)
dat_all$EnclosedPorch_n <- ifelse(dat_all$EnclosedPorch >0 ,1,0)
dat_all$X3SsnPorch_n <- ifelse(dat_all$X3SsnPorch >0 ,1,0)
dat_all$ScreenPorch_n <- ifelse(dat_all$ScreenPorch >0 ,1,0)


dat_all$ttl_area <- sum(dat_all[,c('LotFrontage', 'LotArea', 'MasVnrArea', 'BsmtFinSF1', 'BsmtFinSF2', 'BsmtUnfSF',
                 'TotalBsmtSF', 'X1stFlrSF', 'X2ndFlrSF', 'GrLivArea', 'GarageArea', 'WoodDeckSF', 
                                   'OpenPorchSF', 'EnclosedPorch', 'X3SsnPorch', 'ScreenPorch', 'LowQualFinSF', 'PoolArea')])
dat_all$ttl_area_1_2 <- sum(dat_all[,c('X1stFlrSF','X2ndFlrSF')])


####add new variable: timing/season####

dat_all$age_build <- dat_all$YrSold - dat_all$YearBuilt
dat_all$age_remo <- dat_all$YrSold - dat_all$YearRemodAdd
dat_all$age_gara <- dat_all$YrSold - dat_all$GarageYrBlt

dat_all$winter <- ifelse(dat_all$MoSold<=4 | dat_all$MoSold>=11,1,0)
dat_all$good_year <- ifelse(dat_all$YrSold %in% c(2008,2009),1,0)

####add new variable: good neighbor/bad neighbor####
dat_all$Neighborhood_n <- ifelse(dat_all$Neighborhood %in% c("NridgHt",
                                                             "Crawfor",
                                                             "StoneBr",
                                                             "Somerst",
                                                             "NoRidge"),1,0)

##Hot-key transformation to prepare missing value preparation############
cat <- dat_all[, names(dat_all) %in% c("Id", "cat", "Neighborhood")]
dat_all <- dat_all[, !names(dat_all) %in% c("cat","Neighborhood")]

dv <- dummyVars(~., data=dat_all, sep = ".", fullRank=FALSE)
dat_all <- predict(dv, dat_all)

dat_all <- merge(dat_all, cat, by="Id", all.x = TRUE)



######################new EDA for training data
#check correlation
train_eda <- dat_all[dat_all$cat=="train",]
train_eda <- merge(train_eda, train_price, by="Id")
num_var <- colnames(train_eda)[sapply(train_eda, is.numeric)] #find numerical variables

corr <- as.data.frame(cor(train_eda[,!names(train_eda)%in% c("Id","cat","Neighborhood")],
                          train_eda[,"SalePrice"],method="pearson"))
corr$name <- row.names(corr)
corr <- arrange(corr, desc(V1))
corr <- corr[corr$V1>=0.3 | corr$V1 <=-0.3,]
corr <- corr[complete.cases(corr)==TRUE,]
#hist(corr$V1)
#lesson learned from correlation: there are outliers... need to reshape some high correlated varialbes

#name not right in the chart
library(car)
# for(j in 0:(floor(nrow(corr)/5)+1)){
#   for (n in (2+j*5):(2+j*5+4)){ #(nrow(corr)-4)
#     scatterplotMatrix(~eval(parse(text=corr[n,2]))+
#                         eval(parse(text=corr[n+1,2]))+
#                         eval(parse(text=corr[n+2,2]))+
#                         eval(parse(text=corr[n+3,2]))+
#                         eval(parse(text=corr[n+4,2]))+
#                         SalePrice, data=train_eda)
#     
#   }
# }

#by using excel to combine all highly correlated variables
scatterplotMatrix(~OverallQual+GrLivArea+ExterQual_n+KitchenQual_n++SalePrice, data=train_eda)	
scatterplotMatrix(~GarageCars+GarageArea+TotalBsmtSF+X1stFlrSF+BsmtQual_n++SalePrice, data=train_eda)	
scatterplotMatrix(~FullBath+Neighborhood_n+BsmtQualEx+GarageFinish_n+TotRmsAbvGrd++SalePrice, data=train_eda)	
scatterplotMatrix(~YearBuilt+FireplaceQu_n+YearRemodAdd+KitchenQualEx+FoundationPConc++SalePrice, data=train_eda)	
scatterplotMatrix(~GarageYrBlt+MasVnrArea+Fireplaces+ExterQualGd+ExterQualEx++SalePrice, data=train_eda)	
scatterplotMatrix(~BsmtFinType1GLQ+HeatingQCEx+HeatingQC_n+GarageFinishFin+OpenPorchSF_n++SalePrice, data=train_eda)	
scatterplotMatrix(~BsmtFinSF1+BsmtExposure_n+MasVnrArea_n+SaleTypeNew++SalePrice, data=train_eda)	
scatterplotMatrix(~SaleConditionPartial+MSSubClass_n+FireplaceQuGd+GarageTypeAttchd+LotFrontage++SalePrice, data=train_eda)	
scatterplotMatrix(~MasVnrTypeStone+WoodDeckSF+KitchenQualGd+X2ndFlrSF++SalePrice, data=train_eda)	
scatterplotMatrix(~OpenPorchSF+BsmtExposureGd+Exterior2ndVinylSd+Exterior1stVinylSd+BsmtFinType1_n++SalePrice, data=train_eda)	
scatterplotMatrix(~HeatingQCTA+RecentRemodel+FoundationCBlock+GarageTypeDetchd+GarageType_n++SalePrice, data=train_eda)	
scatterplotMatrix(~MasVnrTypeNone+GarageFinishUnf+BsmtQualTA+FireplaceQuNA_f+age_gara++SalePrice, data=train_eda)	
scatterplotMatrix(~age_remo+KitchenQualTA+age_build+ExterQualTA++SalePrice, data=train_eda)

#learning: 
#log: age_build, age_remo, age_gara GarageYrBlt YearBuilt YearRemoAdd
#binary classification: yes
#outlier: Id: 186,692,1183,1170 (age_build). 547,523,502(OpenPorchSF). 935,1299(LotFrontage)
# 1299(BsmtFinSF1) 524(GrLivArea)
#classification model: OpenPorchSF WoodDeckSF MasVnrArea GarageArea

#arrange(train_eda[train_eda$TotalBsmtSF>5000,c("Id","BsmtFinSF1","SalePrice")],desc(TotalBsmtSF))

#revise data after learning
dat_all$age_build <- log1p(abs(dat_all$age_build))
dat_all$age_remo <- log1p(abs(dat_all$age_remo))
dat_all$age_gara <- log1p(abs(dat_all$age_gara))
dat_all$GarageYrBlt <- log1p(dat_all$GarageYrBlt)
dat_all$YearBuilt <- log1p(dat_all$YearBuilt)
dat_all$YearRemodAdd <- log1p(dat_all$YearRemodAdd)

dat_all <- dat_all[!(dat_all$Id %in% c(186,692,1183,1170,547,523,502,935,1299,524)),]


##identify and remove outliers. should not do hard cut based on boxplot. leave some ranges.

# sort(boxplot(train_price$SalePrice)$out)
# sort(boxplot(dat_all$LotArea)$out)
# sort(boxplot(dat_all$GrLivArea)$out)
# sort(boxplot(dat_all$TotalBsmtSF)$out)

#?? shouldn't outliers be done after data categorical and EDA?
# outlier_price <- train_price[train_price$SalePrice >= 400000,"Id"] 
# 
# dat_all <- dat_all[!dat_all$Id %in% outlier_price, ] #remove outliers
# 
# dat_all <- dat_all[!(dat_all$LotArea>= 25000 & dat_all$cat=="train"),]
# dat_all <- dat_all[!(dat_all$GrLivArea>= 3000 & dat_all$cat=="train"),]
# dat_all <- dat_all[!(dat_all$TotalBsmtSF>= 2500 & dat_all$cat=="train"),]
# dat_all <- dat_all[!(dat_all$BsmtFinSF1>= 2500 & dat_all$cat=="train"),]
# dat_all <- dat_all[!(dat_all$GarageArea>= 1300 & dat_all$cat=="train"),]




##add new variables: nearest neighbor sale price####################
library(FNN)

dat_all <- merge(dat_all, train_price, by="Id", all.x = TRUE)

##for neighborhood that can find 10 nearest based on same scale of Lot Area
##for neighborhood that can't find 10 nearest based on same scale of Lot Area, use median accross the state
##neighborhood still tends to be more important than overall state. However, if same neighborhood but same living area, should be the most powerful

#assign test sale price as median value of the closeset decile of SF to avoid overfitting
library(tidyverse)
dat_all$GA_decile <- ntile(dat_all$GrLivArea, 10)
decile<- as.data.frame(
  tapply(dat_all[dat_all$cat=="train","SalePrice"], dat_all[dat_all$cat=="train","GA_decile"], median)
)
colnames(decile) <- "price_decile"
decile$GA_decile <- rep(1:10)

for (d in 1:10){
  dat_all[dat_all$cat=="test" & dat_all$GA_decile==d, "SalePrice"] <- decile[d,1]
  
}

dat_all$GA_decile <-NULL
# 
# #neigh_sale based on GRliving <= +-5% and year sold +- 1 year in the same neighborhood
# dat_all$neigh_sale <- 0
# dat_all$neigh_sale2 <- 0
# for (s in (1:nrow(dat_all))){
# 
#   test_dat <- dat_all[s,c("Id","SalePrice","Neighborhood","cat","GrLivArea", "YrSold")] #, colnames(neiSal_hotkey)
# 
#   train_dat <- dat_all[-s,c("Id","SalePrice","Neighborhood","cat","GrLivArea", "YrSold")] #, colnames(neiSal_hotkey)
#   train_dat <- train_dat[train_dat$Neighborhood == test_dat$Neighborhood &
#                            train_dat$GrLivArea/test_dat$GrLivArea <= 1.05 &
#                            train_dat$GrLivArea/test_dat$GrLivArea >= 0.95 &
#                            abs(train_dat$YrSold-test_dat$YrSold)<=1,]
#   
#   train_dat$gr_comp <- train_dat$GrLivArea/test_dat$GrLivArea-1
#   train_dat <- arrange(train_dat, abs(gr_comp), GrLivArea)
# 
#   median <- median(train_dat$SalePrice)
#   median2 <- train_dat[train_dat$cat=="train","SalePrice"][1]
# 
#   dat_all[s,"neigh_sale"] <- median
#   dat_all[s,"neigh_sale2"] <- median2
# 
# }
# 
# dat_all[is.na(dat_all$neigh_sale),"neigh_sale"] <- dat_all[is.na(dat_all$neigh_sale),"SalePrice"]
# dat_all[is.na(dat_all$neigh_sale2),"neigh_sale2"] <- dat_all[is.na(dat_all$neigh_sale2),"SalePrice"]

# dat_all <- arrange(dat_all, GrLivArea, YrSold)
# 
# for (w in which(is.na(dat_all$neigh_sale2)==TRUE)){
#   dat_all[w,"neigh_sale2"] <- mean(dat_all[(w-1),"SalePrice"], dat_all[(w+1), "SalePrice"])
#   
# }


# dat_all$gr_comp <- NULL




##
neiSal_hotkey <- as.data.frame(model.matrix(~.+0, data = as.data.frame(dat_all[,"Neighborhood"])))
colnames(neiSal_hotkey) <- gsub(".*`", "", colnames(neiSal_hotkey))
colnames(neiSal_hotkey) <- paste("f", colnames(neiSal_hotkey), sep = "_")

dat_all <- cbind(dat_all, neiSal_hotkey)

table(dat_all$cat, dat_all$Neighborhood)
dat_all1 <- dat_all[dat_all$Neighborhood %in% c("Blmngtn", "Blueste", "BrDale", "ClearCr", 
                                                "MeadowV", "NPkVill", "StoneBr", "SWISU", "Veenker"),]
dat_all2 <- dat_all[!dat_all$Neighborhood %in% c("Blmngtn", "Blueste", "BrDale", "ClearCr", 
                                                 "MeadowV", "NPkVill", "StoneBr", "SWISU", "Veenker"),]
median_sale <- as.data.frame(tapply(dat_all1[,"SalePrice"], dat_all1[,"Neighborhood"], median))
median_sale$Neighborhood <- row.names(median_sale)
colnames(median_sale) <- c("neigh_sale3", "Neighborhood")
dat_all1 <- merge(dat_all1, median_sale, by="Neighborhood", all.x = TRUE)

dat_all2 <- arrange(dat_all2, Neighborhood, GrLivArea, OverallQual, age_build) #LotArea, KitchenAbvGr
dat_all2$neigh_sale3 <- 0
for (s in (1:nrow(dat_all2))){
  
  test_dat <- dat_all2[s,c("Id","SalePrice","Neighborhood","cat","GrLivArea", "OverallQual", "age_build")] #, colnames(neiSal_hotkey)
  
  train_dat <- dat_all2[-s,c("Id","SalePrice","Neighborhood","cat","GrLivArea", "OverallQual", "age_build")] #, colnames(neiSal_hotkey)
  train_dat <- train_dat[train_dat$Neighborhood == test_dat$Neighborhood,]
  # train_dat <- train_dat[train_dat$cat != "test",]
  neiSal_index <- train_dat[,"Id"]
  
  neiSal_knn <- knn(train = train_dat[,-c(1,2,3,4)], #using varialbes that matter in LASSO regression
                    test = test_dat[,-c(1,2,3,4)],
                    cl=neiSal_index,
                    k=5, #10 nearest neighbor is slight better than 1-5
                    algorithm = "brute")
  
  neiSal_indices <- attr(neiSal_knn, "nn.index")
  
  median <- median(train_dat[train_dat$Id %in% train_dat[neiSal_indices,"Id"], "SalePrice"])
  
  dat_all2[s,"neigh_sale3"] <- median
  
}

dat_all <- rbind(dat_all1, dat_all2)

dat_all$SalePrice <- NULL
dat_all<-dat_all[,!colnames(dat_all) %in% colnames(neiSal_hotkey)]


# train_eda <- dat_all[dat_all$cat=="train",]
# train_eda <- merge(train_eda, train_price, by="Id")
# scatterplotMatrix(~neigh_sale3+SalePrice, train_eda)
# cor(train_eda$neigh_sale3,train_eda$SalePrice)
# #outliers: 725,310
# head(arrange(train_eda[train_eda$SalePrice >300000,c("Id", "neigh_sale3","SalePrice")],
#               desc(neigh_sale3)))

dat_all <- dat_all[!dat_all$Id %in% c(725,310),]

##bin all the non-linear varialbes

#neigh_sale
# dat_all$decile <- ntile(dat_all[,names(dat_all) == "neigh_sale"], 20)
# summary <- as.data.frame(summarise(group_by(dat_all, decile), median(neigh_sale)))
# colnames(summary) <- c("decile", "bin_nei_sale")
# dat_all <- merge(dat_all, summary, by="decile", all.x = TRUE)
# 
# dat_all$decile <- ntile(dat_all[,names(dat_all) == "neigh_sale2"], 20)
# summary <- as.data.frame(summarise(group_by(dat_all, decile), median(neigh_sale2)))
# colnames(summary) <- c("decile", "bin_nei_sale2")
# dat_all <- merge(dat_all, summary, by="decile", all.x = TRUE)

dat_all$decile <- ntile(dat_all[,names(dat_all) == "neigh_sale3"], 20)
summary <- as.data.frame(summarise(group_by(dat_all, decile), median(neigh_sale3)))
colnames(summary) <- c("decile", "bin_nei_sale3")
dat_all <- merge(dat_all, summary, by="decile", all.x = TRUE)

#YearBuilt
dat_all$decile <- ntile(dat_all[,names(dat_all) == "YearBuilt"], 20)
summary <- as.data.frame(summarise(group_by(dat_all, decile), median(YearBuilt)))
colnames(summary) <- c("decile", "bin_yr_built")
dat_all <- merge(dat_all, summary, by="decile", all.x = TRUE)

#GarageArea
dat_all$decile <- ntile(dat_all[,names(dat_all) == "GarageArea"], 20)
summary <- as.data.frame(summarise(group_by(dat_all, decile), median(GarageArea)))
colnames(summary) <- c("decile", "bin_GarageArea")
dat_all <- merge(dat_all, summary, by="decile", all.x = TRUE)


#TotalBsmtSF
dat_all$decile <- ntile(dat_all[,names(dat_all) == "TotalBsmtSF"], 20)
summary <- as.data.frame(summarise(group_by(dat_all, decile), median(TotalBsmtSF)))
colnames(summary) <- c("decile", "bin_TotalBsmtSF")
dat_all <- merge(dat_all, summary, by="decile", all.x = TRUE)

#GrLivArea
dat_all$decile <- ntile(dat_all[,names(dat_all) == "GrLivArea"], 20)
summary <- as.data.frame(summarise(group_by(dat_all, decile), median(GrLivArea)))
colnames(summary) <- c("decile", "bin_GrLivArea")
dat_all <- merge(dat_all, summary, by="decile", all.x = TRUE)

#LotArea
dat_all$decile <- ntile(dat_all[,names(dat_all) == "LotArea"], 20)
summary <- as.data.frame(summarise(group_by(dat_all, decile), median(LotArea)))
colnames(summary) <- c("decile", "bin_LotArea")
dat_all <- merge(dat_all, summary, by="decile", all.x = TRUE)

#GarageYrBlt
dat_all$decile <- ntile(dat_all[,names(dat_all) == "GarageYrBlt"], 20)
summary <- as.data.frame(summarise(group_by(dat_all, decile), median(GarageYrBlt)))
colnames(summary) <- c("decile", "bin_GarageYrBlt")
dat_all <- merge(dat_all, summary, by="decile", all.x = TRUE)

#X1stFlrSF
dat_all$decile <- ntile(dat_all[,names(dat_all) == "X1stFlrSF"], 20)
summary <- as.data.frame(summarise(group_by(dat_all, decile), median(X1stFlrSF)))
colnames(summary) <- c("decile", "bin_X1stFlrSF")
dat_all <- merge(dat_all, summary, by="decile", all.x = TRUE)

#BsmtUnfSF
dat_all$decile <- ntile(dat_all[,names(dat_all) == "BsmtUnfSF"], 20)
summary <- as.data.frame(summarise(group_by(dat_all, decile), median(BsmtUnfSF)))
colnames(summary) <- c("decile", "bin_BsmtUnfSF")
dat_all <- merge(dat_all, summary, by="decile", all.x = TRUE)

#age_build
dat_all$decile <- ntile(dat_all[,names(dat_all) == "age_build"], 20)
summary <- as.data.frame(summarise(group_by(dat_all, decile), median(age_build)))
colnames(summary) <- c("decile", "bin_age_build")
dat_all <- merge(dat_all, summary, by="decile", all.x = TRUE)

# 
# poly_nesale <- as.data.frame(poly(dat_all$neigh_sale,3))
# colnames(poly_nesale) <- c("p1","p2","p3")
# dat_all <- cbind(dat_all, poly_nesale)

# dat_all <- arrange(dat_all, Id)
dat_all$decile <- NULL



##none-zero variance varialbe############
nz <- nearZeroVar(dat_all)
dat_all <- dat_all[, -nz]



##Hot-key transformation to prepare missing value preparation############
cat <- dat_all[, names(dat_all) %in% c("Id", "cat")]
dat_all <- dat_all[, !names(dat_all) == "cat"]

dv <- dummyVars(~., data=dat_all, sep = ".", fullRank=FALSE)
dat_all <- predict(dv, dat_all)

dat_all <- merge(dat_all, cat, by="Id", all.x = TRUE)

##skewness: exclude skewness non-matchable in train and test dataset
library(moments)
skewness2 <- data.frame()
for (sk in (1:(length(colnames(dat_all))-1))){ #only consider factor transfomred variables
  skewness_train <- skewness(dat_all[dat_all$cat=="train", colnames(dat_all)[sk]])
  skewness_test <- skewness(dat_all[dat_all$cat=="test", colnames(dat_all)[sk]])
  skewness <- data.frame(skewness_train, skewness_test, sk)
  skewness2 <- rbind(skewness2, skewness)
}

# library(ggplot2)
# ggplot(dat_all, aes(HouseStyle1Story, fill = cat)) + geom_density(alpha = 0.2)

exclude_col <- c(skewness2[is.na(skewness2$skewness_train)==TRUE,"sk"],
                 skewness2[is.na(skewness2$skewness_test)==TRUE,"sk"])

#dat_all <- dat_all[,-exclude_col] #notihing to exclude after using nonzerovariance


##pre-process: log data

var_log <- which(abs(skewness(dat_all[,-ncol(dat_all)]))>0.75)

dat_all[var_log] <- log1p(dat_all[var_log])

train_price$SalePrice <- log1p(train_price$SalePrice)


##pre-process data: Center + Scale#####
Id <- dat_all[,"Id"]
dat_all <- dat_all[,-1]
pp <- preProcess(dat_all, method = c("center", "scale"))
dat_all <- predict(pp, dat_all)
dat_all <- cbind(Id, dat_all)


#remove high correlated varialbes: only for GAM. Perform bad for LASSO after removing
# number <- sapply(dat_all, is.numeric)
# numvar <- dat_all[, number & !names(dat_all)%in%c('SalePrice','bin_nei_sale3')]
# cor <- findCorrelation(cor(numvar), 0.85)
# nm <- names(numvar[cor])
# 
# dat_all <- dat_all[,-cor]
# 
# colnames(dat_all)<- gsub(" |[(]|[)]", "", colnames(dat_all)) #remove " " in col names
 
################################################
##models##############
set.seed(100)

train <- dat_all[dat_all$cat == "train",]
test <- dat_all[dat_all$cat == "test",]

train <- merge(train_price, train, by="Id", all.y = TRUE)

part <- createDataPartition(train$SalePrice, p=0.8, list = FALSE)
ttrain <- train[part,]
ttest <- train[-part,]

###LM####
set.seed(111)
tr <- trainControl(method = "repeatedcv", number = 10, repeats = 5)
lm_caret <- train(SalePrice ~., data=ttrain[,!names(ttrain)%in%c("Id","cat")], method="lm", trControl=tr)
lm_caret$results
lm_caret$finalModel
par(mfrow=c(2,2))
plot(lm_caret$finalModel)
var_Imp <- varImp(lm_caret)
plot(var_Imp, top =10)

pre_ttest <- predict(lm_caret, ttest)
rmse_lm <- sqrt(mean((ttest$SalePrice - pre_ttest)^2))
print(rmse_lm)

lm_pre_test <- data.frame(test$Id,predict(lm_caret, test))
colnames(lm_pre_test) <- c("Id", "lm_pre")

lm_pre_test_export <- lm_pre_test
lm_pre_test_export$lm_pre <- expm1(lm_pre_test_export$lm_pre)
colnames(lm_pre_test_export) <- c("Id", "SalePrice")

write.csv(lm_pre_test_export,"C:/Users/212489345/Documents/class/Kaggle/house_pricing/prediction_lm.csv", 
          row.names = FALSE)


####GAM####
library(ggplot2)
num_var <- c(num_var, "neigh_sale")

#ggplot(train, aes(SalePrice, MSSubClass))+geom_point()+ geom_smooth()
ggplot(train, aes(SalePrice, neigh_sale))+geom_point()+ geom_smooth()
#ggplot(train, aes(SalePrice, OverallQual))+geom_point()+ geom_smooth()
#ggplot(train, aes(SalePrice, OverallCond))+geom_point()+ geom_smooth()

ggplot(train, aes(SalePrice, neigh_sale))+geom_point()+ geom_smooth()
ggplot(train, aes(SalePrice, TotalBsmtSF))+geom_point()+ geom_smooth()

gam_use_var <- c("LotFrontage", "YearBuilt", "YearRemodAdd", "TotalBsmtSF",
                 "GrLivArea", "GarageYrBlt", "neigh_sale")
gam_use_var <- c("YearBuilt", "neigh_sale", "GrLivArea")
####GAM Model######
set.seed(111)
# 10 fold CV repeated 5 times
tr <- trainControl(method = "repeatedcv", number = 10,
                   repeats = 5)
library(gam)
library(mgcv)


# use<-names(train)[c(205, 66)]
# dontuse<-names(train)[c(3:65,67:204)]
# 
# use<-names(train)[66]
# dontuse<-names(train)[c(3:65, 67:205)]

gam_var <- names(train)[names(train)%in%num_var]
gam_notuse_var <- names(train)[!names(train)%in%num_var]
gam_notuse_var <- gam_notuse_var[-c(1,2,length(gam_notuse_var))]

#3 degree is better than 2. more significant vars
form <- as.formula(
  paste0("SalePrice~", paste0("s(", gam_use_var,",3)", collapse = "+"), "+", 
         paste0(gam_notuse_var,collapse="+"),collapse="")
)

# form <- as.formula(
#   paste0("SalePrice~", paste0("s(", gam_use_var,",3)", collapse = "+"))
# )

#GAM cant' deal with both gaussian and poission!
#need to remove poission, use Gam to predict a new value, then use that value in lasso
# form <- as.formula(
#   paste0("SalePrice~", paste0("s(", use,")", collapse = "+"))
# )

# form <- as.formula(
#   paste0("SalePrice~", paste0("s(", paste(sprintf("`%s`", s)),")", collapse = "+"))
# )

gam <- gam(formula = form, data = ttrain, family=gaussian)
summary(gam)
am_caret <- train(SalePrice ~., data=ttrain[,!names(ttrain)%in%c("Id","cat")], 
                  method="gam", trControl=tr)

#for library(mclv)
# varimpor <- varImp(gam)
# varimpor$var <- row.names(varimpor)
# head(arrange(varimpor, desc(Overall)),10)


#!!outlier in ttest: id 251 633
#> View(ttest[row.names(ttest)==600,])
#> View(ttest[row.names(ttest)==241,])

#gam <- train(SalePrice ~., data=ttrain[,!names(ttrain) %in% c("Id","cat")], method="gam", trControl=tr)
pre_ttrain <- predict(gam_caret, ttrain)
rmse_gam <- sqrt(mean((ttrain$SalePrice - pre_ttrain)^2))
print(rmse_gam)


pre_ttest <- predict(gam_caret, ttest)
rmse_gam <- sqrt(mean((ttest$SalePrice - pre_ttest)^2))
print(rmse_gam)



#create a new varialbe for lasso based on gam
gam_pre_ttrain <- data.frame(ttrain$Id,predict(gam, ttrain))
gam_pre_ttest <- data.frame(ttest$Id,predict(gam, ttest))
gam_pre_test <- data.frame(test$Id,predict(gam_caret, test))
colnames(gam_pre_ttrain) <- c("Id", "gam_pre")
colnames(gam_pre_ttest) <- c("Id", "gam_pre")
colnames(gam_pre_test) <- c("Id", "gam_pre")

gam_pre_test_export <- gam_pre_test
gam_pre_test_export$gam_pre <- expm1(gam_pre_test_export$gam_pre)
colnames(gam_pre_test_export) <- c("Id", "SalePrice")

write.csv(gam_pre_test_export,"C:/Users/212489345/Documents/class/Kaggle/house_pricing/prediction_gam.csv", 
row.names = FALSE)

gam_pre <- rbind(gam_pre_ttrain, gam_pre_ttest, gam_pre_test)

##combined into new dat_all
dat_all <- merge(dat_all, gam_pre, by="Id", all.x = TRUE)

train <- dat_all[dat_all$cat == "train",]
test <- dat_all[dat_all$cat == "test",]

train <- merge(train_price, train, by="Id", all.y = TRUE)

part <- createDataPartition(train$SalePrice, p=0.8, list = FALSE)
ttrain <- train[part,]
ttest <- train[-part,]




###Decision tree model####
colnames(ttrain) <- gsub("`| ","",colnames(ttrain))
colnames(ttest) <- gsub("`| ","",colnames(ttest))
colnames(test) <- gsub("`| ","",colnames(test))

library(rpart)
tree <- rpart(SalePrice ~ .,
              data = ttrain[,!names(ttrain) %in% c("Id","cat")],
              method = "anova")
printcp(tree)
tree$variable.importance
# prune the tree based on cv
p_tree <- prune(tree, cp = tree$cptable[which.min(tree$cptable[,"xerror"]), "CP"]) 


rmse_tree_train <- sqrt(mean((ttrain$SalePrice - predict(tree))^2))
print(rmse_tree_train)


pre_ttest <- predict(tree, ttest[,!(colnames(ttest) %in% c("SalePrice", "Id", "cat"))])
rmse_tree <- sqrt(mean((ttest$SalePrice - pre_ttest)^2))
print(rmse_tree)










###GAM didn't perform better than simple lasso

library(glmnet)
x <- data.matrix(ttrain[,!colnames(ttrain)%in%c("Id", "SalePrice", "cat")])
y <- data.matrix(ttrain[,"SalePrice"])
test_Id <- test$Id
test <- data.matrix(test[, !colnames(test)%in%c("Id", "cat")])

################model lasso#############

cv_l1 <- cv.glmnet(x, y, alpha=1)
plot(cv_l1)
cv_l1$lambda.min
cv_l1$lambda.1se
coef(cv_l1)


coe_l1 <-data.frame(c("intercept",colnames(x))[c(which(coef(cv_l1)!=0))],coef(cv_l1)[which(coef(cv_l1)!=0)])
colnames(coe_l1) <- c("var", "coef")
coe_l1 <- coe_l1[order(-coe_l1$coef),] #what features matter


fit_l1 <-glmnet(x, y, alpha = 1, lambda = cv_l1$lambda.min)


pre2_l1 <- predict(fit_l1, 
                   data.matrix(ttest[, !colnames(ttest)%in%c("Id", "cat", "SalePrice")]))

rmse2_l1 <- sqrt(mean((ttest$SalePrice - pre2_l1)^2))
print(rmse2_l1) #0.1563409. #0.1090782 after removing outliers. latest: 0.110907 (after removing high correlation)


##########real predictions (0.12467, after removing 3 types of outliers)
pre_real_test_l1 <- predict(fit_l1, test)
pre_real_test_l1 <- as.data.frame(pre_real_test_l1)
pre_real_test_l1$Id <- test_Id
pre_real_test_l1 <- pre_real_test_l1[,c(2,1)]
colnames(pre_real_test_l1) <- c("Id", "SalePrice")
pre_real_test_l1$SalePrice <- expm1(pre_real_test_l1$SalePrice)

write.csv(pre_real_test_l1, "C:/Users/212489345/Documents/class/Kaggle/house_pricing/prediction_l1.csv", row.names = FALSE)














summary(as.factor(dat_all$ExterQual))






#########Add new features. avg. price by neighborhood############
##!!!!!!should not include its own price. except for its self, what's the mean price?

library(FNN)

dat_all <- merge(dat_all, train_price, by="Id", all.x = TRUE)

##for neighborhood that can find 10 nearest based on same scale of Lot Area
##for neighborhood that can't find 10 nearest based on same scale of Lot Area, use median accross the state
##neighborhood still tends to be more important than overall state. However, if same neighborhood but same living area, should be the most powerful

neiSal_hotkey <- as.data.frame(model.matrix(~.+0, data = as.data.frame(dat_all[,"Neighborhood"])))
colnames(neiSal_hotkey) <- gsub(".*`", "", colnames(neiSal_hotkey))
colnames(neiSal_hotkey) <- paste("f", colnames(neiSal_hotkey), sep = "_")

dat_all <- cbind(dat_all, neiSal_hotkey)

#version5. detailed knn doesn't help.
# dat_all$neigh_sale <- 0
# for (s in (1:nrow(dat_all))){
#   
#   test_dat <- dat_all[s,c("Id","SalePrice","Neighborhood","cat","GrLivArea", "OverallQual", "LotArea", "KitchenAbvGr")] #, colnames(neiSal_hotkey)
#   
#   train_dat <- dat_all[-s,c("Id","SalePrice","Neighborhood","cat","GrLivArea", "OverallQual", "LotArea", "KitchenAbvGr")] #, colnames(neiSal_hotkey)
#   train_dat <- train_dat[train_dat$Neighborhood == test_dat$Neighborhood & train_dat$cat != "test",]
#   # train_dat <- train_dat[train_dat$cat != "test",]
#   neiSal_index <- train_dat[,"Id"]
#   
#   neiSal_knn <- knn(train = train_dat[,-c(1,2,3,4)], #using varialbes that matter in LASSO regression
#                     test = test_dat[,-c(1,2,3,4)],
#                     cl=neiSal_index,
#                     k=10, #10 nearest neighbor is slight better than 1-5
#                     algorithm = "brute") 
#   
#   neiSal_indices <- attr(neiSal_knn, "nn.index")
#   
#   nei <- train_dat[train_dat$Id %in% train_dat[neiSal_indices,"Id"], ]
#   
#   neigh_sale<- data.frame(nei, test_dat)
#   neigh_sale$var <- abs(neigh_sale$GrLivArea.1 / neigh_sale$GrLivArea-1) 
#   median <- median(neigh_sale[neigh_sale$var<0.15,"SalePrice"])
#   dat_all[s,"neigh_sale"] <- median
# }
# 
# #for NAN data (neighborhood doesn't have enough samples), use similar GrLivArea across the state 
# row <- as.numeric(row.names(dat_all[is.na(dat_all$neigh_sale)==TRUE,]))
# 
# for (s in row){
#   
#   test_dat <- dat_all[s,c("Id","SalePrice","Neighborhood","cat","GrLivArea", "OverallQual", "LotArea", "KitchenAbvGr")] #, colnames(neiSal_hotkey)
#   
#   train_dat <- dat_all[-s,c("Id","SalePrice","Neighborhood","cat","GrLivArea", "OverallQual", "LotArea", "KitchenAbvGr")] #, colnames(neiSal_hotkey)
#   train_dat <- train_dat[train_dat$cat != "test",]
#   # train_dat <- train_dat[train_dat$cat != "test",]
#   neiSal_index <- train_dat[,"Id"]
#   
#   neiSal_knn <- knn(train = train_dat[,-c(1,2,3,4)], #using varialbes that matter in LASSO regression
#                     test = test_dat[,-c(1,2,3,4)],
#                     cl=neiSal_index,
#                     k=10, #10 nearest neighbor is slight better than 1-5
#                     algorithm = "brute") 
#   
#   neiSal_indices <- attr(neiSal_knn, "nn.index")
#   
#   nei <- train_dat[train_dat$Id %in% train_dat[neiSal_indices,"Id"], ]
#   
#   neigh_sale<- data.frame(nei, test_dat)
#   neigh_sale$var <- abs(neigh_sale$GrLivArea.1 / neigh_sale$GrLivArea-1) 
#   median <- median(neigh_sale[neigh_sale$var<0.15,"SalePrice"])
#   dat_all[s,"neigh_sale"] <- median
# }
# 
# ##last part. if again NA, remove criteria GrLiveArea < .15. take median of 10 nearest neighbor
# row <- as.numeric(row.names(dat_all[is.na(dat_all$neigh_sale)==TRUE,]))
# 
# for (s in row){
#   
#   test_dat <- dat_all[s,c("Id","SalePrice","Neighborhood","cat","GrLivArea", "OverallQual", "LotArea", "KitchenAbvGr")] #, colnames(neiSal_hotkey)
#   
#   train_dat <- dat_all[-s,c("Id","SalePrice","Neighborhood","cat","GrLivArea", "OverallQual", "LotArea", "KitchenAbvGr")] #, colnames(neiSal_hotkey)
#   train_dat <- train_dat[train_dat$cat != "test",]
#   # train_dat <- train_dat[train_dat$cat != "test",]
#   neiSal_index <- train_dat[,"Id"]
#   
#   neiSal_knn <- knn(train = train_dat[,-c(1,2,3,4)], #using varialbes that matter in LASSO regression
#                     test = test_dat[,-c(1,2,3,4)],
#                     cl=neiSal_index,
#                     k=10, #10 nearest neighbor is slight better than 1-5
#                     algorithm = "brute") 
#   
#   neiSal_indices <- attr(neiSal_knn, "nn.index")
#   
#   nei <- train_dat[train_dat$Id %in% train_dat[neiSal_indices,"Id"], ]
#   
#   neigh_sale<- data.frame(nei, test_dat)
#   neigh_sale$var <- abs(neigh_sale$GrLivArea.1 / neigh_sale$GrLivArea-1) 
#   median <- median(neigh_sale[,"SalePrice"])
#   dat_all[s,"neigh_sale"] <- median
# }
# 
# 
# dat_all$SalePrice <- NULL
# 



##version4: works the best.


dat_all$neigh_sale <- 0
for (s in (1:nrow(dat_all))){

  test_dat <- dat_all[s,c("Id","SalePrice","Neighborhood","cat","GrLivArea", "OverallQual", "LotArea", "KitchenAbvGr")] #, colnames(neiSal_hotkey)

  train_dat <- dat_all[-s,c("Id","SalePrice","Neighborhood","cat","GrLivArea", "OverallQual", "LotArea", "KitchenAbvGr")] #, colnames(neiSal_hotkey)
  train_dat <- train_dat[train_dat$Neighborhood == test_dat$Neighborhood & train_dat$cat != "test",]
  # train_dat <- train_dat[train_dat$cat != "test",]
  neiSal_index <- train_dat[,"Id"]

  neiSal_knn <- knn(train = train_dat[,-c(1,2,3,4)], #using varialbes that matter in LASSO regression
                    test = test_dat[,-c(1,2,3,4)],
                    cl=neiSal_index,
                    k=10, #10 nearest neighbor is slight better than 1-5
                    algorithm = "brute")

  neiSal_indices <- attr(neiSal_knn, "nn.index")

  median <- median(train_dat[train_dat$Id %in% train_dat[neiSal_indices,"Id"], "SalePrice"])

  dat_all[s,"neigh_sale"] <- median

}

dat_all$SalePrice <- NULL
dat_all<-dat_all[,!colnames(dat_all) %in% colnames(neiSal_hotkey)]

# 
#  
# #second try
# dat_all$neigh_sale <- 0
# for (x in (1:nrow(dat_all))){
#   id <- dat_all[x,1]
#   nei_train_id <- dat_all[dat_all$Neighborhood == dat_all[x,"Neighborhood"] & dat_all$Id != id & dat_all$cat=="train",1]
#   
#   nei_avg <- mean(train_price[train_price$Id %in% nei_train_id,"SalePrice"])
#   dat_all[x,"neigh_sale"] <- nei_avg
# }

# first try
# neigh_sale <- aggregate(train$SalePrice, list(train$Neighborhood), mean)
# colnames(neigh_sale) <- c("Neighborhood", "neigh_avg")
# 
# train <- merge(train, neigh_sale, by="Neighborhood", all.x = TRUE)
# test <- merge(test, neigh_sale, by="Neighborhood", all.x = TRUE)
# 
# train$SalePrice <- NULL
# dat_all <- rbind(train, test)


########Missing values################
#str(dat_all) #2919 obs of 81 var
#summary(dat_all)


##missing values
#library(Amelia)
#missmap(dat_all, main = "Missing values")

#check missing value rate. if missing value is high, then exclude this feature

missing_pcnt2 <- data.frame()
for (m in (1:length(colnames(dat_all)))){
  missing_pcnt <- round(nrow(dat_all[is.na(dat_all[,m])==TRUE,])/nrow(dat_all),2)
  missing_cnt <- nrow(dat_all[is.na(dat_all[,m])==TRUE,])
  missing_pcnt <- data.frame(colnames(dat_all)[m] ,missing_pcnt, missing_cnt)
  missing_pcnt2 <- rbind(missing_pcnt2, missing_pcnt)
}

#print (missing_pcnt2)

#exclude var missing values > 5%
# include_var <- missing_pcnt2[missing_pcnt2$missing_pcnt<=0.05,1]
# dat_all <- dat_all[,as.character(include_var)]

#missmap(dat_all, main = "Missing values")


##impute the missing values

#identify the features below will be included to conduct nearest neighbor search for imputation

##!!!!need to transform all categorical into numerica in order to perform KNN
##!!!!KNN logic is based on Neighborhood, then Lot Area, then bld type, then yr built
##in this way to fill the missing values. May not need KNN algorithm

#recheck missing data
# missing_pcnt2 <- data.frame()
# for (m in (1:length(colnames(dat_all)))){
#   missing_pcnt <- round(nrow(dat_all[is.na(dat_all[,m])==TRUE,])/nrow(dat_all),2)
#   missing_cnt <- nrow(dat_all[is.na(dat_all[,m])==TRUE,])
#   missing_pcnt <- data.frame(colnames(dat_all)[m] ,missing_pcnt, missing_cnt)
#   missing_pcnt2 <- rbind(missing_pcnt2, missing_pcnt)
# }
# #print(missing_pcnt2[missing_pcnt2$missing_cnt!=0,])


#transform "MSSubClass" "HouseStyle" "BldgType" into index
#make sure the included features to train knn has no missing values

dat_all$MSSubClass <- as.factor(dat_all$MSSubClass)
subclass <- as.data.frame(
  model.matrix(~ .+0, data = as.data.frame(dat_all[,"MSSubClass"])))

colnames(subclass) <- gsub(".*`", "", colnames(subclass))
colnames(subclass) <- paste("subcl", colnames(subclass), sep = "_")

dat_all <- cbind(dat_all, subclass)


hstyle <- as.data.frame(
  model.matrix(~ .+0, data = as.data.frame(dat_all[,"HouseStyle"])))
colnames(hstyle) <- gsub(".*`", "", colnames(hstyle))

dat_all <- cbind(dat_all, hstyle)


btype <- as.data.frame(
  model.matrix(~ .+0, data = as.data.frame(dat_all[,"BldgType"])))
colnames(btype) <- gsub(".*`", "", colnames(btype))

dat_all <- cbind(dat_all, btype)



# neigh <- as.data.frame(
#   model.matrix(~ .+0, data = as.data.frame(dat_all[,"Neighborhood"])))
# colnames(neigh) <- gsub(".*`", "", colnames(neigh))
# 
# dat_all <- cbind(dat_all, neigh)

##NEW first look at the same neighborhood, then use knn select nearest 10, then use medium to fill NA

#find which rows have missing and need to fill
#missing_pcnt2 <- missing_pcnt2[missing_pcnt2$missing_cnt>0,1][-c(7,8,9,10,11,12,21,23,26,27)] #exclue features that "NA" has a meaning

##missing values don't need to fill




# dat_all_backup <- dat_all #for later missing values inspection

# neighbor <- c("MSSubClass", "MSZoning", "Neighborhood", "BldgType", "HouseStyle", "YearBuilt",
#               "YearRemodAdd", "LotArea")

neighbor <- c("Neighborhood", "YearBuilt", "LotArea", colnames(btype), colnames(subclass), colnames(hstyle))
neighbor2 <- c("YearBuilt", "LotArea", colnames(btype), colnames(subclass), colnames(hstyle)) #not include neighborhood 

##missing values need to filled
missing_pcnt2 <- c("MSZoning", "LotFrontage", "Utilities", "Exterior1st", "Exterior2nd", "MasVnrType", "MasVnrArea",
                   "BsmtFinSF1", "BsmtFinSF2", "BsmtUnfSF", "TotalBsmtSF", "Electrical", "BsmtFullBath", "BsmtHalfBath",
                   "KitchenQual", "Functional", "GarageYrBlt", "GarageCars", "GarageArea", "SaleType")

library(FNN)

for (i in (1:length(missing_pcnt2))){
  te <- dat_all[is.na(dat_all[paste(missing_pcnt2[i])])==TRUE, c("Id",neighbor)]
  tr <- dat_all[is.na(dat_all[paste(missing_pcnt2[i])])==FALSE, c("Id",neighbor)]
  
  #one by one each observation
  for (j in (1:nrow(te))){
    #first pick all bldgs in the same neighborhood
    tr2 <- tr[tr$Neighborhood == te[j,"Neighborhood"],]
    
    #then use knn to select nearest 10
    #only include complete dataset for KNN
    # te2 <- te[complete.cases(te),]
    te2 <- te[j,] #!has to make sure the "neighbor" is complete
    tr2 <- tr2[complete.cases(tr2),]
    
    cl <- tr2[,"Id"]
    row.names(te2) <- te2$Id
    
    #check if k=10 is valid
    if (nrow(tr2) >10) {
      k <- knn(train=tr2[, neighbor2], test=te2[, neighbor2], cl,k=10, algorithm = "brute")
    } else {
      k <- knn(train=tr2[, neighbor2], test=te2[, neighbor2], cl,k=nrow(tr2)-1, algorithm = "brute")
    }
    
    # k <- knn(train=tr2[, neighbor2], test=te2[, neighbor2], cl,k=10, algorithm = "brute")
    
    indices <- attr(k, "nn.index")
    #print(indices[1,])
    
    
    #to fill NA
    #if it's a factor, use the most frequent. if it's numeircal, use medium
    tr_knn <- dat_all[dat_all$Id %in% c(tr2[indices[1,],"Id"]), missing_pcnt2[i]]
    
    if (class(tr_knn) == "character") {
      frequent <- rownames(as.data.frame(sort(table(tr_knn), decreasing = TRUE)[1]))
      dat_all[dat_all$Id == te2[1,1], paste(missing_pcnt2[i])] <- frequent
    } else {
      frequent <- median(tr_knn)
      dat_all[dat_all$Id == te2[1,1], paste(missing_pcnt2[i])] <- frequent
    }
    
    #View(dat_all[dat_all$Id %in% c(te2[1,1],tr2[indices[1,],"Id"]), ])
  }
}


#recheck missing data
missing_pcnt3 <- data.frame()
for (m in (1:length(colnames(dat_all)))){
  missing_pcnt <- round(nrow(dat_all[is.na(dat_all[,m])==TRUE,])/nrow(dat_all),2)
  missing_cnt <- nrow(dat_all[is.na(dat_all[,m])==TRUE,])
  missing_pcnt <- data.frame(colnames(dat_all)[m] ,missing_pcnt, missing_cnt)
  missing_pcnt3 <- rbind(missing_pcnt3, missing_pcnt)
}
print(missing_pcnt3[missing_pcnt3$missing_cnt!=0,])


#########add new features part two##############
##Add new features to transform categorical Quality to Numeric
##features include ExterQual, ExterCond, BsmtQual, BsmtCond, BsmtFinType1, 
##BsmtExposure, BsmtFinType2, HeatingQC, KitchenQual, 
##FireplaceQu, GarageQual, GarageCond, GarageFinish, PoolQC, PavedDrive, Fence

##!!!added after missing value imputation
dat_all2 <- dat_all[,-c(83:length(colnames(dat_all)))] #exclude knn features



# library(Amelia)
# missmap(dat_all)

# View(dat_all[dat_all$Id %in% c(40,1894, 2100),])
# View(dat_all_backup[dat_all_backup$Id %in% c(40,1894, 2100),])



#######transform data. log data#######

library(moments)
#below functions transform all factor variables into columns
#in order to fit glmnet 

c_class2 <- data.frame()
for (c in (1: length(colnames(dat_all2)))){
  c_class <- data.frame(colnames(dat_all2)[c],lapply(dat_all2, class)[[c]])
  c_class2 <- rbind(c_class2, c_class)
}
colnames(c_class2) <- c("colnames", "class")

#find only factor varialbe
var_numeric <- c_class2[c_class2$class %in% c("integer", "numeric"),1]

var_log <- var_numeric[which(abs(skewness(dat_all2[,var_numeric])) >0.75)]
dat_all2[c(var_log)] <- log1p(dat_all2[,var_log])

train_price$SalePrice <- log1p(train_price$SalePrice)


##identify and remove outliers

# outlier_price <- train_price[train_price$SalePrice >13 | train_price$SalePrice<11,"Id"]
# 
# dat_all2 <- dat_all2[!dat_all2$Id %in% outlier_price, ] #remove outliers
# 
# dat_all2 <- dat_all2[!dat_all2$LotArea>11,]
# 



###################preprossing data###################################
library(glmnet)


#below functions transform all factor variables into columns
#in order to fit glmnet 

#find only factor varialbe
var_factor <- c_class2[c_class2$class=="character",1]
var_factor <- var_factor[-length(var_factor)]#exclude "cat"

f_var2 <- as.data.frame(rep(0, nrow(dat_all2)))
for (f in (1:length(var_factor))){
  f_var <- as.data.frame(
    model.matrix(~ .+0, data = as.data.frame(dat_all2[,var_factor[f]])))
  colnames(f_var) <- gsub(".*`", "", colnames(f_var))
  colnames(f_var) <- paste("f",var_factor[f], colnames(f_var), sep = "_")
  f_var2 <- cbind(f_var2, f_var)
}

f_var2 <- f_var2[,-1]

dat_all2 <- cbind(dat_all2, f_var2)

dat_all2 <- dat_all2[,!colnames(dat_all2)%in% var_factor] #exclude factor variables


###remove features that not applied to test set from training. !!!the key that really helps########
skewness2 <- data.frame()
for (sk in (40:length(colnames(dat_all2)))){ #only consider factor transfomred variables
  skewness_train <- skewness(dat_all2[dat_all2$cat=="train", colnames(dat_all2)[sk]])
  skewness_test <- skewness(dat_all2[dat_all2$cat=="test", colnames(dat_all2)[sk]])
  skewness <- data.frame(skewness_train, skewness_test, sk)
  skewness2 <- rbind(skewness2, skewness)
}

exclude_col <- c(skewness2[is.na(skewness2$skewness_train)==TRUE,"sk"],
  skewness2[is.na(skewness2$skewness_test)==TRUE,"sk"])

dat_all2 <- dat_all2[,-exclude_col]


######split to train/test########

#split .7 vs .3
library(caTools)
set.seed(123)
split <- sample.split(train$SalePrice, SplitRatio = 0.7)
ttrain <- subset(train, split==TRUE)
ttest <- subset(train, split==FALSE)



###################model RIDGE#################
x <- data.matrix(ttrain[,!colnames(ttrain)%in%c("Id", "SalePrice", "cat")])
y <- data.matrix(ttrain[,"SalePrice"])
test_Id <- test$Id
test <- data.matrix(test[, !colnames(test)%in%c("Id", "cat")])


cv_l2 <- cv.glmnet(x, y, alpha=0)
plot(cv_l2)
cv_l2$lambda.min
cv_l2$lambda.1se

fit = glmnet(x, y, alpha = 0)
plot(fit)

pre <- predict(fit,
               as.matrix(ttest[, !colnames(ttest)%in%c("Id", "cat", "SalePrice")]))
rmse <- sqrt(mean((ttest$SalePrice - pre)^2))
print(rmse)


fit2 <- glmnet(x, y, alpha=0, lambda = cv_l2$lambda.min)
pre2 <- predict(fit2, 
               as.matrix(ttest[, !colnames(ttest)%in%c("Id", "cat", "SalePrice")]))

rmse2 <- sqrt(mean((ttest$SalePrice - pre2)^2))

print(rmse2)

print(rmse2 - rmse)


#######################Real prediction##################
pre_real_test <- predict(fit2, test)
pre_real_test <- as.data.frame(pre_real_test)
pre_real_test$Id <- test_Id
pre_real_test <- pre_real_test[,c(2,1)]
colnames(pre_real_test) <- c("Id", "SalePrice")
pre_real_test$SalePrice <- expm1(pre_real_test$SalePrice)

write.csv(pre_real_test, "C:/Users/212489345/Documents/class/Kaggle/house_pricing/prediction.csv", row.names = FALSE)



################model lasso#############

cv_l1 <- cv.glmnet(x, y, alpha=1)
plot(cv_l1)
cv_l1$lambda.min
cv_l1$lambda.1se
coef(cv_l1)


coe_l1 <-data.frame(c("intercept",colnames(x))[c(which(coef(cv_l1)!=0))],coef(cv_l1)[which(coef(cv_l1)!=0)])
colnames(coe_l1) <- c("var", "coef")
coe_l1 <- coe_l1[order(-coe_l1$coef),] #what features matter


fit_l1 <-glmnet(x, y, alpha = 1, lambda = cv_l1$lambda.min)


pre2_l1 <- predict(fit_l1, 
                data.matrix(ttest[, !colnames(ttest)%in%c("Id", "cat", "SalePrice")]))

rmse2_l1 <- sqrt(mean((ttest$SalePrice - pre2_l1)^2))
print(rmse2_l1) #0.1563409. #0.1090782 after removing outliers. latest: 0.1033733 #0.1172635 not overfitting much


##########real predictions
pre_real_test_l1 <- predict(fit_l1, test)
pre_real_test_l1 <- as.data.frame(pre_real_test_l1)
pre_real_test_l1$Id <- test_Id
pre_real_test_l1 <- pre_real_test_l1[,c(2,1)]
colnames(pre_real_test_l1) <- c("Id", "SalePrice")
pre_real_test_l1$SalePrice <- expm1(pre_real_test_l1$SalePrice)

write.csv(pre_real_test_l1, "C:/Users/212489345/Documents/class/Kaggle/house_pricing/prediction_l1_remove_outliers_neighavg3.csv", row.names = FALSE)


###################model knn: rmse: 0.23#############


cl_m <- ttrain[,"Id"]

#check if k=10 is valid
 
fit_knn <- knn(train = ttrain[,paste(coe_l1[-1,1])], #using varialbes that matter in LASSO regression
               test = ttest[,paste(coe_l1[-1,1])],
               cl=cl_m,
               k=10,
               algorithm = "brute")  
  
indices_m <- attr(fit_knn, "nn.index")

# print(indices_m[1,])
# 
# print(ttrain[ttrain$Id %in% ttrain[indices_m[1,],"Id"], "SalePrice"])
# print(ttest[ttest$Id == ttest[1,1], "SalePrice"])
# 
# mean(ttrain[ttrain$Id %in% ttrain[indices_m[1,],"Id"], "SalePrice"])
# median(ttrain[ttrain$Id %in% ttrain[indices_m[1,],"Id"], "SalePrice"])

#try median of 10nn

median_10_2 <- data.frame()
for (n in (1:nrow(ttest))){
  
  median_10 <- median(ttrain[ttrain$Id %in% ttrain[indices_m[n,],"Id"], "SalePrice"])
  
  median_10 <- data.frame(ttest[n,"Id"], median_10, ttest[n,"SalePrice"])
  median_10_2 <- rbind(median_10_2, median_10)
}

colnames(median_10_2) <- c("Id", "Pre_median", "Act")

median_10_2$dif <- (median_10_2$Act - median_10_2$Pre_median)^2
rmse <- sqrt(mean(median_10_2$dif))
print(rmse)




###################xgb#############
library(xgboost)
library(caret)
cv.ctrl <- trainControl(method = "repeatedcv", repeats = 1,number = 3)

xgb.grid <- expand.grid(nrounds = 800,
                        max_depth = seq(6,10),
                        eta = c(0.01,0.3, 1),
                        gamma = c(0.0, 0.2, 1),
                        colsample_bytree = c(0.5,0.8, 1),
                        min_child_weight=1,
                        subsample =1
)

xgb_tune <-train(SalePrice ~.,
                 data=ttrain[,!names(ttrain) %in% c("Id","cat")],
                 method="xgbTree",
                 metric = "RMSE",
                 trControl=cv.ctrl,
                 tuneGrid=xgb.grid
)
pre_xgb <- predict(xgb_tune, 
                   data.matrix(ttest[, !colnames(ttest)%in%c("Id", "cat", "SalePrice")]))

rmse_xgb <- sqrt(mean((ttest$SalePrice - pre_xgb)^2))
print(rmse_xgb) #0.1132603 No good than LASSO... real:0.1260442 :(

pre_real_test_xgb <- predict(xgb_tune, test)
pre_real_test_xgb <- as.data.frame(pre_real_test_xgb)
pre_real_test_xgb$Id <- test_Id
pre_real_test_xgb <- pre_real_test_xgb[,c(2,1)]
colnames(pre_real_test_xgb) <- c("Id", "SalePrice")
pre_real_test_xgb$SalePrice <- expm1(pre_real_test_xgb$SalePrice)

write.csv(pre_real_test_xgb, "C:/Users/212489345/Documents/class/Kaggle/house_pricing/prediction_xgb.csv", row.names = FALSE)


#############model stacking##############
#http://blog.kaggle.com/2016/12/27/a-kagglers-guide-to-model-stacking-in-practice/

pre_xgb_ttest <- as.data.frame(pre_xgb)
pre2_l1_ttest <- as.data.frame(pre2_l1)
pre_rf_ttest <- as.data.frame(rf_pr_ttest)

stacking <- cbind(pre_rf_ttest, pre2_l1_ttest, ttest$SalePrice, 
                  ttest$GrLivArea,ttest$OverallQual, ttest$YearBuilt, ttest$Neighborhood_n)
colnames(stacking) <- c("pre_rf","pre_lasso","SalePrice", "GrLivArea", "OverallQual","YearBuilt","Neighborhood_n")

###LM####
set.seed(111)
tr <- trainControl(method = "repeatedcv", number = 10, repeats = 5)
lm_caret <- train(SalePrice ~., data=stacking, method="lm", trControl=tr)

lm_caret$results
lm_caret$finalModel
par(mfrow=c(2,2))
plot(lm_caret$finalModel)
var_Imp <- varImp(lm_caret)
plot(var_Imp, top =10)

# pre_ttest <- predict(lm_caret, ttest)
# rmse_lm <- sqrt(mean((ttest$SalePrice - pre_ttest)^2))
# print(rmse_lm)
# 
# lm_pre_test <- data.frame(test$Id,predict(lm_caret, test))
# colnames(lm_pre_test) <- c("Id", "lm_pre")
# 
# lm_pre_test_export <- lm_pre_test
# lm_pre_test_export$lm_pre <- expm1(lm_pre_test_export$lm_pre)
# colnames(lm_pre_test_export) <- c("Id", "SalePrice")


scatterplotMatrix(~pre_rf+pre_lasso+SalePrice,stacking)

rmse_xgb <- sqrt(mean((ttest$SalePrice - ((pre_rf_ttest+pre2_l1_ttest)/2))^2))
rmse_xgb <- sqrt(mean((ttest$SalePrice - (0.3*pre_rf_ttest+.7*pre2_l1_ttest))^2))
print(rmse_xgb)

#real prediction
pre_real_stacking <- merge(pre_rf, pre_real_test_l1, by="Id")
pre_real_stacking <- merge(test, pre_real_stacking, by="Id")
colnames(pre_real_stacking)[c(188,189)] <- c("pre_rf","pre_lasso")

pre_stacking <- as.data.frame(predict(lm_caret, pre_real_stacking))


pre_stacking <- data.frame(test$Id,pre_stacking$`predict(lm_caret, pre_real_stacking)`)
colnames(pre_stacking) <- c("Id","SalePrice")


pre_real_stacking$SalePrice <- (0.2139*pre_real_stacking$SalePrice.x + 0.7719*pre_real_stacking$SalePrice.y + 0.1713)

pre_real_stacking$SalePrice <- (0.3*pre_real_stacking$SalePrice.x + 0.7*pre_real_stacking$SalePrice.y)
pre_real_stacking <- pre_real_stacking[,c("Id","SalePrice")]

write.csv(pre_stacking, "C:/Users/212489345/Documents/class/Kaggle/house_pricing/prediction_stacking.csv", row.names = FALSE)
scatterplotMatrix(~pre_xgb+pre_lasso+SalePrice, stacking)
plot(pre_xgb_ttest$pre_xgb, ttest$SalePrice)
plot(pre2_l1_ttest$s0, ttest$SalePrice)

#










param <- list(max_depth = 200,
              eta=0.1,
              gama=0.01)

cv_xgb <- xgb.cv(data = x, label = y, 
                 params = param, nfold = 5, nrounds = 200,
                 early_stopping_rounds = 5)


fit_xgb <- xgboost(data = x,
                   label = y,
                   params = param,
                   nrounds = 100)

pre_xgb <- predict(fit_xgb, test)

pre_xgb <- as.data.frame(pre_xgb)

pre_xgb$Id <- test_Id

pre_xgb <- pre_xgb[,c(2,1)]
colnames(pre_xgb) <- c("Id", "SalePrice")
pre_xgb$SalePrice <- expm1(pre_xgb$SalePrice)

write.csv(pre_xgb, "C:/Users/212489345/Documents/class/Kaggle/house_pricing/prediction_xgb2.csv", row.names = FALSE)

###next step: remove saleprice in trainning set outliers
##create more features!! based on importance features. some clues


############random forest: 0.15 RSME#################
library(caret)
tr <- trainControl(method = "repeatedcv", number = 10,
                   repeats = 5)

rf <- train(SalePrice ~ ., data=ttrain[,!names(ttrain)%in%c("Id", "cat")], method="rf", trControl=tr,
            importance =TRUE)
rf$results
varImp(rf)
plot(rf$finalModel)

rf_pr_ttest <- predict(rf, ttest[,!names(ttrain)%in%c("Id", "cat")])
rf_ttest_rmse <- sqrt(mean((ttest$SalePrice - rf_pr_ttest)^2))
print(rf_ttest_rmse)

pre_rf <- as.data.frame(predict(rf, test))
pre_rf <- data.frame(test$Id,pre_rf)
colnames(pre_rf) <- c("Id","SalePrice")
pre_rf$SalePrice <- expm1(pre_rf$SalePrice)

write.csv(pre_rf, "C:/Users/212489345/Documents/class/Kaggle/house_pricing/prediction_rf.csv", row.names = FALSE)


