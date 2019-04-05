test1<-read.csv("test.csv")
train1<-read.csv("train.csv")
sale_price<-train1$SalePrice

library(dplyr)
train2 <- select(train1, -SalePrice)
data<-rbind(test1,train2)
 data <- select(data,-Street,-Utilities,-RoofMatl,-MiscFeature,-GarageQual,-Alley,-PoolQC,-MiscVal,-PoolArea,-X3SsnPorch,-Fence,-Heating,-MasVnrArea,-BsmtHalfBath,-MSZoning,-Condition2,-Id,-MSSubClass,-Condition1,-Neighborhood,-YearBuilt,-RoofStyle,-Exterior1st,-MasVnrType,-BsmtFinType2,-SaleCondition,-BsmtFinSF1,-BsmtFinSF2,-Heating,-Electrical,-GarageArea,-GarageCond,-TotalBsmtSF,-GrLivArea,-GarageFinish,-GarageYrBlt,-BsmtFinSF2,-BsmtFinSF1,-BsmtUnfSF,-CentralAir,-GarageType,-YrSold,-TotRmsAbvGrd)

View(data)
dim(data)
summary(data)

str(data)
table(data$BsmtUnfSF)

m=mean(data$LotFrontage,na.rm = TRUE)
data$LotFrontage[is.na(data$LotFrontage)]=m

m=mean(data$BsmtFinSF1,na.rm = TRUE)
data$BsmtFinSF1[is.na(data$BsmtFinSF1)]=m

m=mean(data$BsmtFinSF2,na.rm = TRUE)
data$BsmtFinSF2[is.na(data$BsmtFinSF2)]=m

m=mean(data$BsmtUnfSF,na.rm = TRUE)
data$BsmtUnfSF[is.na(data$BsmtUnfSF)]=m

m=mean(data$TotalBsmtSF,na.rm = TRUE)
data$TotalBsmtSF[is.na(data$TotalBsmtSF)]=m

m=mean(data$BsmtFullBath,na.rm = TRUE)
data$BsmtFullBath[is.na(data$BsmtFullBath)]=m

m=mean(data$HalfBath,na.rm = TRUE)
data$HalfBath[is.na(data$HalfBath)]=m

m=mean(data$GarageYrBlt,na.rm = TRUE)                    
data$GarageYrBlt[is.na(data$GarageYrBlt)]=m

m=mean(data$GarageCars,na.rm = TRUE)
data$GarageCars[is.na(data$GarageCars)]=m

m=mean(data$GarageArea,na.rm = TRUE)
data$GarageArea[is.na(data$GarageArea)]=m







# data$LotFrontageimp<- ifelse(is.na(data$LotFrontage), mean(data$LotFrontage, na.rm=TRUE), data$LotFrontage)
# a<-as.factor(data$LotFrontage)
# b<-as.numeric(a,na.rm=TRUE)
# class(data$LotFrontage)
class(data$MSZoning)
data$MSZoning<-as.character(data$MSZoning)
####################mean 
m=round(mean(data$OpenPorchSF,na.rm = TRUE))
data$OpenPorchSF[is.na(data$OpenPorchSF)]=m
table(data$SaleType)
summary(data)

###########mode

  xtab <- table(data$MSSubClass) 
  m <- names(which(xtab == max(data$MSSubClass))) 
  data$MSSubClass[is.na(data$MSSubClass)]=m
  
  table(data$MSSubClass)
  
  class(data$MSSubClass)
  data$MSSubClass=as.character(data$MSSubClass)
  
summary(data$MSSubClass)


xtab <- table(data$Exterior1st) 
m <- names(which(xtab == max(xtab))) 
data$Exterior1st[is.na(data$Exterior1st)]=m
table(data$Exterior1st)
class(data$Exterior1st)
data$Exterior1st=as.character(data$Exterior1st)
summary(data$Exterior1st)


xtab <- table(data$Exterior2nd) 
m <- names(which(xtab == max(xtab))) 
data$Exterior2nd[is.na(data$Exterior2nd)]=m
table(data$Exterior2nd)
class(data$Exterior2nd)
data$Exterior2nd=as.character(data$Exterior2nd)
summary(data$Exterior2nd)

xtab <- table(data$MasVnrType) 
m <- names(which(xtab == max(xtab))) 
data$MasVnrType[is.na(data$MasVnrType)]=m

table(data$MasVnrType)

class(data$MasVnrType)
data$MasVnrType=as.character(data$MasVnrType)

summary(data$MasVnrType)


xtab <- table(data$BsmtQual) 
m <- names(which(xtab == max(xtab))) 
data$BsmtQual[is.na(data$BsmtQual)]=m

table(data$BsmtQual)

class(data$BsmtQual)
data$BsmtQual=as.character(data$BsmtQual)

summary(data$BsmtQual)

xtab <- table(data$BsmtCond) 
m <- names(which(xtab == max(xtab))) 
data$BsmtCond[is.na(data$BsmtCond)]=m

table(data$BsmtCond)

class(data$BsmtCond)
data$BsmtCond=as.character(data$BsmtCond)

summary(data$BsmtCond)

xtab <- table(data$BsmtExposure) 
m <- names(which(xtab == max(xtab))) 
data$BsmtExposure[is.na(data$BsmtExposure)]=m
table(data$BsmtExposure)
class(data$BsmtExposure)
data$BsmtExposure=as.character(data$BsmtExposure)
summary(data$BsmtExposure)

xtab <- table(data$BsmtFinType1) 
m <- names(which(xtab == max(xtab))) 
data$BsmtFinType1[is.na(data$BsmtFinType1)]=m
table(data$BsmtFinType1)
class(data$BsmtFinType1)
data$BsmtFinType1=as.character(data$BsmtFinType1)
summary(data$BsmtFinType1)


xtab <- table(data$BsmtFinType2) 
m <- names(which(xtab == max(xtab))) 
data$BsmtFinType2[is.na(data$BsmtFinType2)]=m
table(data$BsmtFinType2)
class(data$BsmtFinType2)
data$BsmtFinType2=as.character(data$BsmtFinType2)
summary(data$BsmtFinType2)


xtab <- table(data$Electrical) 
m <- names(which(xtab == max(xtab))) 
data$Electrical[is.na(data$Electrical)]=m
table(data$Electrical)
class(data$Electrical)
data$Electrical=as.character(data$Electrical)
summary(data$Electrical)

xtab <- table(data$Functional) 
m <- names(which(xtab == max(xtab))) 
data$Functional[is.na(data$Functional)]=m
table(data$Functional)
class(data$Functional)
data$Functional=as.character(data$Functional)
summary(data$Functional)

xtab <- table(data$GarageType) 
m <- names(which(xtab == max(xtab))) 
data$GarageType[is.na(data$GarageType)]=m
table(data$GarageType)
class(data$GarageType)
data$GarageType=as.character(data$GarageType)
summary(data$GarageType)

xtab <- table(data$GarageYrBlt) 
m <- names(which(xtab == max(xtab))) 
data$GarageYrBlt[is.na(data$GarageYrBlt)]=m
table(data$GarageYrBlt)
class(data$GarageYrBlt)
data$GarageYrBlt=as.character(data$GarageYrBlt)
summary(data$GarageYrBlt)


xtab <- table(data$GarageCond) 
m <- names(which(xtab == max(xtab))) 
data$GarageCond[is.na(data$GarageCond)]=m
table(data$GarageCond)
data$GarageCond=as.character(data$GarageCond)

xtab <- table(data$SaleType) 
m <- names(which(xtab == max(xtab))) 
data$SaleType[is.na(data$SaleType)]=m
table(data$SaleType)
data$SaleType=as.character(data$SaleType)


xtab <- table(data$GarageType) 
m <- names(which(xtab == max(xtab))) 
data$GarageType[is.na(data$GarageType)]=m
table(data$GarageType)
data$GarageType=as.character(data$GarageType)

xtab <- table(data$GarageFinish) 
m <- names(which(xtab == max(xtab))) 
data$GarageFinish[is.na(data$GarageFinish)]=m
table(data$GarageFinish)
data$GarageFinish=as.character(data$GarageFinish)


xtab <- table(data$MSZoning) 
m <- names(which(xtab == max(xtab))) 
data$MSZoning[is.na(data$MSZoning)]=m
table(data$MSZoning)
data$MSZoning=as.character(data$MSZoning)


xtab <- table(data$FireplaceQu) 
m <- names(which(xtab == max(xtab))) 
data$FireplaceQu[is.na(data$FireplaceQu)]=m
table(data$FireplaceQu)
data$FireplaceQu=as.character(data$FireplaceQu)
summary(data$FireplaceQu)

xtab <- table(data$KitchenQual) 
m <- names(which(xtab == max(xtab))) 
data$KitchenQual[is.na(data$KitchenQual)]=m
table(data$KitchenQual)
data$KitchenQual=as.character(data$KitchenQual)
summary(data$KitchenQual)



nrow(data)
t1<-data[1460:2919,]
t2<-data[1:1459,]
t1<-cbind(t1,sale_price)
model<-lm(sale_price~.,data=t1)
model
result=predict(model,t2)

a<-cbind(t1$Id,result)
write.csv(result,file='Housing Price.csv')
summary(model)
colSums(is.na(data))
class(data$MSSubClass)
data$MSSubClass<-as.factor(data$MSSubClass)
data$MSSubClass<-as.numeric(data$MSSubClass)

class(data$GarageType)
data$GarageType<-as.factor(data$GarageType)
data$GarageType<-as.numeric(data$GarageType)

levels(t1$Condition2)
levels(t2$Condition2)
table(t1$HouseStyle)
table(t2$HouseStyle)
table(data$HouseStyle)
