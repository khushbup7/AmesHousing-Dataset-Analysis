myData=as.data.frame(unclass(AmesHousing))
summary(myData)

myData = myData[,-1]
dim(myData)
myData = myData[,c("MS.SubClass","MS.Zoning", "Lot.Frontage","Lot.Area","Lot.Shape"
                   ,"Land.Contour","Lot.Config","Neighborhood","Bldg.Type","House.Style",
                    "Overall.Qual","Overall.Cond","Roof.Style",
                  "BsmtFin.Type.1","Bsmt.Unf.SF","X1st.Flr.SF","Gr.Liv.Area",
                   "Full.Bath","TotRms.AbvGrd","SalePrice")]
myData =  na.omit(myData)

dim(myData)
train = sample(2374,1500)
lr.model = lm(SalePrice~., data=myData, subset=train)
summary(lr.model)
lr.predict = predict(lr.model, myData[-train,-20])
MSE.lr = mean((lr.predict-myData[-train, 20])^2)
MSE.lr
sqrt(MSE.lr) #32449.07

dim(myData)
library(pls)
pcr.fit = pcr(SalePrice~., data=myData, scale = FALSE, subset=train, validation="CV")
validationplot(pcr.fit, val.type="MSEP")
summary(pcr.fit)
pcr.pred = predict(pcr.fit, myData[-train,-20], ncomp=1)
MSE.pcr = mean((pcr.pred - myData[-train,20])^2)
sqrt(MSE.lr)
MSE.pcr

library(glmnet)
x=model.matrix(SalePrice~.,myData)[,-1]
y=myData$SalePrice
# We create a sequence of lambdas to test
grid=10^seq(10,-2,length=100)
# We now do Ridge reqression for each lambda
ridge.mod=glmnet(x,y,alpha=0,lambda=grid)
dim(coef(ridge.mod))
names(ridge.mod)
# We can access the lambda values, but the coeff we need to access
# separately
ridge.mod$lambda [50]
coef(ridge.mod)[,50]
# The predict function allows us to calculate the coefficients
# for lambdas that were not in our original grid
# Here are the coefficients for lambda = 50 ...
predict(ridge.mod,s=50,type="coefficients")[1:20,]
# We would like to choose an optimal lambda - we'll demonstrate
# a simple CV method here (K-fold can be used with more work).
set.seed(1)
#train=sample(1:nrow(x), nrow(x)/2)
test=(-train)
y.test=y[test]
ridge.mod=glmnet(x[train,],y[train],alpha=0,lambda=grid, thresh=1e-12)
# We need to find the optimal lambda - we can use CV
set.seed(1)
cv.out=cv.glmnet(x[train,],y[train],alpha=0)
# The cv function does 10-fold CV by default
plot(cv.out)
bestlam=cv.out$lambda.min
bestlam
ridge.pred=predict(ridge.mod,s=bestlam,newx=x[test,])
sqrt(mean((ridge.pred-y.test)^2)) #31854.3
 #Standard erroy for salary
# ONce we do CV find the best lambda, we can use all of the data
# to build our model using this lambda ...
ridge.model=glmnet(x,y,alpha=0)
predict(ridge.model,type="coefficients",s=bestlam)[1:20,]
# Note all coef are included, but some a weighted heavier than others
# Now we apply the Lasso - note all we do is change the alpha option
lasso.mod =glmnet (x[train ,],y[train],alpha =1, lambda =grid)
plot(lasso.mod)
# Note that Lasso takes certain coeff to zero for large enough lambda
# We'll do CV to find the best lambda again ...
set.seed (1)
cv.out =cv.glmnet (x[train ,],y[train],alpha =1)
plot(cv.out)
# lambda small - too much variance | too large - too much bias
bestlam =cv.out$lambda.min
lasso.pred=predict (lasso.mod,s=bestlam,newx=x[test,])
sqrt(mean(( lasso.pred -y.test)^2)) #32219.42
# The MSE is similar to what we saw for Ridge
# Let's find the coefficients ...
out=glmnet (x,y,alpha =1, lambda =grid)
lasso.coef=predict(out,type ="coefficients",s=bestlam )[1:20,]
lasso.coef
lasso.coef[lasso.coef!=0]

accuracy = 1 - 31854.3/182200
accuracy
