
library(data.table)
library(glmnet)
library(stargazer)
library(ggplot2)
library(knitr)
library(dplyr)
library(neuralnet)



setwd("D:\\725 project")
mkt_data_fact <- fread(file = "mkt_data_fact.csv")


y <- mkt_data_fact$ave_price
x1 <-mkt_data_fact$ave_distance
x2 <- mkt_data_fact$ave_passengers
x3 <-mkt_data_fact$market_size
x4 <-mkt_data_fact$hub_flag
x5 <- mkt_data_fact$vacation_flag
x6 <- mkt_data_fact$income_geo_mean
x7 <- mkt_data_fact$slot_flag
x8 <- mkt_data_fact$period

n= 5697
perc_train = 0.5


# Neural Networks Estimation


set.seed(0)

train <- sample.int(5697, 2849, replace = F)

data <- data.frame(y, x1, x2, x3, x4, x5, x6, x7, x8)
names(data) <- c("yvar","x1","x2","x3","x4","x5","x6","x7", "x8")


# adjustments for scaling
maxs <- apply(data, 2, max) 
mins <- apply(data, 2, min)
data_n <- as.data.frame(scale(data,center = mins, scale = maxs - mins))

nn <- neuralnet(yvar ~ x1+ x2+ x3+ x4+x5+x6 +x7+x8    , data = data_n[train,], hidden = 15,
                linear.output = F, learningrate=0.001)
nnfit <- compute(nn, data_n[-train,]) # scaled fit


pr.nn_ <- nnfit$net.result*(max(data$y)-min(data$y))+min(data$y) # un-scale our fit
test.r <- (data_n[-train,]$y)*(max(data$y)-min(data$y))+min(data$y)

MSE.nn <- mean((test.r - pr.nn_)^2) # get MSE

stargazer(MSE.nn, summary = FALSE, rownames = FALSE, type="text", 
          title = "MSE of ANN", out="MSE-ANN.txt")


par(mfrow=c(2,4))
gwplot(nn,selected.covariate = "x1")
gwplot(nn,selected.covariate = "x2")
gwplot(nn,selected.covariate = "x3")
gwplot(nn,selected.covariate = "x4")
gwplot(nn,selected.covariate = "x5")
gwplot(nn,selected.covariate = "x6")
gwplot(nn,selected.covariate = "x7")
gwplot(nn,selected.covariate = "x8")

dev.off()
dev.new()
plot(nn)
