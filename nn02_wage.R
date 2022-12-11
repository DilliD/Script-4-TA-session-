#install.packages("neuralnet")
library(reshape); library(neuralnet)

data = read.table("econ301hw.txt")
data = rename(data, c(V1 = "lnwage",
                      V2 = "age",
                      V3 = "male",
                      V4 = "vet",
                      V5 = "met",
                      V6 = "gpa",
                      V7 = "toeic",
                      V8 = "abro",
                      V9 = "intn",
                      V10 = "busi",
                      V11 = "engi",
                      V12 = "dble",
                      V13 = "pwage",
                      V14 = "fedu",
                      V15 = "medu"))

normalize = function(x){return( (x - min(x))/(max(x) - min(x) ) )  }
#normalize = function(x){return( (x - mean(x))/sd(x)  }
data$age = normalize(data$age);
#data$lnwage = normalize(data$lnwage);
data$gpa = normalize(data$gpa)
data$toeic = normalize(data$toeic);
data$pwage = normalize(data$pwage);
data$fedu = normalize(data$fedu);
data$medu = normalize(data$medu)

n=500
sn = sample(1:n, size = 0.8*n)
d.training= data[sn,]
d.test = data[-sn,]

eq1 = lnwage ~ age + male + vet + met + toeic + gpa + abro + intn + busi + engi + dble + pwage + fedu + medu

model_ols = neuralnet(eq1, d.training, hidden = 0, threshold = 0.02, stepmax = 1e+06)
pred_ols = compute(model_ols, d.test)

model_nn = neuralnet(eq1, d.training, hidden = c(2,2,1), threshold = 0.02, stepmax = 1e+06, algorithm = "rprop+")
#print(model_nn)
#plot(model_nn)

pred_nn = compute(model_nn, d.test)
          
result = cbind(d.test$lnwage, pred_nn$net.result, pred_ols$net.result)
print(result)

mse_nn = mean((d.test$lnwage - pred_nn$net.result)^2)
mse_ols = mean((d.test$lnwage - pred_ols$net.result)^2)

print(cbind(mse_nn, mse_ols))

