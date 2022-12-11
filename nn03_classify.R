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
data$lnwage = normalize(data$lnwage);
data$gpa = normalize(data$gpa)
data$toeic = normalize(data$toeic);
data$pwage = normalize(data$pwage);
data$fedu = normalize(data$fedu);
data$medu = normalize(data$medu)

n=500
sn = sample(1:n, size = 0.8*n)
d.training= data[sn,]
d.test = data[-sn,]

cutoff = 0.6;

eq1 = vet ~ age + toeic + gpa + abro + intn + busi + engi + dble + pwage + fedu + medu + lnwage

model_ols = neuralnet(eq1, d.training, hidden = 0, threshold = 0.02, stepmax = 1e+06, algorithm = "rprop+")
pred_ols = compute(model_ols, d.test)
vet_ols = pred_ols$net.result
vet_ols = as.numeric((vet_ols>=cutoff))

model_nn = neuralnet(eq1, d.training, hidden = c(3,3,1), threshold = 0.02, stepmax = 1e+06,
                     algorithm = "rprop+")
#print(model_nn)
#plot(model_nn)

pred_nn = compute(model_nn, d.test)
vet_nn = pred_nn$net.result
vet_nn = as.numeric((vet_nn>=cutoff))

result = cbind(d.test$vet, vet_nn, vet_ols)
print(result)

prate_nn = mean(as.numeric(vet_nn == d.test$vet))
prate_ols = mean(as.numeric(vet_ols == d.test$vet))
print(cbind(prate_nn, prate_ols))
