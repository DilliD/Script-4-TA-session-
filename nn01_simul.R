#install.packages("neuralnet")
library(neuralnet); library(reshape)

n=1000;
x1 = 1 + rnorm(n); x2 = 0.5*rnorm(n); x3 = 1.24*rnorm(n)
x12 = x1*x2; x13 = x1*x3; x23 = x2*x3; x123 = x1*x2*x3; x1233 = x1*x2*x3*x3
#y = 1 + 0.2*x12 + 0.3*x13 + 0.2*x23 + 0.5*rnorm(n)
y = 1 + 0.5*x12 + 0.5*x13 + 0.5*x23 -0.001*x123 - 0.0001*x1233 + 0.5*rnorm(n)

data = cbind(y, x1, x2, x3)

sn = sample(1:n, size = 0.8*n)
dt = data[sn,]
dp = data[-sn,]

eq1 = y ~ x1 + x2 + x3

model_ols = neuralnet(eq1, dt, hidden = 0, threshold = 0.05,
                      stepmax = 1e+06, algorithm = "rprop+")
pred_ols = compute(model_ols, dp)

model_n = neuralnet(eq1, dt, hidden = c(2,2,1), threshold = 0.05,
                    stepmax = 1e+06, algorithm = "rprop+")

print(model_n)
plot(model_n)

pred_n = compute(model_n, dp)

summary(model_n)

result = cbind( dp[1:200,1], pred_n$net.result, pred_ols$net.result )
print(result)

mse_n = mean( (dp[1:200,1] - pred_n$net.result)^2 )
mse_ols = mean( (dp[1:200,1] - pred_ols$net.result)^2 )
print(cbind(mse_n, mse_ols))

