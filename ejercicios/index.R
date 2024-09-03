library(MVN)

X=c(60, 61, 61, 62, 63, 64, 65, 68, 70)
W=c( 125, 130, 120, 135, 130, 140, 140, 160, 169)

data=data.frame(X,W)
result <- mvn(data, mvnTest="hz")
result$multivariateNormality
print(cor(X,W))

print(cor.test(X,W))
