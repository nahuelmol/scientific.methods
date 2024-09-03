library(dplyr)
library(MVN)
library(knitr)
library(readr)
library(ggplot2)
library(patchwork)
library(nortest)

yp <-c(6.40, 15.05, 18.75, 30.25, 44.85, 48.85, 51.55, 61.50, 100.44, 111.42)
x1 <-c(1.32, 2.69, 3.56, 4.41, 5.35, 6.20, 7.12, 8.87, 9.80, 10.65)
x2 <-c(1.15, 3.40, 4.10, 8.75, 14.82, 15.15, 15.32, 18.18, 35.19, 40.40)
datos<-data.frame(yp, x1, x2)

library(paletteer)
colors = paletteer_c("scico::berlin", n=3)

plot(
  x = iris$Petal.Length,
  y = iris$Petal.Width,
  bg = colors[ unclass(iris$Species) ],
  cex = 3,
  pch=21
)

height <- c(176, 154, 138, 196, 132, 176, 181, 169, 150, 175)
bodymass <- c(82, 49, 53, 112, 47, 69, 77, 71, 62, 78)


lm(height ~ bodymass)
plot(bodymass, height, 
     pch = 16, cex = 1.3, 
     col = "blue", 
     main = "HEIGHT PLOTTED AGAINST BODY MASS", 
     xlab = "BODY MASS (kg)", ylab = "HEIGHT (cm)")

abline(98.0054, 0.9528)

###########################
#CORRELOGRAMA
corrgram(mtcars, order=TRUE, lower.panel=panel.shade, upper.panel=panel.pie, text.panel=panel.txt, main="Car Milage Data in PC2/PC1 Order") 
###########################

###########################
par(mfrow = c(1, 1))
plot(mtcars$wt, mtcars$mpg,
     main = "millas por gallon VS weigth",
     sub = "plot",
     xlab = "peso",
     ylab = "millas por galón",
     cex.lab = 1.2,
     cex.axis = 1,
     mgp = c(2.4, 1, 0),
     pch = 19,
     col = "black")

df <- data.frame(mtcars$mpg, mtcars$wt)
colnames(df) <- c("peso", "millas/galón")
pairs.panels(df, method = "pearson",
             main = "Correlación mpg / wt",
             cex.labels = 1.5,
             cex.cor = 1, stars = TRUE,
             pch = 20,
             gap = 0, 
             lm = TRUE, col = "#2ECC71", 
             hist.col = "#2ECC71")

####################################
#medidas de tendencia central
mod <- lm(mpg ~ wt, data = mtcars) # mpg ~ wt
summary(mod)
mean(mod$residuals)

####################################
plot(mod$residuals)
qqPlot(mod$residuals, 
       distribution = "norm",
       main = "Q-Q PLOT de residuos en mpg ~ wt",
       xlab = "cuantiles teóricos",
       ylab = "cuantiles de la muestra",
       id = FALSE, grid = TRUE,
       envelope = 0.95, col = carPalette()[1], col.lines = carPalette()[3],
       pch = 20,
       cex = 1,
       lwd = 2)
qqline(mod$residuals,
       col = "blue",
       lty = 1,
       lwd = 2)   

#########################
#VALIDACION DE SUPUESTOS
#########################
#supuesto de normalidad
#Shapiro-Wilk
shapiro.test(mod$residuals)

#Anderson-Darling
ad.test(mod$residuals)

#Lilliefors
lillie.test(mod$residuals)

#supuesto de independencia
#primero se busca graficamente, si existiese, algun patron
ggplot(dwdata,aes(x=orden,y=residuos))+
  geom_point(color = "#013ADF" ,
             fill = "#013ADF" , size = 4, shape = 18, alpha = 0.5)+xlab("orden"
  )+
  geom_abline(slope = 0)

#Durbin-Watson
library(lmtest)
dwtest(mod,alternative="two.sided",iterations = 1000)

#########################
#supuesto de homoedasticidad 
#H0 -> homocedasticidad, H1 -> no homocedasticidad

#-> Breusch-Pagan
library(lmtest)
bptest(mod)

#-> Goldfeld-Quandt
library(lmtest)
gqtest(mod, order.by = ~ gashog2d, data=sumaria_2015)


#-> White, no existe una función que 
#de forma directa realice el test de White en R
#########################
#OUTLIERS
#########################
#Para controlar el nivel de significación global 
#del contraste se puede aplicar la corrección del nivel 
#por Bonferroni, estableciendo como hipótesis de nulidad: Ninguna de las observaciones es un outlier versus
#alguna lo es

#library(car)
outlierTest(mod)
influenceIndexPlot(mod, vars="Bonf", las=1,col="blue")

#########################
#LEVERAGE
#########################

library(stats)
cota=3 * mean(hatvalues(mod))
leverage <- hatvalues(mod) > cota
cbind(hatvalues(mod),cota,leverage)


#########################
#DISTANCIAS de COOK (medidas de influencia)
#########################

library(car)
influenceIndexPlot(mod,vars="Cook",las=1,col="blue")

#########################
#DFFITS
#########################

df <- mod$df.residual
p <- length(mod$coeﬀicients)
n <- nrow(mod$mod)
dﬀits_crit = 2 * sqrt(p / n)
dﬀits <- dﬀits(mod)
df <- data.frame(obs = names(dﬀits), dﬀits = dﬀits)
ggplot(df, aes(y = dﬀits, x = obs)) + 
  geom_point(color="#013ADF" ) +
  geom_hline(yintercept = c(dﬀits_crit, -dﬀits_crit), 
        linetype="dashed" ) + 
        labs(title = "DFFITS",
              subtitle = "Observaciones Influyentes" ,
              x = "Orden de Observación" ,
              y = "DFFITS" )+theme_bw()

#########################
#DFBETAS
#########################
#esta función entrega los valores estandarizados
dfbetas(mod)[,2]> 1
