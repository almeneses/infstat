###################################################################################
###################################################################################
##Script clase 6#
################################################################################### 
###################################################################################

#MODELO DE REGRESIÓN LINEAL SIMPLE Y MÚLTIPLE

rm(list = ls())

setwd("~/Documentos")

#Regresión cuadrática
precio = c(79,79,79,79,79,99,99,99,99,99,119,
           119,119,119,119) 

n_paquetes = c(142,151,163,168,176,91,100,107
               ,115,126,77,86,95,100,106)

#y=Número de páquetes : variable de respuesta
#x=precio: variable independiente


#Diagrama de dispersión
plot(precio, n_paquetes, pch = 19)

#Correlación: -1 <= cor <= 1
cor(precio, n_paquetes)

#Modelo de relación lineal
modelo.precio <- lm(n_paquetes ~ precio)

class(modelo.precio )

names(modelo.precio)

summary(modelo.precio)

anova(modelo.precio)

precio2 <- precio*precio

modelo.cuadratico = lm(n_paquetes~precio+precio2)
summary(modelo.cuadratico)

cor(precio, precio2)

#####################################################################################
#####################################################################################

gasto =  c(2.0, 2.5, 2.5, 3.0, 4.0, 4.5, 5.5, 7.0, 4.5, 8.0)
beneficio = c(6593, 7053, 7137, 7295, 8227, 8451,
              12303, 25698, 10485, 46621)


plot(gasto, beneficio, pch = 19)
bene.ln = log(beneficio)

mod1.bene = lm(beneficio ~ gasto)
mod2.bene = lm(bene.ln ~ gasto)

summary(mod1.bene)
summary(mod2.bene)


#Ejemplo usando simulación
x1 <- runif(200, 2, 3)
x2 <- 3*x1 + rnorm(200, 0, 1)
x3 <- rnorm(200, 4 , 2)
error <- rnorm(200, 0, 1)

y <- 1.2 * x1 - 0.7 * x2 + x3 + error

Datos <- cbind.data.frame(y, x1, x2, x3)

cor(Datos)

modelo1 <- lm(y ~ x1 + x2 + x3, data = Datos )
summary(modelo1)

modelo2 <- lm(y ~  x2 + x3, data = Datos )
summary(modelo2)



par(mfrow = c(2,2))
plot(y~x1, data = Datos)
plot(y~x2, data = Datos)
plot(y~x3, data = Datos)



data(mtcars)

library(corrplot)

corrplot(cor(mtcars), method="circle")
corrplot(cor(mtcars), method="number")

#Modelo 1
m1 <- lm(mpg ~ ., data = mtcars)
summary(m1)

m2 <- lm(mpg ~ wt, data = mtcars)
summary(m2)

#mpg: consumo
#cyl: número de cilindros
#wt: peso

m3 <- lm(mpg ~ cyl + wt, data = mtcars)
summary(m3)

residuales <- m3$residuals

###########################################################################
#Validación de los supuestos
###########################################################################

#Media cero
t.test(residuales )

#Normalidad
shapiro.test(residuales )

#Independencia
library(randtests)
runs.test(residuales)

#Multicolinealidad
library("car")

barplot( vif(m3),
         main      = "VIF",
         horiz     = FALSE,
         col       = "steelblue",
         cex.names = 0.8)
abline(h = 2.5, lwd = 2, lty = 2, col='red')

#Indentificar el problema de multicolinealidad:
#Analice la magnitud de la multicolinealidad considerando el tamaño del.
#Una regla general es que si entonces la multicolinealidad es alta
#(también se usa comúnmente un límite de 5). Factor de inflación de la varianza
#Allison, P. D. (1999). Multiple Regression: A Primer. Thousand Oaks, CA: Pine Forge Press. p. 142.
#Hair, J. F.; Anderson, R.; Tatham, R. L.; Black, W. C. (2006). Multivariate Data Analysis. Upper Saddle River, NJ: Prentice Hall.



####################################################################################
#MODELO DE REGRESIÓN BAJO UN ENFOQUE DE MACHINE LEARNING
####################################################################################

#Inicializar la libreria h2o
install.packages("h2o")

library(h2o)

h2o.init()

#Cargar datos
datos <- read.csv("vino.csv")

corrplot(cor(datos), method="number")

cor(datos)
pairs(datos, lower.panel = NULL, main = "Matriz de dispersión",
      labels = names(datos), col = "blue")


indices <- sample(x = nrow(datos), size = 0.8*( nrow(datos) ) )

datos_entrenamiento <- datos[indices, ]
datos_test <- datos[-indices, ]

datos_entrenamiento <- as.h2o(datos_entrenamiento)
datos_test <- as.h2o(datos_test)

predictores <- names(datos)[-1]
objetivo <- "alcohol"

modelo_1_glm <- h2o.glm(family = "gaussian",
                        x = predictores,
                        y = objetivo,
                        training_frame = datos_entrenamiento,
                        compute_p_values = TRUE,
                        lambda = 0)



predictores_1 <- c("malic_acid", "ash", "alcalinity_of_ash", "magnesium")


modelo_2_glm <- h2o.glm(family = "gaussian",
                        x = predictores_1,
                        y = objetivo,
                        training_frame = datos_entrenamiento,
                        compute_p_values = TRUE,
                        lambda = 0)




estimacion_modelo <- h2o.predict(modelo_2_glm , newdata = datos_test)

datos_test <- as.data.frame(datos_test)

estimacion_modelo <- as.data.frame(estimacion_modelo )


plot(datos_test$alcohol, 
     estimacion_modelo$predict,
     pch = 19)


Datos_Obs_Est_Res <- cbind.data.frame("Obs" = datos_test$alcohol,
                                      "Est" = estimacion_modelo$predict,
                                      "Res" = datos_test$alcohol - estimacion_modelo$predict)

library(ggplot2)

ggplot(Datos_Obs_Est_Res,
       aes(x = Obs,
           y = Res))+
  geom_point()

plot(Datos_Obs_Est_Res$Res, pch = 19)
abline(h = 0, lty = 2, col = 2)

#Validación de los supuestos

#Media cero
t.test(Datos_Obs_Est_Res$Res)

#Normalidad
shapiro.test(Datos_Obs_Est_Res$Res)

#Independencia
library(randtests)
runs.test(Datos_Obs_Est_Res$Res)





