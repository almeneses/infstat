###################################################################################
###################################################################################
##Script clase 6                                                                  #
################################################################################### 
###################################################################################

rm(list = ls())
install.packages("gridExtra")

#Librerias
library(data.table)
library(ggplot2)
library(gridExtra)
library(dplyr)

#Directorio de trabajo 
setwd("~/Users")


#Cargar datos
Datos_bancarios <- fread("Default.csv")
dim(Datos_bancarios)
names(Datos_bancarios)
table(Datos_bancarios$default)
table(Datos_bancarios$student)


#Gráfico de dispersión entre 
pdf("Fig1.pdf")
ggplot(Datos_bancarios, aes(x=balance, y=income, color=default)) +
  geom_point() + 
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=20,face="bold"))
dev.off()




p1 <- ggplot(Datos_bancarios, aes(x = default, y = income, fill = default )) + 
  geom_boxplot()+
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=20,face="bold"))


p2 <- ggplot(Datos_bancarios, aes(x = default, y = balance, fill = default )) + 
  geom_boxplot()+
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=20,face="bold"))

pdf("Fig2.pdf", width = 10, height = 5)
grid.arrange(p1, p2, ncol = 2, nrow = 1)
dev.off()

x11()
#Categorizar la variable default
Datos_bancarios <- Datos_bancarios %>%
  mutate(def_cat = if_else(default == "No", 0,  1)) %>%
  as.data.table()

table(Datos_bancarios$default,
      Datos_bancarios$def_cat)

mod_linear <- lm(def_cat ~ balance, data = Datos_bancarios)


pdf("Fig3.pdf", width = 10, height = 5)
ggplot(Datos_bancarios, aes(x=balance, y=def_cat, color=default)) +
  geom_point()+
  stat_smooth(method = "lm", col = "blue")
dev.off()

summary(mod_linear)

####################################################
#Modelo de regresión logistica#
####################################################


mod_logistico <- glm(def_cat~balance, family =  binomial(link=logit), data = Datos_bancarios)
summary(mod_logistico)

x11()
plot(Datos_bancarios$balance, Datos_bancarios$def_cat)
curve(predict(mod_logistico,data.frame(balance=x),type="resp"),add=TRUE)


#Figura 4: Ajuste logistico 
pdf("Fig4.pdf", width = 10, height = 5)
ggplot(Datos_bancarios, aes(x=balance, y=def_cat)) +
  geom_point() + 
  stat_smooth(method="glm", method.args=list(family="binomial"), se=FALSE)
dev.off()


###############################################################
###############################################################
#Modelo Probit
###############################################################
###############################################################


#Modelo probit#
mod_probit <- glm(def_cat~balance, family =  binomial(link = probit), data = Datos_bancarios)
summary(mod_logistico)
summary(mod_probit)

#probit(\hat(x_{i})) = −5.3539 + 0.16974 \cdot x
# Con un nivel de balance de x = 1000, el probit es igual a
# −5.3539 + 0.16974 \cdot (1000)

x11()
plot(Datos_bancarios$balance, Datos_bancarios$def_cat)
curve(predict(mod_logistico,data.frame(balance=x),type="resp"),add=TRUE, col = 2, lwd=2)
curve(predict(mod_probit,data.frame(balance=x),type="resp"),add=TRUE, col = 5, lwd=2)
curve(predict(mod_linear,data.frame(balance=x),type="resp"),add=TRUE, col = 3, lwd=2)
legend("topleft", c("Lineal", "Logit", "Probit"),
       col = c(3, 2, 5), lwd = c(2,2,2), lty = c(1,1,1))



################################################################
################################################################
# Modelo con sobredispersión
################################################################
################################################################
install.packages("AER")
library(AER)
DatosCancer <- read.csv("DatosCancer.csv")


#Regresión Binomial Negativa
library(MASS)
## Regresión Binomial Negativa
DatosCancer <- read.csv("DatosCancer.csv")


modelo.bn <- glm.nb(casos ~ ciudad + edad + offset(log(poblacion)),
                    data = DatosCancer)
summary(modelo.bn)



################################################################
################################################################
# Modelo tasas 
################################################################
################################################################

DatosCancer <- read.csv("DatosCancer.csv")
DatosCancer <- cbind.data.frame(DatosCancer, "tasa" = DatosCancer$casos/DatosCancer$poblacion)
colnames(DatosCancer) <- c("ciudad", "edad", "poblacion", 
                           "casos", "edad.cont", "tasa")

install.packages("lattice")
library(lattice)

x11()
xyplot(tasa ~ edad.cont, groups = DatosCancer$ciudad, 
       data = DatosCancer, 
       type = "b", auto.key = list(corner = c(0, 1)))


## Ajuste de un modelo Poisson
mod1.pois <- glm(casos ~ ciudad + edad, offset = log(poblacion), 
                 family = poisson(link = "log"), data = DatosCancer)
summary(mod1.pois)


mod2.pois <- glm(casos ~ ciudad + edad.cont,
                 offset = log(poblacion),
                 family = poisson(link = "log"), data = DatosCancer)

mod3.pois <- glm(casos ~ ciudad + edad, family = poisson(link="log"), data = DatosCancer)

mod1.pois$aic
mod2.pois$aic
mod3.pois$aic

#Cargar el paquete MASS
library(MASS)
modelo.bn <- glm.nb(casos ~ ciudad + edad + offset(log(poblacion)),
                    data = DatosCancer)
modelo.bn$aic



D2.mod1.pois <- (mod1.pois$null.deviance - mod1.pois$deviance)/mod1.pois$null.deviance*100
D2.mod2.pois <- (mod2.pois$null.deviance - mod2.pois$deviance)/mod2.pois$null.deviance*100

dispersiontest(mod1.pois)
dispersiontest(mod2.pois)
#Si la Devianza residual es lo suficientemente cercana a los grados de
#libertad residuales, es una buen modelo
list(dev.residual = deviance(mod1.pois),
     gl = df.residual(mod1.pois),
     valor.p = pchisq(deviance(mod1.pois), df.residual(mod1.pois), lower = F))

list(dev.residual = deviance(mod2.pois),
     gl = df.residual(mod2.pois),
     valor.p = pchisq(deviance(mod2.pois), df.residual(mod2.pois), lower = F))

#Devianza residual
res.mod1 <- residuals(mod1.pois, type="deviance")
res.mod2 <- residuals(mod2.pois, type="deviance")


