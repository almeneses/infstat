

# Librerias 
library(faraway)
data("mtcars")
data("gala")
summary(mtcars)
head(mtcars)


# Ejemplo 1
# f(X) = 2+3X,

x<-seq(-10, 10, by = 1)
plot((2+3*x),type="l",ylab = "Y",xlab = "X",main = "Y = 2+3X")

# Modelo con error
set.seed(20)
e<-rnorm(21,0,4.5)
y<-2+3*x+e
plot(y)


model1<-lm(y~x)

plot(y~x)
abline(model1,col="red") 

###################
#    Variables    #
###################

# mpg:	Miles/(US) gallon
# cyl:	Number of cylinders
# disp:	Displacement (cu.in.)
# hp:	Gross horsepower
# drat:	Rear axle ratio
# wt:	Weight (1000 lbs)
# qsec:	1/4 mile time
# vs:	Engine (0 = V-shaped, 1 = straight)
# am:	Transmission (0 = automatic, 1 = manual)
# gear:	Number of forward gears
data("mtcars")
require(graphics)
pairs(mtcars, main = "mtcars data", gap = 1/4)

plot(mpg~cyl,data=mtcars)


coplot(mpg ~ disp | as.factor(cyl), data = mtcars,
       panel = panel.smooth, rows = 1)
## possibly more meaningful, e.g., for summary() or bivariate plots:
mtcars2 <- within(mtcars, {
  vs <- factor(vs, labels = c("V", "S"))
  am <- factor(am, labels = c("automatic", "manual"))
  cyl  <- ordered(cyl)
  gear <- ordered(gear)
  carb <- ordered(carb)
})
summary(mtcars2)



model1<-lm(mpg~cyl,data = mtcars)
summary(model1)

model2<-lm(mpg~cyl+disp+hp,data = mtcars)
summary(model2)

plot(mpg~cyl,data = mtcars)
abline(model2,col="blue")

par(mfrow=c(2,2))
plot(model2)

model3<-lm(mpg~cyl+disp+drat+qsec,data = mtcars)
summary(model3)

model4<-lm(mpg~cyl+hp,data = mtcars)
summary(model4)

model5<-lm(mpg~cyl+	am,data = mtcars)
summary(model5)

plot(mtcars)

AIC(model3)
AIC(model4)


####################################
#### Análisis de los residuales ####
####################################

# Normalidad
shapiro.test(model3$residuals)

# Homogeneidad de Varianza
library(lmtest)
bptest(model3)	

# No correlación de los errores
dwtest(model3)


################################################
#      Modelos Lineales Generalizados          #
#                                              #
################################################

###########################
#### Modelos logisticos ###
###########################

# Librerias para los datos
library(tidyverse)
library(caret)

data("PimaIndiansDiabetes2", package = "mlbench")
PimaIndiansDiabetes2 <- na.omit(PimaIndiansDiabetes2)

# modelo
model <- glm( diabetes ~ ., 
              data = PimaIndiansDiabetes2, 
              family = binomial)

summary(model)

model0<-glm( diabetes ~ glucose+mass+pedigree+age, 
             data = PimaIndiansDiabetes2, family = binomial)

summary(model0)

# Make predictions
probabilities <- model %>% predict(PimaIndiansDiabetes2, type = "response")
predicted.classes <- ifelse(probabilities > 0.5, "pos", "neg")
# Model accuracy
mean(predicted.classes == PimaIndiansDiabetes2$diabetes)

plot(model)


PimaIndiansDiabetes2 %>%
  mutate(prob = ifelse(diabetes == "pos", 1, 0)) %>%
  ggplot(aes(glucose, prob)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "glm", 
              method.args = list(family = "binomial")) +
  labs(
    title = "Logistic Regression Model", 
    x = "Plasma Glucose Concentration",
    y = "Probability of being diabete-pos"
  )


###########################
#### Modelos Poisson    ###
###########################

p <- read_csv("https://stats.idre.ucla.edu/stat/data/poisson_sim.csv")
p <- within(p, {
  prog <- factor(prog, levels=1:3, labels=c("General", "Academic", 
                                            "Vocational"))
  id <- factor(id)
})
summary(p)

with(p, tapply(num_awards, prog, function(x) {
  sprintf("M (SD) = %1.2f (%1.2f)", mean(x), sd(x))
}))
ggplot(p, aes(num_awards, fill = prog)) +
  geom_histogram(binwidth=.5, position="dodge")

m1 <- glm(num_awards ~ prog + math, family="poisson", data=p)

summary(m1 <- glm(num_awards ~ prog + math, family="poisson", data=p))
library(msm)
s <- deltamethod(list(~ exp(x1), ~ exp(x2), ~ exp(x3), ~ exp(x4)), 
                 coef(m1), vcov(m1))

## exponentiate old estimates dropping the p values
rexp.est <- exp(r.est[, -3])
## replace SEs with estimates for exponentiated coefficients
rexp.est[, "Robust SE"] <- s
