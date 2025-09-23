##################
#### Sesion 7 ####
##################

# Librerías

library(gridExtra)
library(tidyverse)
library(rstatix)
library(ggplot2)
library(modelr)
library(dplyr)
library(effectsize)
library(MASS)
library(MVN)


library(lattice)
library(plyr)
library(car)
library(corrplot)
library(doBy)
library(multcomp)



#########################
#### Ejemplo 1 GLM ######
#########################

# Cargando los datos
dataglm1 <- read_csv("dataglm1.csv")
attach(dataglm1)
# Probando diferentes funciones de enlace
# enlace probit
glm.parab.probit <- glm(cbind(p.buenas, p.malas) ~ x1 + x2 + x3 + x4 + x1 * x2 + x2 * x3,
  family = binomial(link = probit), data = dataglm1
)
anova(glm.parab.probit)
summary(glm.parab.probit)

# Pseudo R^2 probit
pseudo.R1.probit <- cor(glm.parab.probit$fitt * 1000, dataglm1$p.buenas)^2
pseudo.R1.probit
pseudo.R2.probit <- 1 - (sum((glm.parab.probit$fitt - dataglm1$p.buenas / 1000)^2) / (8 - 7)) /
  (sum((dataglm1$p.buenas / 1000 - mean(dataglm1$p.buenas / 1000))^2) / (8 - 1))
pseudo.R2.probit

# enlace logit
glm.parab.logit <- glm(cbind(p.buenas, p.malas) ~ x1 + x2 + x3 + x4 + x1 * x2 + x2 * x3,
  family = binomial(link = logit), data = dataglm1
)
anova(glm.parab.logit)
summary(glm.parab.logit)
cor(glm.parab.logit$fitt * 1000, dataglm1$p.buenas)^2

# Pseudo R^2 logit
pseudo.R1.logit <- cor(glm.parab.logit$fitt * 1000, dataglm1$p.buenas)^2
pseudo.R1.logit
pseudo.R2.logit <- 1 - (sum((glm.parab.logit$fitt - dataglm1$p.buenas / 1000)^2) / (8 - 7)) /
  (sum((dataglm1$p.buenas / 1000 - mean(dataglm1$p.buenas / 1000))^2) / (8 - 1))
pseudo.R2.logit

# enlace cloglog
glm.parab.cloglog <- glm(cbind(p.buenas, p.malas) ~ x1 + x2 + x3 + x4 + x1 * x2 + x2 * x3,
  family = binomial(link = cloglog), data = dataglm1
)
anova(glm.parab.cloglog)
summary(glm.parab.cloglog)
cor(glm.parab.cloglog$fitt * 1000, p.buenas)^2

# Pseudo R^2 cloglog
pseudo.R1.cloglog <- cor(glm.parab.cloglog$fitt * 1000, p.buenas)^2
pseudo.R1.cloglog
pseudo.R2.cloglog <- 1 - (sum((glm.parab.cloglog$fitt - p.buenas / 1000)^2) / (8 - 7)) /
  (sum((p.buenas / 1000 - mean(p.buenas / 1000))^2) / (8 - 1))
pseudo.R2.cloglog

# enlace log-log
glm.parab.log <- glm(cbind(p.buenas, p.malas) ~ x1 + x2 + x3 + x4 + x1 * x2 + x2 * x3,
  family = binomial(link = log)
)
anova(glm.parab.log)
summary(glm.parab.log)
cor(glm.parab.log$fitt * 1000, p.buenas)^2
# Pseudo R^2 log-log
pseudo.R1.log <- cor(glm.parab.log$fitt * 1000, p.buenas)^2
pseudo.R1.log
pseudo.R2.log <- 1 - (sum((glm.parab.log$fitt - p.buenas / 1000)^2) / (8 - 7)) /
  (sum((p.buenas / 1000 - mean(p.buenas / 1000))^2) / (8 - 1))
pseudo.R2.log

# enlace Cauchit
glm.parab.cauchit <- glm(cbind(p.buenas, p.malas) ~ x1 + x2 + x3 + x4 + x1 * x2 + x2 * x3,
  family = binomial(link = cauchit)
)
anova(glm.parab.cauchit)
summary(glm.parab.cauchit)
cor(glm.parab.cauchit$fitt * 1000, p.buenas)^2

# Pseudo R^2 Cauchit
pseudo.R1.cauchit <- cor(glm.parab.cauchit$fitt * 1000, p.buenas)^2
pseudo.R1.cauchit
pseudo.R2.cauchit <- 1 - (sum((glm.parab.cauchit$fitt - p.buenas / 1000)^2) / (8 - 7)) /
  (sum((p.buenas / 1000 - mean(p.buenas / 1000))^2) / (8 - 1))
pseudo.R2.cauchit

########################################
# VALIDACIÓN DE SUPUESTOS GRÁFICAMENTE #
########################################
par(mfrow = c(2, 2))
plot(glm.parab.logit$fitted, p.buenas)
E1 <- residuals(glm.parab.logit, type = "pearson")
F1 <- predict(glm.parab.logit, type = "response")
G1 <- residuals(glm.parab.logit, type = "deviance")
plot(E1)
plot(F1)
plot(G1)
# Residuales de Pearson
library(ggplot2)
library(digest)
library(labeling)
library(gridExtra)
p1 <- ggplot(data = NULL, aes(x = F1, y = E1)) +
  xlab("número de partes buenas") +
  ylab("Residuales de Pearson") +
  stat_smooth(aes(x = F1, y = E1), method = "gam") +
  geom_jitter(size = 0.5, position = position_jitter(width = 0.05))
# Véase también si hay patrones para nuestras variables explicativas:
p2 <- ggplot(data = dataglm1, aes(factor(x1), y = E1)) +
  ylab("Residuales
de Pearson") +
  xlab("x1") +
  stat_boxplot() +
  geom_hline(yintercept = 0, colour = 2)
p3 <- ggplot(data = dataglm1, aes(factor(x2), y = E1)) +
  ylab("Residuales
de Pearson") +
  xlab("x2") +
  stat_boxplot() +
  geom_hline(yintercept = 0, colour = 2)
p4 <- ggplot(data = dataglm1, aes(factor(x3), y = E1)) +
  ylab("Residuales
de Pearson") +
  xlab("x3") +
  stat_boxplot() +
  geom_hline(yintercept = 0, colour = 2)
p5 <- ggplot(data = dataglm1, aes(factor(x4), y = E1)) +
  ylab("Residuales
de Pearson") +
  xlab("x4") +
  stat_boxplot() +
  geom_hline(yintercept = 0, colour = 2)
# Residuales de deviance
d1 <- ggplot(data = NULL, aes(x = F1, y = G1)) +
  xlab("Defectos en obleas") +
  ylab("Residuales de deviance") +
  stat_smooth(aes(x = F1, y = G1), method = "gam") +
  geom_jitter(size = 0.5, position = position_jitter(width = 0.05))
# Véase también si hay patrones para nuestras variables explicativas:
d2 <- ggplot(data = dataglm1, aes(factor(x1), y = G1)) +
  ylab("Residuales
de deviance") +
  xlab("x1") +
  stat_boxplot() +
  geom_hline(yintercept = 0, colour = 2)
d3 <- ggplot(data = dataglm1, aes(factor(x2), y = G1)) +
  ylab("Residuales
de deviance") +
  xlab("x2") +
  stat_boxplot() +
  geom_hline(yintercept = 0, colour = 2)
d4 <- ggplot(data = dataglm1, aes(factor(x3), y = G1)) +
  ylab("Residuales
de deviance") +
  xlab("x3") +
  stat_boxplot() +
  geom_hline(yintercept = 0, colour = 2)
d5 <- ggplot(data = dataglm1, aes(factor(x4), y = G1)) +
  ylab("Residuales
de deviance") +
  xlab("x4") +
  stat_boxplot() +
  geom_hline(yintercept = 0, colour = 2)
grid.arrange(p1, p2, p3, p4, p5, d1, d2, d3, d4, d5, ncol = 5) ## Esta función
# permite colocar las 3 figuras juntas


#########################
#### Ejemplo 2 GLM ######
#########################
##################
#### Sesion 7 ####
##################

soja <- read.delim2("soja.txt")
soja <- soja[-74, ]
soja <- transform(soja, K = factor(K))
xyplot(nvag + ngra ~ K,
  groups = umid,
  outer = TRUE,
  data = soja,
  type = c("p", "a"),
  scales = "free",
  ylab = NULL,
  xlab = expression("Applied potassium amount" ~ (mg ~ dm^{
    -3
  })),
  auto.key = list(
    title = "Soil water content (%)",
    cex.title = 1,
    columns = 3
  ),
  strip = strip.custom(
    factor.levels = c(
      "Number of pods",
      "Number of beans"
    )
  )
)


#--------------------------------------------
# Poisson.

m0 <- glm(nvag ~ bloc + umid * K,
  data = soja,
  family = poisson
)

# Analysis of deviance table.
anova(m0, test = "Chisq")

# Wald test for interaction.
a <- c(0, attr(model.matrix(m0), "assign"))
ai <- a == max(a)
L <- t(replicate(sum(ai), rbind(coef(m1) * 0), simplify = "matrix"))
L[, ai] <- diag(sum(ai))
linearHypothesis(
  model = m0, # m0 is not being used here.
  hypothesis.matrix = L,
  vcov. = vcov(m1),
  coef. = coef(m1)
)


# numero de granos
#--------------------------------------------
# Poisson.
soja$off <- 10
fivenum(with(soja, ngra / off))
m0 <- glm(ngra ~ offset(log(off)) + bloc + umid * K,
  data = soja,
  family = poisson
)
# Analysis of deviance table.
anova(m0, test = "Chisq")


# Número de granos por vaina
#--------------------------------------------
# Poisson.

m0 <- glm(ngra ~ offset(log(nvag)) + bloc + umid * K,
  data = soja,
  family = poisson
)
