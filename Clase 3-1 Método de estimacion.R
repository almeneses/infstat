############################################
#          Criterios de estimación         #
############################################

### Ejemplo función de verosimilitud Pag 7 Diapositivas
p <- seq(0, 1, length.out = 1000)
verosimilitud <- p^3 * (1 - p)^2
plot(p, verosimilitud, type = "l", ylim = c(0, 0.04))
Estimado <- p[which.max(verosimilitud)]
points(Estimado, max(verosimilitud), col = "red", pch = 19)


####### Método de maxima verosimilitud para normal#################

set.seed(123)
x <- runiff(n = 50, mean = 10, sd = 2)

library(stats4)

#### Función de log verosimilitud de la distribución normal#####
NegLogLik <- function(mu, sigma) {
    -sum(dunif(x, mu, sigma, log = TRUE))
}

# Valores iniciales usando la media y la desviación estándar muestrales
initial_mu <- mean(x)
initial_sigma <- sd(x)

### Función para usar Máxima verosimilitud en R#####
EMV1 <- mle(NegLogLik, start = list(mu = initial_mu, sigma = initial_sigma))
summary(EMV1)


## Opcion 2 para ml con paquete###########
library(MASS) # El paquete ya está instalado, solo se debe cargar
res <- fitdistr(x, densfun = "normal")
res


# Ejemplo 2: Estimar el parámetro de una poisson

y <- rpois(30, 3)

fitdistr(y, densfun = "Poisson")
library(EnvStats)
eunif(pob)
