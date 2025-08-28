###############################################################################
# Clase: Estimadores y distribuciones muestrales
###############################################################################

### Simular una población
# Crear una población N=10000 que se distribuye normal(0,1)
N <- 10000
mu <- 3
pob <- rnorm(N, 3, 5)

set.seed(1234)
# Graficar el histograma de la población
hist(pob, col = "lightblue", main = "", freq = FALSE)
lines(density(pob), col = "red", lty = 2)

# Parámetro: Media
mu

# Generar muestras aletoria de tamaño n=5
B <- 100 # Cantidad de muestras
n <- 5 # Tamaño de la muestra
muestra <- matrix(NA, B, n)
prom.muestra <- 0
var.muestra <- 0


### Propuesta de estimadores###
T1 <- function(datos) {
  mean(datos)
}

T2 <- function(datos) {
  (max(datos) + min(datos)) / 2
}

T3 <- function(datos) {
  (max(datos) - min(datos))^2
}

################### Propiedad de sesgo################
# Generar muestras aletoria de tamaño n
B <- 100000 # Cantidad de muestras
n <- 100 # Tamaño de la muestra
muestra <- matrix(NA, B, n)
Est1 <- 0
Est2 <- 0
Est3 <- 0

for (i in 1:B) {
  muestra[i, ] <- sample(pob, n, replace = FALSE)
  Est1[i] <- T1(muestra[i, ])
  Est2[i] <- T2(muestra[i, ])
  Est3[i] <- T3(muestra[i, ])
}

par(mfrow = c(1, 3))

hist(Est1, col = "lightblue", main = "", freq = FALSE)
lines(density(Est1), col = "red", lty = 2)
abline(v = mu, col = "blue")
hist(Est2, col = "lightblue", main = "", freq = FALSE)
lines(density(Est2), col = "red", lty = 2)
abline(v = mu, col = "blue")
hist(Est3, col = "lightblue", main = "", freq = FALSE)
lines(density(Est3), col = "red", lty = 2)
abline(v = mu, col = "blue")

BT1 <- mean(Est1) - mu
BT2 <- mean(Est2) - mu
BT3 <- mean(Est3) - mu

ECMT1 <- var(Est1) + (BT1)^2
ECMT2 <- var(Est2) + (BT2)^2
ECMT3 <- var(Est3) + (BT3)^2
ECMT1
ECMT2
ECMT3

#### Propiedad de consistencia####
set.seed(123) #
sample_sizes <- seq(10, 3000, by = 10) # Tamaños de muestra
estimates1 <- numeric(length(sample_sizes)) # Para guardar los estimadores
estimates2 <- numeric(length(sample_sizes)) # Para guardar los estimadores

# Generar datos y calcular estimadores
for (i in seq_along(sample_sizes)) {
  sample <- sample(pob, sample_sizes[i], replace = FALSE)
  estimates1[i] <- T1(sample) # Media muestral como estimador
  estimates2[i] <- T2(sample) # Media muestral como estimador
}

par(mfrow = c(1, 1))
# Visualización de resultados
plot(sample_sizes, estimates1,
  type = "l", col = "blue",
  xlab = "Tamaño de la muestra (n)", ylab = "Estimadores",
  main = "", ylim = c(-1, 0.5)
)
lines(sample_sizes, estimates2, type = "l", col = "red")
abline(h = mu, col = "black", lty = 2) # Línea de la media poblacional
legend("topright",
  legend = c("T1", "T2", "Parámetro"),
  col = c("blue", "red", "black"), lty = c(1, 1, 2), bty = "n"
)

### Propiedad de Eficiencia###
# Visualización
boxplot(Est1, Est2,
  names = c("T1", "T2"),
  main = "Comparación de estimadores", ylab = "Valor del estimador",
  col = c("blue", "green")
)


### TLC####

# Configuración inicial
set.seed(123) # Para reproducibilidad
B <- 10000 # Número de muestras aleatorias
n_values <- c(2, 10, 50, 100) # Diferentes tamaños de muestra
poblacion <- rexp(100000, rate = 1) # Población: distribución exponencial (no normal)

# Función para simular el TLC
simulate_tlc <- function(n, B, poblacion) {
  medias <- numeric(B) # Almacenar medias muestrales
  for (i in 1:B) {
    muestra <- sample(poblacion, n, replace = TRUE) # Tomar muestra aleatoria
    medias[i] <- mean(muestra) # Calcular media muestral
  }
  return(medias)
}

# Simular medias muestrales para obtener límites comunes
all_medias <- lapply(n_values, simulate_tlc, B = B, poblacion = poblacion)
xlim_range <- range(unlist(all_medias)) # Rango común en el eje X
ylim_range <- c(0, 10) # Fijar rango común en el eje Y para densidades

# Graficar con la misma escala en todos los histogramas
par(mfrow = c(2, 2)) # Configurar el espacio gráfico
for (i in seq_along(n_values)) {
  hist(all_medias[[i]],
    breaks = 30, probability = TRUE, col = "lightblue",
    main = paste("n =", n_values[i]), xlab = "Media muestral",
    ylab = "Densidad", xlim = xlim_range, ylim = ylim_range
  )
  curve(dnorm(x, mean = mean(all_medias[[i]]), sd = sd(all_medias[[i]])),
    col = "red", lwd = 2, add = TRUE
  )
}

# Restaurar gráficos a uno solo
par(mfrow = c(1, 1))
