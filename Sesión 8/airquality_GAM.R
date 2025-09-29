data("airquality")
airquality <- na.omit(airquality) # Eliminar NA
head(airquality)


plot(Ozone ~Solar.R ,data=airquality )

library(gam)
# Modelo GAM con funciones suaves para cada predictor
gam_ozone <- gam(Ozone ~ s(Temp) + s(Wind) + s(Solar.R), data = airquality)


par(mfrow = c(1, 3))
plot(gam_ozone, se = TRUE, shade = TRUE)

glm_ozone <- lm(Ozone ~ Temp + Wind + Solar.R, data=airquality)

par(mfrow = c(1, 3))
plot(glm_ozone, se = TRUE, shade = TRUE)

summary(gam_ozone)
print("-----------")
summary(glm_ozone)


airquality$pred_gam <- predict(gam_ozone)
ggplot(airquality, aes(x = Ozone, y = pred_gam)) +
  geom_point(color = "steelblue") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Valores observados vs. predichos por GAM",
       x = "Ozone observado", y = "Ozone predicho") +
  theme_minimal()


airquality$pred_gam <- predict(glm_ozone)
ggplot(airquality, aes(x = Ozone, y = pred_gam)) +
  geom_point(color = "steelblue") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Valores observados vs. predichos por GAM",
       x = "Ozone observado", y = "Ozone predicho") +
  theme_minimal()