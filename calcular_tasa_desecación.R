tabla <- df2 # Escribe aquí el nombre de la tabla

# Modelos Peso-Bellota.Especie
## Ajustar modelos lineales para cada combinación Bellota-Especie
modelos <- lapply(split(tabla, list(tabla$i, tabla$Especie)), function(df) {
  lm(Peso ~ Tiempo_acumulado, data = df)
})

## Extraer información de los modelos
info_modelos <- lapply(modelos, function(modelo) {
  coef <- summary(modelo)$coefficients[2, c("Estimate", "Pr(>|t|)")]
  r_squared <- summary(modelo)$r.squared
  c(coef, R_cuadrado = r_squared)
})

## Organizar la información en un marco de datos (data frame)
tabla_info <- do.call(rbind.data.frame, info_modelos)

## Agregar nombres de fila
rownames(tabla_info) <- NULL

## Agregar nombres de columnas
colnames(tabla_info) <- c("Pendiente", "P_valor", "R_cuadrado")

## Meter columnas para bellota y especie
n <- 10
especies <- c("Encina", "Quejigo")
tabla_info$Bellota <- c(1:n, 1:n)
tabla_info$Especie <- c(rep(especies[1], n), rep(especies[2], n))
tabla_info <- 
  tabla_info |>
  relocate(Bellota, Especie)

tasas_desecacion <- tabla_info
rm(tabla_info, info_modelos, modelos)
