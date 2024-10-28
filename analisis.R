library(readxl)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(car)       # Para pruebas de homogeneidad de varianza (Levene)
library(MASS)      # Para el test de normalidad de Shapiro-Wilk
library(FSA)       # Para hacer la prueba post-hoc

rD1 <- read_excel(path = "data/PesoCotiledones.xlsx", sheet = 1)
rD2 <- read.csv2(file = "data/PesoCotiledones.csv")

cols <- rD2[1,]
df2 <- rD2[2:21,]
colnames(df2) <- cols
colnames(df2)[3] <- "i"
df2 <- df2[,1:16]
df2$i <- as.numeric(df2$i)
str(df2)

# Convertir las comas a puntos para que los valores numéricos sean reconocidos correctamente
df2[, 4:16] <- lapply(df2[, 4:16], function(x) as.numeric(gsub(",", ".", x)))


# Hacer el pivot longer
df2 <- pivot_longer(df2, 
                    cols = starts_with("19-9-24 17:00"):starts_with("4-10-24 13:00"), 
                    names_to = "Fecha", 
                    values_to = "Peso")

# Convertir la columna 'Fecha' en formato de fecha usando dmy
df2 <- df2 %>%
  mutate(Fecha = dmy_hm(Fecha))


df2 <- df2 %>%
  # Agrupar por 'Especie' e 'i'
  group_by(Especie, i) %>%
  # Calcular la diferencia de tiempo entre fechas consecutivas en horas
  mutate(Tiempo_acumulado = cumsum(c(0, as.numeric(difftime(Fecha[-1], Fecha[-length(Fecha)], units = "hours"))))) %>%
  ungroup()

source(file = "calcular_tasa_desecación.R")

# Añadir Peso seco
df2 <- 
merge(x = df2, y = rD2[, c(1, 3, 17, 18)], 
      by.x = c("Especie", "i"), by.y = c("X", "X.2"), 
      all.x = TRUE)
colnames(df2)[9:10] <- c("Peso seco", "Peso seco cotiledones")
df2[, 9:10] <- lapply(df2[, 9:10], function(x) as.numeric(gsub(",", ".", x)))

# Arreglar formato de algunas variables
df2$Especie <- as.factor(df2$Especie)
df2$Tratamiento <- as.factor(df2$Tratamiento)

# Crear variable ratio Peso seco/Peso fresco
df2.rt <-
df2 |>
  filter(Tiempo_acumulado == 0) |>
  mutate(
    `Ratio peso seco / peso fresco` = `Peso seco` / Peso 
  )
df2 <- merge(x = df2, y = df2.rt[c( "Especie", "i", "Ratio peso seco / peso fresco")], 
             by = c("Especie", "i"), all.x = TRUE)
colnames(rD1)[3] <- "i"
df2 <- 
merge(df2, df2.rt[c( "Especie", "i", "Peso")], by = c("Especie", "i")
      )
colnames(df2)[c(7, 12)] <- c("Peso", "Peso fresco")

# Unir datasets con información de ambiente y estufa
## Paso 1: Ajustar rD1 añadiendo las columnas que faltan en df2
rD1$Fecha <- NA  # Añadir columna 'Fecha' con NAs
rD1$Tiempo_acumulado <- NA  # Añadir 'Tiempo_acumulado' con NAs
rD1$Ratio_peso_seco_peso_fresco <- NA  # Añadir 'Ratio peso seco / peso fresco' con NAs

## Paso 2: Eliminar columnas relacionadas con los cotiledones en rD1
rD1 <- rD1[, !names(rD1) %in% c("Cotiledones fresco (g)", "Cotiledones seco (g)")]

## Paso 3: Renombrar las columnas de rD1 para que coincidan con las de df2
names(rD1)[names(rD1) == "Peso fresco (g)"] <- "Peso fresco"
names(rD1)[names(rD1) == "Peso seco (g)"] <- "Peso seco"
names(rD1)[names(rD1) == "Longitud (mm)"] <- "Longitud (mm)"
names(rD1)[names(rD1) == "Anchura (mm)"] <- "Anchura (mm)"

## Paso 4: Unir los datasets usando rbind.fill (de la librería plyr)
combined_df <- bind_rows(df2, rD1)

## Paso 5: Recalcular ratio peso seco peso fresco
combined_df[combined_df$Tratamiento == "Estufa",]$`Ratio peso seco / peso fresco` <- rD1$`Peso seco` / rD1$`Peso fresco`

## Paso 6: Reajustes de columnas no deseadas y formatos
combined_df <- combined_df[,-13]



ggplot(df2, aes(x = Tiempo_acumulado, y = Peso, colour = Especie)) +
  geom_smooth(method = "lm") + 
  geom_point()

# Scatter plot para cada una de las bellotas y especies
df2 |>
  filter(Especie == "Encina") |>
  ggplot(aes(x = Tiempo_acumulado, y = Peso)) +
  geom_point() +
  geom_smooth(method =lm) +
  facet_wrap(~ i, scales = "free", ncol = 2) + 
  ylab("Peso (g)") +
  theme_minimal() +
  labs(title = "Desecación en Encina") +
  xlab("Tiempo (h)") +
  theme(axis.text.x = element_text(size = 6, angle = 0, hjust = 1))

df2 |>
  filter(Especie == "Quejigo") |>
  ggplot(aes(x = Tiempo_acumulado, y = Peso)) +
  geom_point() +
  geom_smooth(method =lm) +
  facet_wrap(~ i, scales = "free", ncol = 2) + 
  ylab("Peso (g)") +
  theme_minimal() +
  labs(title = "Desecación en Quejigo") +
  xlab("Tiempo (h)") +
  theme(axis.text.x = element_text(size = 6, angle = 0, hjust = 1))

# Comparación de ratio peso seco/peso fresco entre tratamiento ambiente y tratamiento estufa
sel <- c(1, 12, 23, 34, 45, 56, 67, 78, 89, 
         100, 111, 122, 133, 144, 155, 166, 
         177, 188, 199, 210, 221:240)
combined_df1 <- combined_df[sel,]
## Calcular la media y el error estándar para cada tratamiento
summary_df <- combined_df1 %>%
  group_by(Tratamiento) %>%
  summarise(
    mean_ratio = mean(`Ratio peso seco / peso fresco`),
    se_ratio = sd(`Ratio peso seco / peso fresco`) / sqrt(n())
  )

summary_df2 <- combined_df1 %>%
  group_by(Tratamiento, Especie) %>%
  summarise(
    mean_ratio = mean(`Ratio peso seco / peso fresco`),
    se_ratio = sd(`Ratio peso seco / peso fresco`) / sqrt(n())
  )

summary(
  aov(`Ratio peso seco / peso fresco` ~ Tratamiento*Especie, data = combined_df1))

## Crear el diagrama de barras con barras de error
ggplot(summary_df, aes(x = Tratamiento, y = mean_ratio)) +
  geom_bar(stat = "identity", fill = "steelblue", color = "black", width = 0.7) +
  geom_errorbar(aes(ymin = mean_ratio - se_ratio, ymax = mean_ratio + se_ratio), 
                width = 0.2, color = "red") +
  labs(title = "Ratio Peso Seco / Peso Fresco por Tratamiento",
       x = "Tratamiento",
       y = "Ratio Peso Seco / Peso Fresco") +
  theme_minimal()

# Crear el diagrama de barras estilizado
ggplot(summary_df, aes(x = Tratamiento, y = mean_ratio)) +
  geom_bar(stat = "identity", fill = "#69b3a2", color = "black", width = 0.6) +
  geom_errorbar(aes(ymin = mean_ratio - se_ratio, ymax = mean_ratio + se_ratio), 
                width = 0.2, color = "#ff6347", size = 0.8) +
  labs(title = "Ratio Peso Seco / Peso Fresco por Tratamiento",
       x = "Tratamiento",
       y = "Ratio Peso Seco / Peso Fresco") +
  theme_minimal(base_size = 14) +  # Fuente base ajustada para mayor legibilidad
  theme(
    panel.grid.major = element_blank(),  # Eliminar líneas de cuadrícula grandes
    panel.grid.minor = element_blank(),  # Eliminar líneas de cuadrícula pequeñas
    axis.line = element_line(size = 0.8, color = "black"),  # Línea de eje más clara
    axis.title.x = element_text(margin = margin(t = 10)),  # Espacio entre el título y el eje x
    axis.title.y = element_text(margin = margin(r = 10)),  # Espacio entre el título y el eje y
    plot.title = element_text(hjust = 0.5, face = "bold"),  # Centrar el título y hacerlo en negrita
    axis.text = element_text(color = "black")  # Colores de texto del eje más oscuros
  ) +
  scale_fill_manual(values = c("#69b3a2"))  # Colores personalizados

# Crear el gráfico de puntos con barras de error
ggplot(summary_df, aes(x = Tratamiento, y = mean_ratio)) +
  geom_point(size = 4, color = "#69b3a2") +  # Media representada como un punto
  geom_errorbar(aes(ymin = mean_ratio - se_ratio, ymax = mean_ratio + se_ratio), 
                width = 0.1, color = "black", size = 0.8) +  # Barras de error
  labs(title = "Media del Ratio Peso Seco / Peso Fresco por Tratamiento",
       x = "Tratamiento",
       y = "Ratio Peso Seco / Peso Fresco") +
  theme_minimal(base_size = 14) +  # Fuente base ajustada para mayor legibilidad
  theme(
    panel.grid.major = element_blank(),  # Eliminar líneas de cuadrícula grandes
    panel.grid.minor = element_blank(),  # Eliminar líneas de cuadrícula pequeñas
    axis.line = element_line(size = 0.8, color = "black"),  # Línea de eje más clara
    axis.title.x = element_text(margin = margin(t = 10)),  # Espacio entre el título y el eje x
    axis.title.y = element_text(margin = margin(r = 10)),  # Espacio entre el título y el eje y
    plot.title = element_text(hjust = 0.5, face = "bold"),  # Centrar el título y hacerlo en negrita
    axis.text = element_text(color = "black")  # Colores de texto del eje más oscuros
  )
# Crear el gráfico de puntos con barras de error
ggplot(summary_df2, aes(x = interaction(Tratamiento, Especie), y = mean_ratio, colour = Especie)) +
  geom_point(size = 4) +  # Media representada como un punto
  geom_errorbar(aes(ymin = mean_ratio - se_ratio, ymax = mean_ratio + se_ratio), 
                width = 0.1, size = 0.8) +  # Barras de error
  labs(title = "Media del Ratio Peso Seco / Peso Fresco por Tratamiento",
       x = "Tratamiento",
       y = "Ratio Peso Seco / Peso Fresco") +
  theme_minimal(base_size = 14) +  # Fuente base ajustada para mayor legibilidad
  theme(
    panel.grid.major = element_blank(),  # Eliminar líneas de cuadrícula grandes
    panel.grid.minor = element_blank(),  # Eliminar líneas de cuadrícula pequeñas
    axis.line = element_line(size = 0.8, color = "black"),  # Línea de eje más clara
    axis.title.x = element_text(margin = margin(t = 10)),  # Espacio entre el título y el eje x
    axis.title.y = element_text(margin = margin(r = 10)),  # Espacio entre el título y el eje y
    plot.title = element_text(hjust = 0.5, face = "bold"),  # Centrar el título y hacerlo en negrita
    axis.text = element_text(color = "black")  # Colores de texto del eje más oscuros
  )

# ANOVA de peso seco/peso fresco segun tratamiento y especie
# Definir variables
data <- combined_df1
response <- data$`Ratio peso seco / peso fresco`
factor1 <- data$Tratamiento
factor2 <- data$Especie

# Crear un modelo ANOVA con la interacción de dos variables categóricas
modelo <- aov(response ~ factor1 * factor2, data = data)

### 1. Test de Normalidad de los residuos
# Extraer los residuos del modelo
residuos <- residuals(modelo)

# Prueba de Shapiro-Wilk para testar normalidad
shapiro_test <- shapiro.test(residuos)
print(shapiro_test) # Distribución normal

# También puedes hacer un QQ plot para visualizar la normalidad
qqnorm(residuos)
qqline(residuos, col = "red")

### 2. Test de Homogeneidad de Varianzas
# Prueba de Levene para testar homogeneidad de varianza
levene_test <- leveneTest(response ~ factor1 * factor2, data = data)
print(levene_test) # Homogeneidad de varianzas

# Gráfico de los residuos frente a los valores ajustados para verificar la homogeneidad
plot(fitted(modelo), residuos)
abline(h = 0, col = "red")


ggplot(df2, aes(x = `Peso fresco`, y = `Peso seco`)) +
  geom_point(aes(colour = Especie)) +
  geom_smooth(method = "lm")

tabla <- combined_df1[combined_df1$Tratamiento == "Ambiente",]

lm.pspf <- lm(tabla$`Peso seco` ~ tabla$`Peso fresco`)
a <- coefficients(lm.pspf)[1]
b <- coefficients(lm.pspf)[2]

combined_df1 <-
combined_df1 |>
  mutate(
    `Peso seco deducido` = a + `Peso fresco`*b, .after = `Peso seco`
  )
n <- length(combined_df1$`Peso fresco`)
Peso <- c(combined_df1$`Peso fresco`, combined_df1$`Peso seco`, combined_df1$`Peso seco deducido`)
Especie <- rep(combined_df1$Especie, 3)
Trat <- rep(combined_df1$Tratamiento, 3)
t <- c(rep("t0", n), rep("tf", 2*n))
modelo <- c(rep("Original", 2*n), rep("Modelo", n))

peso.modelos <- data.frame(Especie, Peso, t, modelo, Trat)


peso.modelos |>
  ggplot(aes(x = t, y = Peso, colour = modelo)) +
  geom_point()

summary_df3 <- peso.modelos %>%
  group_by(t, modelo, Trat) %>%
  summarise(
    mean_ratio = mean(Peso),
    se_ratio = sd(Peso) / sqrt(n())
  )

# Crear el gráfico de puntos con barras de error
ggplot(summary_df3, aes(x = interaction(t, modelo, Trat), y = mean_ratio, colour = modelo, alpha = 0.25)) +
  geom_point(size = 4) +  # Media representada como un punto
  geom_errorbar(aes(ymin = mean_ratio - se_ratio, ymax = mean_ratio + se_ratio), 
                width = 0.1, size = 0.8) +  # Barras de error
  labs(title = "Media del Ratio Peso Seco / Peso Fresco por Tratamiento",
       x = "Tratamiento",
       y = "Ratio Peso Seco / Peso Fresco") +
  theme_minimal(base_size = 14) +  # Fuente base ajustada para mayor legibilidad
  theme(
    panel.grid.major = element_blank(),  # Eliminar líneas de cuadrícula grandes
    panel.grid.minor = element_blank(),  # Eliminar líneas de cuadrícula pequeñas
    axis.line = element_line(size = 0.8, color = "black"),  # Línea de eje más clara
    axis.title.x = element_text(margin = margin(t = 10)),  # Espacio entre el título y el eje x
    axis.title.y = element_text(margin = margin(r = 10)),  # Espacio entre el título y el eje y
    plot.title = element_text(hjust = 0.5, face = "bold"),  # Centrar el título y hacerlo en negrita
    axis.text = element_text(color = "black")  # Colores de texto del eje más oscuros
  )

##
# Gráfico t0 a tf del peso 
##
combined_df2 <- data.frame(
  Peso = c(combined_df1$`Peso fresco`, combined_df1$`Peso seco`),
  `Tipo de peso` = c(rep("Peso fresco", 40), rep("Peso seco", 40))
)
 
combined_df2 <- cbind(
  rbind(combined_df1[,c(1:5,11)], combined_df1[,c(1:5,11)]), 
  combined_df2)

ggplot(combined_df2, aes(x = Tipo.de.peso, y = Peso)) +
  geom_point(aes(colour = Tratamiento))

summary_df4 <- combined_df2 %>%
  group_by(Tratamiento, Especie, Tipo.de.peso) %>%
  summarise(
    mean_ratio = mean(Peso),
    se_ratio = sd(Peso) / sqrt(n())
  )

# Crear el gráfico de puntos con barras de error
ggplot(summary_df4, aes(x = Tipo.de.peso, y = mean_ratio, colour = Tratamiento), alpha = 0.25) +
  geom_point(size = 2) +  # Media representada como un punto
  geom_errorbar(aes(ymin = mean_ratio - se_ratio, ymax = mean_ratio + se_ratio), 
                width = 0.1, size = 0.8) +   # Barras de error
  facet_wrap(~ Especie) +
  ylab("Media ± EE del peso (g)") +
  xlab("")


# ANOVA de peso fresco y peso seco segun tratamiento y especie
# Definir variables
data <- combined_df2
response <- combined_df2$Peso
factor1 <- data$Tratamiento
factor2 <- data$Especie
factor3 <- data$Tipo.de.peso

# Crear un modelo ANOVA con la interacción de dos variables categóricas
modelo <- aov(response ~ factor1 * factor2 * factor3, data = data)

### 1. Test de Normalidad de los residuos
# Extraer los residuos del modelo
residuos <- residuals(modelo)

# Prueba de Shapiro-Wilk para testar normalidad
shapiro_test <- shapiro.test(residuos)
print(shapiro_test) # Distribución NO-normal

# También puedes hacer un QQ plot para visualizar la normalidad
qqnorm(residuos)
qqline(residuos, col = "red")

### 2. Test de Homogeneidad de Varianzas
# Prueba de Levene para testar homogeneidad de varianza
levene_test <- leveneTest(response ~ factor1 * factor2 * factor3, data = data)
print(levene_test) # Homogeneidad de varianzas

# Gráfico de los residuos frente a los valores ajustados para verificar la homogeneidad
plot(fitted(modelo), residuos)
abline(h = 0, col = "red")

# Aplicar Kruskal test
# Crear una nueva variable combinada
data$combined_factor <- interaction(combined_df2$Tratamiento, combined_df2$Especie, combined_df2$Tipo.de.peso)

# Aplicar el test de Kruskal-Wallis a la nueva variable
kruskal_test_combined <- kruskal.test(response ~ combined_factor, data = data)
print(kruskal_test_combined)

posthoc <- dunnTest(response ~ combined_factor, data = data, method = "bh")
posthoc$res[posthoc$res$P.adj <= 0.05,]
# Ambiente.Encina.Peso seco - Estufa.Encina.Peso seco
# Ambiente.Quejigo.Peso seco - Estufa.Quejigo.Peso seco
