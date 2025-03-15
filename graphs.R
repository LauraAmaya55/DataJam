# Instalación de paquetes y carga de librerías ----------------------------------------------------------------------------

install.packages("ggplot2")
install.packages("treemap")
library(ggplot2)
library(scales)
library(treemap)


# 2. Cálculos para EPV (Encuesta de Percepción de Vulnerabilidad)----------------------------------------------------------------------------

ins_2024 <- sum((EPV2024$P103 == 2) * EPV2024$FACTOR, na.rm = TRUE)
ins_2023 <- sum((EPV2023$P103 == 2) * EPV2023$FEX, na.rm = TRUE)
ins_2022 <- sum((EPV2022$P103 == 2) * EPV2022$FEX, na.rm = TRUE)
ins_2021 <- sum((EPV2021$P103 == 2) * EPV2021$FEX, na.rm = TRUE)
ins_2020 <- sum((EPV2020$P103 == "Inseguro") * EPV2020$FEX, na.rm = TRUE)


# 3. Cálculos para ECN (Encuesta de Condiciones de Vida) ----------------------------------------------------------------------------

ins_2024_ecn <- sum((ecn2024$P56 == 2) * ecn2024$FACTOR_REGION, na.rm = TRUE)
ins_2023_ecn <- sum((ecn2023$P56 == 2) * ecn2023$PONDERA, na.rm = TRUE)
ins_2022_ecn <- sum((ecn2022$P56 == 2) * ecn2022$F.EXPANSIÓN, na.rm = TRUE)
ins_2021_ecn <- sum((ecn2021$P54 == 2) * ((ecn2020$fexp + ecn2022$F.EXPANSIÓN) / 2), na.rm = TRUE)
ins_2020_ecn <- sum((ecn2020$p_p64_p64 == 2) * ecn2020$fexp, na.rm = TRUE)


# 4. Preparación para gráficos----------------------------------------------------------------------------

datos_epv <- data.frame(
  Año = c(2020, 2021, 2022, 2023, 2024),
  Conteo = c(ins_2020, ins_2021, ins_2022, ins_2023, ins_2024)
)

datos_ecn <- data.frame(
  Año = c(2020, 2021, 2022, 2023, 2024),
  Conteo = c(ins_2020_ecn, ins_2021_ecn, ins_2022_ecn, ins_2023_ecn, ins_2024_ecn)
)

# 5. Gráficos de barras EPV y ECN ----------------------------------------------------------------------------


# Gráfico para EPV
ggplot(datos_epv, aes(x = factor(Año), y = Conteo)) +
  geom_bar(stat = "identity", fill = "blue", alpha = 0.6) +
  geom_point(aes(y = Conteo), color = "darkblue", size = 3) +
  geom_line(aes(x = factor(Año), y = Conteo, group = 1), color = "darkblue", size = 1) +
  labs(title = "Percepción de inseguridad por año (EPV)", x = "Año", y = "Conteo ponderado") +
  scale_y_continuous(labels = comma) +
  theme_minimal()

# Gráfico para ECN
ggplot(datos_ecn, aes(x = factor(Año), y = Conteo)) +
  geom_bar(stat = "identity", fill = "red", alpha = 0.6) +
  geom_point(aes(y = Conteo), color = "darkred", size = 3) +
  geom_line(aes(x = factor(Año), y = Conteo, group = 1), color = "darkred", size = 1) +
  labs(title = "Percepción de inseguridad por año (ECN)", x = "Año", y = "Conteo ponderado") +
  scale_y_continuous(labels = comma) +
  theme_minimal()


# 6. Treemaps para ECN (2024, 2023, 2022, 2020)----------------------------------------------------------------------------

# Treemap para 2024
cols <- paste("P39_", LETTERS[1:7], sep = "")
count_5 <- sapply(cols, function(col) sum(ifelse(ecn2024[[col]] == 5, ecn2024$FACTOR_REGION, 0), na.rm = TRUE))
total_5 <- sum(count_5)
percent_5 <- round((count_5 / total_5) * 100, 2)
count_data <- data.frame(columna = names(count_5), cantidad_5 = count_5, porcentaje = percent_5)
count_data$label <- paste0(count_data$columna, "\n", count_data$porcentaje, "%")

treemap(count_data, index = "label", vSize = "cantidad_5", vColor = "cantidad_5", draw = TRUE, palette = "Reds", title = "Proporción de percepción de servicios más importantes a implementar en economía popular")

# Treemap para 2023
cols_2023 <- paste0("P39_", 1:6)
count_5_2023 <- sapply(cols_2023, function(col) sum(ifelse(ecn2023[[col]] == 5, ecn2023$PONDERA, 0), na.rm = TRUE))
total_5_2023 <- sum(count_5_2023)
percent_5_2023 <- round((count_5_2023 / total_5_2023) * 100, 2)
count_data_2023 <- data.frame(columna = names(count_5_2023), cantidad_5 = count_5_2023, porcentaje = percent_5_2023)
count_data_2023$label <- paste0(count_data_2023$columna, "\n", count_data_2023$porcentaje, "%")

treemap(count_data_2023, index = "label", vSize = "cantidad_5", vColor = "cantidad_5", draw = TRUE, palette = "Reds", title = "Proporción de percepción de servicios más importantes a implementar en economía popular en 2023")

# Treemap para 2022
cols_2022 <- paste0("P39.", 1:6)
count_5_2022 <- sapply(cols_2022, function(col) sum(ifelse(ecn2022[[col]] == 5, ecn2022$`F.EXPANSIÓN`, 0), na.rm = TRUE))
total_5_2022 <- sum(count_5_2022)
percent_5_2022 <- round((count_5_2022 / total_5_2022) * 100, 2)
count_data_2022 <- data.frame(columna = names(count_5_2022), cantidad_5 = count_5_2022, porcentaje = percent_5_2022)
count_data_2022$label <- paste0(count_data_2022$columna, "\n", count_data_2022$porcentaje, "%")

treemap(count_data_2022, index = "label", vSize = "cantidad_5", vColor = "cantidad_5", draw = TRUE, palette = "Reds", title = "Proporción de percepción de servicios más importantes a implementar en economía popular en 2022")

# Treemap para 2020
cols_2020 <- paste0("p_p37_p37_", 1:13)
count_1_2020 <- sapply(cols_2020, function(col) sum(ifelse(ecn2020[[col]] == 1, ecn2020$fexp, 0), na.rm = TRUE))
total_1_2020 <- sum(count_1_2020)
percent_1_2020 <- round((count_1_2020 / total_1_2020) * 100, 2)
count_data_2020 <- data.frame(columna = names(count_1_2020), cantidad_1 = count_1_2020, porcentaje = percent_1_2020)
count_data_2020$label <- paste0(count_data_2020$columna, "\n", count_data_2020$porcentaje, "%")

treemap(count_data_2020, index = "label", vSize = "cantidad_1", vColor = "cantidad_1", draw = TRUE, palette = "Reds", title = "Proporción de percepción de servicios más importantes a implementar en economía popular en 2020")
