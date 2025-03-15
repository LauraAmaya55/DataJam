library(readr)
library(MASS)
library(stargazer)
library(dplyr)
library(gmodels)
library(tidyr)
library(sf)     
library(ggplot2)
EPV2024 <- read_csv("EPV/EPV2024.csv")
EPV2023 <- read_csv("EPV/EPV2023.csv")
EPV2022 <- read_csv("EPV/EPV2022.csv")
EPV2021 <- read_csv("EPV/EPV2021.csv")
EPV2020 <- read_csv("EPV/EPV2020.csv")
ecn2024 <- read_csv("ECN/ecn2024.csv")
ecn2023 <- read_csv("ECN/ecn2023.csv")
ecn2022 <- read_csv("ECN/ecn2022.csv")
ecn2021 <- read_csv("ECN/ecn2021.csv")
ecn2020 <- read_csv("ECN/ecn2020.csv")

#### EPV ####
# Logits #
logit_EPV2024 <- glm(PERCEPCION ~ SEXO + REDAD + ESTRATO + P203 + P121 + P500, data = EPV2024, family = binomial(link = "logit"))
logit_EPV2024_loc <- glm(P102 ~ SEXO + REDAD + ESTRATO + P203 + P121 + P500, data = EPV2024, family = binomial(link = "logit"))

stargazer(logit_EPV2024, logit_EPV2024_loc, type = "text", 
          title = "Comparación de Resultados del Logit", 
          column.labels = c("en Bogotá", "en la localidad en la que vive"),
          dep.var.labels = c("Percepción de seguridad", "Percepción de seguridad"),
          covariate.labels = c("SEXO (MUJER=1)", "REDAD", "ESTRATO", "VÍCTIMA (SI=1)", "PRECENCIÓ (SI=1)", "NEGOCIO (SI=1)"),
          out = "logit_comparison.tex")

# CrossTables #
EPV2024_rep <- EPV2024 %>%
  uncount(weights = round(FEX))

tabla1 <- CrossTable(EPV2024_rep$LOCALIDAD, EPV2024_rep$P203, prop.chisq = FALSE)
tabla2 <- CrossTable(EPV2024_rep$ESTRATO, EPV2024_rep$P203, prop.chisq = FALSE)
tabla3 <- CrossTable(EPV2024_rep$SEXO, EPV2024_rep$P203, prop.chisq = FALSE)

# Mapa #
bogota_localidades <- st_read("Localidades/Loca.shp")
bogota_localidades <- bogota_localidades[bogota_localidades$LocCodigo != 20, ]
bogota_localidades$LocCodigo <- as.numeric(bogota_localidades$LocCodigo)

proporciones_row <- tabla1$prop.row

tabla_df <- data.frame(
  LOCALIDAD = rownames(proporciones_row),
  Proporción = proporciones_row[, 2]
)

bogota_map <- bogota_localidades %>%
  mutate(LocCodigo = as.character(LocCodigo)) %>%
  left_join(tabla_df, by = c("LocCodigo" = "LOCALIDAD"))

centroides <- st_centroid(bogota_localidades)

ggplot(bogota_map) +
  geom_sf(aes(fill = Proporción), color = "black") +
  scale_fill_gradient(low = "#e01e37", high = "#641220", na.value = "white") +
  labs(fill = "Proporción") +
  geom_text(data = centroides, aes(x = st_coordinates(geometry)[,1], 
                                   y = st_coordinates(geometry)[,2], 
                                   label = LocCodigo), 
            size = 2, color = "white") +
  theme_minimal()

library(dplyr)
columnas_localidades <- c("P203241", "P203242", "P203243", "P203244", "P203245", "P203246", "P203247", "P203248", "P203249", "P2032410", "P2032411", "P2032412", "P2032413", "P2032414", "P2032415", "P2032416", "P2032417", "P2032418", "P2032419")

porcentaje_localidades <- EPV2024 %>%
  summarise(across(all_of(columnas_localidades), 
                   ~sum((. == 1) * FEX, na.rm = TRUE) / sum(FEX, na.rm = TRUE))) %>%
  pivot_longer(cols = everything(), 
               names_to = "Localidad", 
               values_to = "Proporción")

porcentaje_localidades$Localidad <- as.numeric(gsub("P20324", "", porcentaje_localidades$Localidad))

bogota_map <- bogota_localidades %>%
  mutate(LocCodigo = as.character(LocCodigo)) %>%
  left_join(porcentaje_localidades %>% mutate(Localidad = as.character(Localidad)), 
            by = c("LocCodigo" = "Localidad"))

ggplot(bogota_map) +
  geom_sf(aes(fill = Proporción), color = "black") +
  scale_fill_gradient(low = "#e01e37", high = "#641220", na.value = "white") +
  labs(fill = "Proporción") +
  geom_text(data = centroides, aes(x = st_coordinates(geometry)[,1], 
                                   y = st_coordinates(geometry)[,2], 
                                   label = LocCodigo), 
            size = 2, color = "white") +
  theme_minimal()


#### ECN ####

ecn2024$P56_1 <- factor(ecn2024$P56_1, ordered = TRUE)
ecn2024$P64 <- factor(ecn2024$P64, ordered = TRUE)
ecn2024$F5 <- factor(ecn2024$F5, ordered = TRUE)

ecn2024 <- ecn2024[ecn2024$P62 %in% c(1, 2), ]

ecn2024$P62 <- ifelse(ecn2024$P62 == 1, 0, 1)

ecn2024$PERCEPCION <- as.factor(ecn2024$PERCEPCION)

logit_ordinal <- polr(PERCEPCION ~ F5 + P56_1 + P64, data = ecn2024, method = "logistic")
summary(logit_ordinal)
stargazer(logit_ordinal, type = "text", 
          title = "Resultados del Logit Ordinal", digits = 3,
          column.labels ="en Bogotá",
          dep.var.labels = "Percepción de inseguridad",
          out = "ordinal_logit.tex")

ecn2024_rep <- ecn2024 %>%
  uncount(weights = round(FEX))

tabla4 <- CrossTable(ecn2024$F5, ecn2024$P58, prop.chisq = FALSE)
tabla5 <- CrossTable(ecn2024$P62, ecn2024$P56_1, prop.chisq = FALSE)
tabla6 <- CrossTable(ecn2024$P64, ecn2024$P56_1, prop.chisq = FALSE)