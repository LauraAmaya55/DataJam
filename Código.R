library(readr)
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

library(MASS)
library(stargazer)
library(dplyr)
library(gmodels)
library(tidyr)


logit_EPV2024 <- glm(PERCEPCION ~ SEXO + REDAD + ESTRATO + P203 + P500, data = EPV2024, family = binomial(link = "logit"))
logit_EPV2024_loc <- glm(P102 ~ SEXO + REDAD + ESTRATO + P203 + P500, data = EPV2024, family = binomial(link = "logit"))

stargazer(logit_EPV2024, logit_EPV2024_loc, type = "text", 
          title = "Comparación de Resultados del Logit", 
          column.labels = c("en Bogotá", "en la localidad en la que vive"),  # Nombres de columnas
          dep.var.labels = c("Percepción de seguridad", "Percepción de seguridad"),  # Cambia nombres
          covariate.labels = c("SEXO (MUJER=1)", "REDAD", "ESTRATO", "VÍCTIMA (SI=1)", "NEGOCIO (SI=1)"),  # Etiquetas de los predictores
          out = "logit_comparison.tex")

EPV2024_rep <- EPV2024 %>%
  uncount(weights = round(FEX))

tabla1 <- CrossTable(EPV2024_rep$LOCALIDAD, EPV2024_rep$P203, prop.chisq = FALSE)
tabla2 <- CrossTable(EPV2024_rep$ESTRATO, EPV2024_rep$P203, prop.chisq = FALSE)
tabla3 <- CrossTable(EPV2024_rep$SEXO, EPV2024_rep$P203, prop.chisq = FALSE)


#Proporciones relativas por columna (segunda fila de cada celda)
##El 84% de las personas que no fueron víctimas de algún delito son hombres.
##El 85.4% de las personas que no fueron víctimas de algún delito son mujeres.
##El 16% de las personas que fueron víctimas de algún delito son hombres.
##El 14.6% de las personas que fueron víctimas de algún delito son mujeres.

#Proporciones relativas por fila (tercera fila de cada celda)
##El 46.7% de los hombres no fueron víctimas de algún delito.
##El 49.2% de los hombres fueron víctimas de algún delito.
##El 53.3% de las mujeres no fueron víctimas de algún delito.
##El 50.8% de las mujeres fueron víctimas de algún delito.

#Proporciones relativas al total general (cuarta fila de cada celda)
##El 39.6% del total son hombres que no fueron víctimas de algún delito.
##El 7.5% del total son hombres que fueron víctimas de algún delito. 
##El 45.2% del total son mujeres que no fueron víctimas de algún delito.
##El 7.7% del total son mujeres que fueron víctimas de algún delito.

##Un 15.3% del total fueron víctimas de algún delito.


#### ECN ####

ecn2024$PERCEPCION <- factor(ecn2024$PERCEPCION, ordered = TRUE)
ecn2024$P64 <- factor(ecn2024$P64, ordered = TRUE)
ecn2024$F5 <- factor(ecn2024$F5, ordered = TRUE)

ecn2024 <- ecn2024[ecn2024$P62 %in% c(1, 2), ]

ecn2024$P62 <- ifelse(ecn2024$P62 == 1, 0, 1)

logit_ordinal <- polr(P56_1 ~ F5 + PERCEPCION + P64, data = ecn2024, method = "logistic")
summary(logit_ordinal)

#las mujeres tienen menos probabilidad de tener una percepción buena de seguridad comparada con los hombres.
#F5.L dice que entre más grande la empresa, es más probable de tener una percepción buena de seguridad.
#P64.L dice que a mayor nivel de educación siendo posgrado el mayor nivel, es más probable de tener una percepción buena de seguridad.

tabla4 <- CrossTable(ecn2024$F5, ecn2024$P56_1, prop.chisq = FALSE)
tabla5 <- CrossTable(ecn2024$P62, ecn2024$P56_1, prop.chisq = FALSE)
tabla6 <- CrossTable(ecn2024$P64, ecn2024$P56_1, prop.chisq = FALSE)
