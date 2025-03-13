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

logit_EPV2024 <- glm(PERCEPCION ~ SEXO + REDAD + ESTRATO + P203 + P500, data = EPV2024, family = binomial(link = "logit"), weights = FACTOR)
summary(logit_EPV2024)

#las mujeres tienen una probabilidad mucho mayor de sentirse inseguras
#a medida que la edad aumenta, las personas tienen menos probabilidades de sentirse inseguras
#los estratos más altos están asociados con una menor probabilidad de sentirse inseguro
#quienes fueron víctimas tienen una menor probabilidad de sentirse inseguros
#las personas con una empresa tienen menos probabilidad de sentirse inseguras

library(MASS)
logit_EPV2024_loc <- glm(P102 ~ SEXO + REDAD + ESTRATO + P203 + P500, data = EPV2024, family = binomial(link = "logit"), weights = FACTOR)
summary(logit_EPV2024_loc)

#las mujeres tienen una probabilidad mucho mayor de sentirse inseguras
#a medida que la edad aumenta, las personas tienen más probabilidades de sentirse inseguras
#los estratos más altos están asociados con una menor probabilidad de sentirse inseguro
#quienes fueron víctimas tienen una mayor probabilidad de sentirse inseguros
#las personas con una empresa tienen más probabilidad de sentirse inseguras

library(gmodels)

tabla1 <- CrossTable(EPV2024$LOCALIDAD, EPV2024$P203, prop.chisq = FALSE)
tabla2 <- CrossTable(EPV2024$ESTRATO, EPV2024$P203, prop.chisq = FALSE)
tabla3 <- CrossTable(EPV2024$SEXO, EPV2024$P203, prop.chisq = FALSE)


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