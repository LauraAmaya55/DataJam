library(readxl)
Base_EPV_2024_anonimizada <- read_excel("EPV_Encuesta de Percepción y Victimización/2024/Base EPV 2024 anonimizada.xlsx")
Base2023 <- read_excel("EPV_Encuesta de Percepción y Victimización/2023/Base2023.xlsx")
Base2022 <- read_excel("EPV_Encuesta de Percepción y Victimización/2022/Base2022.xlsx")
Base2021 <- read_excel("EPV_Encuesta de Percepción y Victimización/2021/Base2021.xlsx")
Base_2020 <- read_excel("EPV_Encuesta de Percepción y Victimización/2020/Base_2020.xlsx")

EPV2024 <- Base_EPV_2024_anonimizada[Base_EPV_2024_anonimizada$MUNICIPIO == 11001, ]
EPV2024[, 11:309][is.na(EPV2024[, 11:309])] <- 0
EPV2024$P103[EPV2024$P103 == 1] <- 0
EPV2024$P103[EPV2024$P103 == 2] <- 1
EPV2024$P102[EPV2024$P102 == 1] <- 0
EPV2024$P102[EPV2024$P102 == 2] <- 1
EPV2024$SEXO[EPV2024$SEXO == 1] <- 0
EPV2024$SEXO[EPV2024$SEXO == 2] <- 1
EPV2024$P203[EPV2024$P203 == 2] <- 0
EPV2024$P500[EPV2024$P500 == 2] <- 0
EPV2024$P121[EPV2024$P121 == 2] <- 0
for (i in 2:19) {
  column_name <- paste0("P20324", i)
  EPV2024[[column_name]][EPV2024[[column_name]] == i] <- 1
}
colnames(EPV2024)[colnames(EPV2024) == "FACTOR"] <- "FEX"
colnames(EPV2024)[colnames(EPV2024) == "P103"] <- "PERCEPCION"

EPV2023 <- Base2023
EPV2023[, 8:133] <- lapply(EPV2023[, 8:133], as.numeric)
EPV2023[, 8:133][is.na(EPV2023[, 8:133])] <- 0

EPV2023$P103[EPV2023$P103 == 1] <- 0
EPV2023$P103[EPV2023$P103 == 2] <- 1
colnames(EPV2023)[colnames(EPV2023) == "P103"] <- "PERCEPCION"

EPV2022 <- Base2022
EPV2022[, 6:124] <- lapply(EPV2022[, 6:124], as.numeric)
EPV2022[, 6:124][is.na(EPV2022[, 6:124])] <- 0
EPV2022$P103[EPV2022$P103 == 1] <- 0
EPV2022$P103[EPV2022$P103 == 2] <- 1
colnames(EPV2022)[colnames(EPV2022) == "P103"] <- "PERCEPCION"

EPV2021 <- Base2021
EPV2021[, 14:184] <- lapply(EPV2021[, 14:184], as.numeric)
EPV2021[, 14:184][is.na(EPV2021[, 14:184])] <- 0
EPV2021$P103[EPV2021$P103 == 1] <- 0
EPV2021$P103[EPV2021$P103 == 2] <- 1
colnames(EPV2021)[colnames(EPV2021) == "P103"] <- "PERCEPCION"

EPV2020 <- Base_2020
EPV2020$P103[EPV2020$P103 == "Inseguro"] <- 1
EPV2020$P103[EPV2020$P103 == "Seguro"] <- 0
colnames(EPV2020)[colnames(EPV2020) == "P103"] <- "PERCEPCION"

ecn2020$p_p64_p64[ecn2020$p_p64_p64 %in% c(1, 3)] <- 0
ecn2020$p_p64_p64[ecn2020$p_p64_p64 == 2] <- 1
colnames(ecn2020)[colnames(ecn2020) == "fexp"] <- "FEX"
colnames(ecn2020)[colnames(ecn2020) == "p_p64_p64"] <- "PERCEPCION"

ecn2021$P54[ecn2021$P54 %in% c(1, 3)] <- 0
ecn2021$P54[ecn2021$P54 == 2] <- 1
colnames(ecn2021)[colnames(ecn2021) == "P54"] <- "PERCEPCION"

ecn2022$P56[ecn2022$P56 %in% c(1, 3)] <- 0
ecn2022$P56[ecn2022$P56 == 2] <- 1
colnames(ecn2022)[colnames(ecn2022) == "F.EXPANSIÓN"] <- "FEX"
colnames(ecn2022)[colnames(ecn2022) == "P56"] <- "PERCEPCION"

ecn2023$P56[ecn2023$P56 %in% c(1, 3)] <- 0
ecn2023$P56[ecn2023$P56 == 2] <- 1
colnames(ecn2023)[colnames(ecn2023) == "PONDERA"] <- "FEX"
colnames(ecn2023)[colnames(ecn2023) == "P56"] <- "PERCEPCION"

ecn2024$P56[ecn2024$P58 == 2] <- 0
colnames(ecn2024)[colnames(ecn2024) == "FACTOR_REGION"] <- "FEX"
colnames(ecn2024)[colnames(ecn2024) == "P56"] <- "PERCEPCION"

datasets <- list(EPV2024 = EPV2024, EPV2023 = EPV2023, EPV2022 = EPV2022, EPV2021 = EPV2021, EPV2020 = EPV2020)
for (nombre in names(datasets)) {
  write.csv(datasets[[nombre]], file = paste0("C:/Users/Asus/Documents/2025/DataJam/", nombre, ".csv"), row.names = FALSE)
}

