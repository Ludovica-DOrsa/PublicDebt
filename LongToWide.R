library(readxl)
library(openxlsx)
library(tidyverse)

data <- read_xlsx(path = "demo.xlsx", sheet = "Popolazione femminile")

data <- data %>%
  pivot_longer(
    cols = c(names(data[, !names(data) %in% c("Anno", "Codice regione", "Regione")])),
    names_to = "Età",
    values_to = "Femmine",
    values_drop_na = TRUE
  )

write.xlsx(data, 'pop.xlsx')