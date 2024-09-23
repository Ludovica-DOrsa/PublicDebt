library(readxl)
library(tidyverse)
library(rjson)

govt <- read_xlsx(path = "data.xlsx", sheet = "GOVERNI")
occ <- read_xlsx(path = "occupation.xlsx", sheet = "Totale")
debt <- read_xlsx(path = "data.xlsx", sheet = "DATI")
parties <- read_xlsx(path = "data.xlsx", sheet = "PARTITI")
pop <-  read_xlsx(path = "demo.xlsx", sheet = "Popolazione per Regione")
pop <-  pop[!is.na(pop$Regione),]
bil <-  read_xlsx(path = "demo.xlsx", sheet = "Bilancio")
bil <-  bil[!is.na(bil$Regione),]
geojson <- rjson::fromJSON(file="limits_IT_regions.geojson")
g <- list(fitbounds = "locations", visible = FALSE)

lista_input1eng <- c("Real GDP" = "Pil reale", 
                     "Nominal GDP" = "Pil nominale",
                     "Debt" = "Debito",
                     "Deficit (-)/Surplus(+)" = "Deficit (-)/Surplus(+)",
                     "Primary Deficit(-)/Surplus(+)" = "Deficit(-)/Surplus(+) primario",
                     "Interest expenditure" = "Spesa per interessi",
                     "Variation in debt" = "Variazione debito",
                     "Agg. stock flow" = "Agg. stock flussi",
                     "Real growth rate (r)" = "Tasso di crescita reale (r)",
                     "Nominal growth rate (g)" = "Tasso crescita nominale (g)",
                     "Deflator" = "Deflatore",
                     "Implicit interest (i)" = "Interessi impliciti (i)",
                     "Domestic currency debt" = "Debito in valuta domestica",
                     "Foreign currency debt" = "Debito in valuta estera",
                     "Operational Deficit (-)/Surplus(+)" = "Deficit (-)/Surplus(+) operativo",
                     "Per capita debt" = "Debito pro capite")

lista_input2eng <- c("Natural balance" = "Saldo naturale",
  "Foreign migration balance" = "Saldo migratorio con l'estero")

debt <- debt %>%
  pivot_longer(
    cols = c(starts_with("18"), starts_with("19"), starts_with("20")),
    names_to = "Year",
    values_to = "Value",
    values_drop_na = TRUE)