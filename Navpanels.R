library(plotly)
library(shiny)
library(bslib)
library(htmltools)

navpanel1 <- nav_panel(
  textOutput("navpanel1Id"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("daterange", textOutput("range1Id"), min = 1948, max = 2022, 
                  value = c(2012, 2022),  dragRange = TRUE, sep = ""),
      selectInput("var", textOutput("var1Id"), 
                  choices = c("Pil reale", "Pil nominale", "Debito", 
                              "Deficit (-)/Surplus(+)", 
                              "Deficit(-)/Surplus(+) primario", 
                              "Spesa per interessi", "Variazione debito", 
                              "Agg. stock flussi", "Tasso di crescita reale (r)",
                              "Tasso crescita nominale (g)", "Deflatore", 
                              "Interessi impliciti (i)", 
                              "Debito in valuta domestica", 
                              "Debito in valuta estera", 
                              "Deficit (-)/Surplus(+) operativo", 
                              "Debito pro capite"), selected = "Debito"),
      selectInput("um",  textOutput("umId"), unique(debt$`UNITA DI MISURA`)),
      uiOutput("cardUI")
      ),
    mainPanel(plotlyOutput("year_var"),
              uiOutput("seggiUI"))
    )
  )


navpanel2 <- nav_panel(
  textOutput("navpanel2Id"), 
  sidebarLayout(
    sidebarPanel(
      sliderInput("daterange2", textOutput("daterange2Id"), min = 1992, 
                  max = 2024, value = c(1992), sep = "", animate = TRUE),
      selectInput("viz",  textOutput("viz1Id"), c("Mappa", "Piramide"), 
                  selected = "Piramide"),
      selectInput("reg",  textOutput("reg1Id"), c("Piemonte", 
                                                  "Valle d'Aosta/Vallée d'Aoste",
                                                  "Lombardia", 
                                                  "Trentino-Alto Adige/Südtirol",
                                                  "Veneto", 
                                                  "Friuli-Venezia Giulia",
                                                  "Liguria", 
                                                  "Emilia-Romagna", 
                                                  "Toscana",
                                                  "Umbria", "Marche", "Lazio", 
                                                  "Abruzzo", "Molise", 
                                                  "Campania","Puglia",
                                                  "Basilicata", "Calabria", 
                                                  "Sicilia", "Sardegna", 
                                                  "Totale"), selected = "Totale")
      ),
    mainPanel(
      plotlyOutput("year_map"))
          ))


navpanel3 <- nav_panel(
  textOutput("navpanel3Id"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("daterange3", textOutput("daterange3Id"), min = 1994, 
                  max = 2022, value = c(1994, 2022), sep = ""),
      
      selectInput("var3", textOutput("var3Id"), c("Saldo naturale",
                                                  "Saldo migratorio con l'estero"),
                  selected = "Saldo naturale"),
      selectInput("reg2",  textOutput("reg2Id"), unique(pop$Regione), 
                  selected = "Totale")
      ),
    mainPanel(
      plotlyOutput("year_map2")
      )
    ))