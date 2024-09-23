library(plotly)
library(shiny)
library(bslib)
library(htmltools)
library(sortable)
source("Data.R")

navpanel1 <- nav_panel(
  textOutput("navpanel1Id"),
  br(),
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
  br(),
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
  br(),
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

navpanel4 <- nav_panel(
  textOutput("navpanel4Id"),
  br(),
  sidebarLayout(
    sidebarPanel(
      sliderInput("daterange4", textOutput("daterange4Id"), min = 1977, 
                  max = 2023, value = c(1977, 2023), sep = ""),
      uiOutput("bucket_ui"),
    ),
    mainPanel(
      plotlyOutput("year_viz3")
    )
  ))

navpanel5 <- nav_panel(
  "About",
  br(),
  card( 
    card_header("About me"),
    p("Hi! My name is Ludovica. I'm a Data Scientist and a recent statistics graduate. I enjoy building applications about European economics and politics."),
    p("Contact me:"),
    layout_column_wrap(
      width = 1/2,
      tags$a(href = "https://www.linkedin.com/in/ludovicadorsa/", target = "_blank", 
             class = "btn btn-primary", 
             HTML('<i class="fa-brands fa-linkedin" style="color: #ffffff;"></i> LinkedIn')),
      tags$a(href = "https://github.com/Ludovica-DOrsa", target = "_blank", 
             class = "btn btn-primary", 
             HTML('<i class="fa-brands fa-github" style="color: #ffffff;"></i> GitHub'))
    )
  ), 
  card( 
    card_header("About the data"),
    p("The data powering this application were collected from:"),
    tags$div(
    "- ",
    tags$a(href="https://osservatoriocpi.unicatt.it/cpi-archivio-studi-e-analisi-i-numeri-della-finanza-pubblica-dal-1861-a-oggi", 
           "Osservatorio CPI")),
    tags$div(
    "- ",
    tags$a(href="https://www.istat.it/dati/banche-dati/", "Istat")),
    tags$div(
    "- ",
    tags$a(href="https://www.imf.org/external/datamapper/pb@FPP/ITA", "International Monetary Fund")),
    tags$div(
    "- ",
    tags$a(href="https://www.bancaditalia.it/statistiche/", "Banca d'Italia")),
    tags$div(
    "- ",
    tags$a(href="https://economy-finance.ec.europa.eu/economic-research-and-databases/economic-databases/ameco-database_en", "AMECO")
    )
    
  )
  )
