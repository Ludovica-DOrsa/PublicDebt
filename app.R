library(readxl)
library(tidyverse)
library(plotly)
library(shiny)
library(bslib)
library(htmltools)
source("Parliament_Graph.R")

show_message <- function(){message(Sys.time(), " updating hover_reactive")}

govt <- read_xlsx(path = "data.xlsx", sheet = "GOVERNI")
debt <- read_xlsx(path = "data.xlsx", sheet = "DATI")
parties <- read_xlsx(path = "data.xlsx", sheet = "PARTITI")

debt <- debt %>%
  pivot_longer(
    cols = c(starts_with("18"), starts_with("19"), starts_with("20")),
    names_to = "Year",
    values_to = "Value",
    values_drop_na = TRUE)


ui <- page_fillable(
    includeCSS("www/style.css"),

    # Application title
    titlePanel("Esplorazione del debito pubblico italiano"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("daterange", "Scegli un range di anni:", min = 1948,
                        max = 2022, value = c(2012, 2022), dragRange = TRUE,
                        sep = ""),
            selectInput("var", "Seleziona una statistica:", 
                        c("Popolazione a inizio anno",
                          "Pil reale",
                          "Pil nominale",
                          "Debito",
                          "Deficit (-)/Surplus(+)",
                          "Deficit(-)/Surplus(+) primario",
                          "Spesa per interessi",
                          "Variazione debito",
                          "Agg. stock flussi",
                          "Tasso di crescita reale (r)",
                          "Tasso crescita nominale (g)",
                          "Deflatore",
                          "Interessi impliciti (i)",
                          "Debito in valuta domestica",
                          "Debito in valuta estera",
                          "Deficit (-)/Surplus(+) operativo",
                          "Debito pro capite"),
                        selected = "Debito"),
            selectInput("um",  "Seleziona un\'unità di misura:",
                        unique(debt$`UNITA DI MISURA`)),
            uiOutput("cardUI"),
            ),
        
        mainPanel(
           plotlyOutput("year_var"),
           uiOutput("seggiUI")
           
        )
    )
)


server <- function(input, output, session) {
  
  observe({
    if(!is.null(input$var)){
      updateSelectInput(session, "um",  choices = unique(debt[debt$ANNO == 
                                                input$var, ]$`UNITA DI MISURA`),
                        selected = unique(debt[debt$ANNO == 
                                                 input$var, ]$`UNITA DI MISURA`)[1])
    }
  })

    output$year_var <- renderPlotly({
      dat <- debt %>%
        filter(Year >= input$daterange[1]) %>%
        filter(Year <= input$daterange[2]) %>%
        filter(`UNITA DI MISURA` == input$um) %>%
        filter(ANNO == input$var)
      
      plot_ly(source = "yv", dat, x = ~Year, y = ~Value, type = "bar") %>%
        layout(title = list(text = input$var, 
                            xanchor = 'left', yanchor =  'top'), 
               xaxis = list(title = ''),
               font = list(family = "Oswald", size = 16),
               yaxis = list(title = toString(input$um)))
    })
    
    hover_reactive <- reactiveVal() 
    observe({
      show_message()
      hover_data <- event_data("plotly_hover", source = "yv")
      if (!is.null(hover_data)){
        hover_reactive(hover_data) 
        
        last_govt <- govt %>%
          filter(format(govt$Inizio, format = "%Y") <= hover_data$x) %>% 
          arrange(Inizio) %>%
          slice(n())
        
        output$cardUI <- renderUI({
          div(card(
            full_screen = TRUE,
            card_header("Governo in carica: "),
            card_body(
              layout_column_wrap(
                width = 1/2, 
                card_body(
                  a(toString(last_govt$Governo), href = last_govt$Link),
                  paste0(last_govt$Inizio, " - ", last_govt$Fine),
                  last_govt$Coalizione),
                card_body(renderImage({
                  list(src = paste0(gsub(" ", "", toString(last_govt$Nome)),
                                    ".png"), width = 100)}, deleteFile = FALSE))
                   )))
          )
        })
        
        output$seggiUI <- renderUI({
          div(
            selectInput("cam",  "Seleziona una camera:",
                        unique(parties$Camera)),
            plotlyOutput("parl_plot")
          )
        })
        
        
        output$parl_plot <- renderPlotly({
          
          # parliament plot
          last_party <- parties %>%
            filter(Inizio <= hover_data$x) %>% 
            arrange(Inizio) %>% 
            filter(Inizio == max(Inizio))%>% 
            filter(Camera == input$cam)
          
          # TRANSFORM last_party
          last_party <- last_party[rep(row.names(last_party), 
                                       last_party$Seggi), ]
          last_party <- parliamentary_Coord(last_party, 
                                            angle_total = 240, 
                                            rows = 7, ratio = 6)
          
          # Create a color mapping dictionary for the plot
          d_ict <- last_party %>%
            select(Partito, Colore) %>%
            distinct()
          color_map <- setNames(d_ict$Colore, d_ict$Partito)
          
          req(input$cam)
          fig <- plot_ly(
            last_party,
            type = 'scatterpolar',
            mode = 'markers+text',
            r = ~radio,
            theta = ~tetha,
            # text = ~Partito,
            customdata = ~Nome,
            marker = list(size = 5, opacity = 0.7),
            color = ~Partito,
            colors = color_map,
            hoverinfo = 'text',
            hovertemplate = '<b>%{customdata}</b><extra></extra>',
            textposition = 'middle center'
          )
          if (input$cam == "Camera dei deputati"){
            pic_source <- "Camera.png"
          } else {
            pic_source <- "Senato.png"
          }
          
          # Update the layout to match the custom settings and properly center the plot
          fig %>%
            layout(polar = list(radialaxis = list(visible = FALSE),
                angularaxis = list(visible = FALSE, 
                                   direction = 'counterclockwise')),
              showlegend = FALSE,
              height = 500,
              margin = list(b = 20, r = 5, l = 5, t = 10),
              font = list(family = "Oswald", size = 12),
              uniformtext = list(minsize = 8, mode = "hide"),
              images = list(list(source =  base64enc::dataURI(file=pic_source), 
                                 xref = "paper", yref = "paper",
                                 x = 0.5, y = 0.5,
                                 sizex = 0.2, sizey = 0.2, 
                                 xanchor = "center", yanchor = "middle",
                                 layer = "above"
                  )
                )
            )
        })
    }
    
    
    
})
    
}

# Run the application 
shinyApp(ui = ui, server = server)
