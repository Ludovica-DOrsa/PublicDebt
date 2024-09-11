library(readxl)
library(tidyverse)
library(plotly)
library(shiny)
library(bslib)
library(htmltools)

show_message <- function(){message(Sys.time(), " updating hover_reactive")}

govt <- read_xlsx(path = "data.xlsx", sheet = "GOVERNI")
debt <- read_xlsx(path = "data.xlsx", sheet = "DATI")


debt <- debt %>%
  pivot_longer(
    cols = c(starts_with("18"), starts_with("19"), starts_with("20")),
    names_to = "Year",
    values_to = "Value",
    values_drop_na = TRUE)

# Define UI for application that draws a histogram
ui <- page_fillable(

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
                          "i-g",
                          "Debito in valuta domestica",
                          "Debito in valuta estera",
                          "Deficit (-)/Surplus(+) operativo")),
            selectInput("um",  "Seleziona un\'unità di misura:",
                        unique(debt$`UNITA DI MISURA`)),
            card(height = 350, full_screen = TRUE, 
                 card_header("Governo in carica:"),
                 card_body(layout_column_wrap(width = 1/2, 
                                              card_body(uiOutput("govt"),
                                                        textOutput("dates"),
                                                        textOutput("coal")),
                                              card_body(imageOutput("pic")))))),
        
        mainPanel(
           plotlyOutput("year_var"),
           
        )
    )
)

# Define server logic required to draw a histogram
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
      
      plot_ly(dat, x = ~Year, y = ~Value, type = "bar") %>%
        layout(xaxis = list(title = ''),
               yaxis = list(title = toString(input$um)))
    })
    
    hover_reactive <- reactiveVal() 
    observe({
      show_message()
      hover_data <- event_data("plotly_hover")
      if (!is.null(hover_data)){
        hover_reactive(hover_data) 
        
        last_govt <- govt %>%
          filter(format(govt$Inizio, format = "%Y") <= hover_data$x) %>% 
          arrange(Inizio) %>%
          slice(n())
        
        output$govt <- renderUI({
          tagList(a(toString(last_govt$Governo), 
                                 href = last_govt$Link))})
        output$pic <- renderImage({
          list(src = paste0(gsub(" ", "", toString(last_govt$Nome)), 
                                     ".png"), width = 100)}, deleteFile = FALSE)
        
        output$dates <-  renderText({
          paste0(last_govt$Inizio, " - ", last_govt$Fine)})
        
        output$coal <-  renderText({
          last_govt$Coalizione})
    }
    
    
    
})
    
}

# Run the application 
shinyApp(ui = ui, server = server)
