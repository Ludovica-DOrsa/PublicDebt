library(readxl)
library(tidyverse)
library(plotly)
library(shiny)
library(bslib)
library(htmltools)
library(rjson)
source("Parliament_Graph.R")
source("Data.R")
source("Navpanels.R")

show_message <- function(){message(Sys.time(), " updating hover_reactive")}

# ------------ UI --------------------------------------------------------------

ui <- page_fluid(
    includeCSS("www/style.css"),
    tags$head(
      tags$link(rel = "stylesheet", 
                href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.0.0-beta3/css/all.min.css")
    ),

    # Application title
    layout_columns(
    titlePanel(textOutput("titleId")),
    tags$div(style = "display: inline-block; cursor: pointer; text-align: right;", 
             title = "Change language",
             id = "toggleButton", tags$img(src = 'italy.png', 
                                           height = "25px", id = "flag_image",
                                           position = "fixed-right" )),
    tags$script(HTML("
    // Toggle the flag image when the custom button is clicked
    document.getElementById('toggleButton').onclick = function() {
      Shiny.setInputValue('toggleFlag', Math.random());
    };
    
    Shiny.addCustomMessageHandler('toggleFlag', function(flag) {
      var img = document.getElementById('flag_image');
      if(flag === 'italy') {
        img.src = 'united-kingdom.png';  
      } else {
        img.src = 'italy.png';  
      }
    });
  ")),
    
    col_widths = c(10, 2), 
    style='padding:20px;'
    ),
    
    navset_tab( 
      navpanel1, 
      navpanel2,
      navpanel3,
      navpanel4,
      navpanel5,
      id = "tab")
)


server <- function(input, output, session) {
  
  flagState <- reactiveVal("uk") 
  observeEvent(input$toggleFlag, {
    if(flagState() == "uk") {
      flagState("italy")  
    } else {
      flagState("uk")  
    }
    
    session$sendCustomMessage("toggleFlag", flagState())
  })
  
  observeEvent(flagState(), {
    if (flagState() == "italy") {
      output$titleId <- renderText({"Esplorazione della macroeconomia italiana"})
      output$navpanel1Id <- renderText({"Debito pubblico"})
      output$navpanel2Id <- renderText({"Popolazione"})
      output$navpanel3Id <- renderText({"Bilancio demografico"})
      output$var3Id <- output$var1Id  <- renderText({"Seleziona una statistica:"})
      output$umId  <- renderText({"Seleziona un\'unità di misura:"})
      output$daterange2Id  <- renderText({"Scegli un anno:"})
      output$daterange4 <- output$daterange3Id <- output$range1Id <- 
        renderText({"Scegli un range di anni:"})
      output$viz1Id  <- renderText({"Seleziona una visualizzazione:"})
      output$reg2Id  <- renderText({"Seleziona una regione:"})
      output$reg1Id  <- renderText({"Seleziona una regione:"})
      output$navpanel4Id <- renderText({"Lavoro"})
      output$bucket_ui <- renderUI({fluidRow(
        column(
          width = 12, bucket_list(header=NULL,
            group_name = "bucket_list_group", orientation = "horizontal",
            add_rank_list(
              text = "Variabili disponibili", 
              labels = unique(occ$Variabile),
              input_id = "rank_list_1"),
            add_rank_list(text = "Variabili selezionate", labels = NULL, 
                          input_id = "rank_list_2")
          )
        )
      )})
    } else {
      output$titleId <- renderText({"Italian macroeconomics explorer"})
      output$navpanel1Id <- renderText({"Public debt"})
      output$navpanel2Id <- renderText({"Population"})
      output$navpanel3Id <- renderText({"Demographic balance"})
      output$navpanel4Id  <- renderText({"Employment"})
      output$var3Id <- output$var1Id  <- renderText({"Select a statistic:"})
      output$umId  <- renderText({"Select a unit of measurement:"})
      output$daterange2Id  <- renderText({"Pick a year:"})
      output$daterange4 <- output$daterange3Id <- output$range1Id <- 
        renderText({"Pick a year range:"})
      output$viz1Id  <- renderText({"Select a visualization:"})
      output$reg2Id  <- renderText({"Select a region:"})
      output$reg1Id  <- renderText({"Select a region:"})
      updateSelectInput(session, "var",
                        choices = c("Real GDP" = "Pil reale", 
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
                                    "Per capita debt" = "Debito pro capite"),
                        selected = "Debito")
      
      output$bucket_ui <- renderUI({fluidRow(
        column(
          width = 12, bucket_list(header=NULL,
            group_name = "bucket_list_group", orientation = "horizontal",
            add_rank_list(
              text = "Available variables", 
              labels = unique(occ$Variable),
              input_id = "rank_list_1"),
            add_rank_list(text = "Selected variables", labels = NULL, 
                          input_id = "rank_list_2")
          )
        )
      )})
      
      updateSelectInput(session, "viz",
                        choices = c("Map" = "Mappa", 
                                    "Pyramid" = "Piramide"),
                        selected = "Piramide")
      
      updateSelectInput(session, "reg",  
                  choices = c("Piemonte" = "Piemonte",
                    "Valle d'Aosta/Vallée d'Aoste" = "Valle d'Aosta/Vallée d'Aoste",
                    "Lombardia" = "Lombardia",
                    "Trentino-Alto Adige/Südtirol" = "Trentino-Alto Adige/Südtirol",
                    "Veneto" = "Veneto",
                    "Friuli-Venezia Giulia" = "Friuli-Venezia Giulia",
                    "Liguria" = "Liguria",
                    "Emilia-Romagna" = "Emilia-Romagna",
                    "Toscana" = "Toscana",
                    "Umbria" = "Umbria",
                    "Marche" = "Marche",
                    "Lazio" = "Lazio",
                    "Abruzzo" = "Abruzzo",
                    "Molise" = "Molise",
                    "Campania" = "Campania",
                    "Puglia" = "Puglia",
                    "Basilicata" = "Basilicata",
                    "Calabria" = "Calabria",
                    "Sicilia" = "Sicilia",
                    "Sardegna" = "Sardegna",
                    "Total" = "Totale"
                  ), selected = "Totale")
      
      updateSelectInput(session, "reg2",  
                        choices = c("Piemonte" = "Piemonte",
                          "Valle d'Aosta/Vallée d'Aoste" = "Valle d'Aosta/Vallée d'Aoste",
                          "Lombardia" = "Lombardia",
                          "Trentino-Alto Adige/Südtirol" = "Trentino-Alto Adige/Südtirol",
                          "Veneto" = "Veneto",
                          "Friuli-Venezia Giulia" = "Friuli-Venezia Giulia",
                          "Liguria" = "Liguria",
                          "Emilia-Romagna" = "Emilia-Romagna",
                          "Toscana" = "Toscana",
                          "Umbria" = "Umbria",
                          "Marche" = "Marche",
                          "Lazio" = "Lazio",
                          "Abruzzo" = "Abruzzo",
                          "Molise" = "Molise",
                          "Campania" = "Campania",
                          "Puglia" = "Puglia",
                          "Basilicata" = "Basilicata",
                          "Calabria" = "Calabria",
                          "Sicilia" = "Sicilia",
                          "Sardegna" = "Sardegna",
                          "Total" = "Totale"
                        ), selected = "Totale")
      
      updateSelectInput(session, "var3",
                        choices = c("Natural balance" = "Saldo naturale",
                                    "Foreign migration balance" = "Saldo migratorio con l'estero"), 
                        selected = "Saldo naturale")
      
      
    }
  })
  
  # ------------ Debito pubblico ---------------------------------
  
  
  observe({
    if(!is.null(input$var)){
      if (flagState() == "italy") {
      updateSelectInput(session, "um",  choices = unique(debt[debt$ANNO == 
                                                input$var, ]$`UNITA DI MISURA`),
                        selected = unique(debt[debt$ANNO == 
                                                 input$var, ]$`UNITA DI MISURA`)[1])
      } else {
        updateSelectInput(session, "um",  choices = unique(debt[debt$ANNO == 
                                                                  input$var, ]$UNIT),
                          selected = unique(debt[debt$ANNO == 
                                                   input$var, ]$UNIT)[1])
      }
      }
  })
  
    output$year_var <- renderPlotly({
      
      if (flagState() == "italy") {
      dat <- debt %>%
        filter(Year >= input$daterange[1]) %>%
        filter(Year <= input$daterange[2]) %>%
        filter(`UNITA DI MISURA` == input$um) %>%
        filter(ANNO == input$var)}
      else {
        dat <- debt %>%
          filter(Year >= input$daterange[1]) %>%
          filter(Year <= input$daterange[2]) %>%
          filter(UNIT == input$um) %>%
          filter(ANNO == input$var)}
      
      
      if (flagState() == "italy") {
      plot_ly(source = "yv", dat, x = ~Year, y = ~Value, type = "bar") %>%
        layout(title = list(text = toString(input$var), 
                            xanchor = 'left', yanchor =  'top'), 
               xaxis = list(title = ''),
               font = list(family = "Oswald", size = 16),
               yaxis = list(title = toString(input$um)))
      } else {
        plot_ly(source = "yv", dat, x = ~Year, y = ~Value, type = "bar") %>%
          layout(title = list(text = toString(names(lista_input1eng[
            lista_input1eng==input$var])), 
            xanchor = 'left', yanchor =  'top'), 
                 xaxis = list(title = ''),
                 font = list(family = "Oswald", size = 16),
                 yaxis = list(title = toString(input$um)))
      }
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
        
        if (flagState() == "italy") {
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
          )})
        
        output$seggiUI <- renderUI({
          div(
            selectInput("cam",  "Seleziona una camera:",
                        unique(parties$Camera)),
            plotlyOutput("parl_plot")
          )
        })
        
        } else {
          output$cardUI <- renderUI({
          div(card(
            full_screen = TRUE,
            card_header("Government in office: "),
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
          )})
          
          output$seggiUI <- renderUI({
            div(
              selectInput("cam",  "Select a chamber:",
                          choices = c("Chamber of Deputies" = 
                                        "Camera dei deputati",
                                      "Senate of the Italian Republic" = 
                                        "Senato della Repubblica")),
              plotlyOutput("parl_plot")
            )
          })
          
        }
        
        
        
        
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
          fig <- plot_ly(last_party, type = 'scatterpolar', 
                         mode = 'markers+text', r = ~radio, theta = ~tetha,
                         customdata = ~Nome, 
                         marker = list(size = 5, opacity = 0.7),
                         color = ~Partito, colors = color_map, 
                         hoverinfo = 'text',
                         hovertemplate = '<b>%{customdata}</b><extra></extra>',
                         textposition = 'middle center')
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
    # ------------ Demografia ------------------------------------------------
    observe(
      # ----------------------------------"Popolazione a inizio anno"-----------
     if (input$viz == "Mappa"){
       output$year_map <- renderPlotly({
         
         pop_chart <- pop %>%
           filter(Regione != "Totale", Anno == input$daterange2) %>%
           group_by(Regione, Anno, `Codice regione`) %>% 
           summarise(Popolazione = sum(Totale))
         
         fig <- plot_ly() 
         
         if (flagState() == "italy") {
         
         fig %>% 
           add_trace(type="choropleth", geojson=geojson, 
                     locations=pop_chart$`Codice regione`, 
                     z=pop_chart$Popolazione, colorscale="Viridis",
                     featureidkey="properties.reg_istat_code_num") %>% 
           layout(geo = g, 
                  title = "Popolazione residente per regione",
                  font = list(family = "Oswald", size = 16), yaxis = list(title = "")) %>% 
           colorbar(title = "Residenti Totali")
         } else {
           fig %>% 
             add_trace(type="choropleth", geojson=geojson, 
                       locations=pop_chart$`Codice regione`, 
                       z=pop_chart$Popolazione, colorscale="Viridis",
                       featureidkey="properties.reg_istat_code_num") %>% 
             layout(geo = g, 
                    title = "Resident population by region",
                    font = list(family = "Oswald", size = 16), yaxis = list(title = "")) %>% 
             colorbar(title = "Total residents")
         }
      })}
     else{
       
       output$year_map <- renderPlotly({
         
         if (input$reg == "Totale"){
           pop_chart <- pop %>%
             filter(Anno == input$daterange2) %>%
             pivot_longer(c("Maschi", "Femmine"), names_to = "Genere", 
                          values_to = "Popolazione") %>%
             mutate(POP = if_else(Genere == "Maschi", 
                                  Popolazione, -Popolazione)) %>%
             group_by(Anno, Età, Genere) %>% 
             summarise(POP = sum(POP))
           
         } else {
           
           pop_chart <- pop %>%
             filter(Regione == input$reg, Anno == input$daterange2) %>%
             pivot_longer(c("Maschi", "Femmine"), names_to = "Genere", 
                          values_to = "Popolazione") %>%
             mutate(POP = if_else(Genere == "Maschi", 
                                  Popolazione, -Popolazione))
           
         }
         
         pop_chart$Età <- factor(pop_chart$Età, levels = sort(unique(as.numeric(pop_chart$Età))))
         
         if (flagState() == "italy") {
         
         plot_ly(data = pop_chart, type = "bar", orientation = "h", 
                 name = ~Genere, color = ~Genere,
                 x = ~POP, y = ~Età) %>%
           layout(title = "", 
                  font = list(family = "Oswald", size = 16), 
                  barmode = "relative", 
                  xaxis = list(title = "Popolazione residente per età"),
                  yaxis = list(title = "")
                  )
         } else {
           pop_chart <- pop_chart %>%
             mutate(gender_eng = ifelse(Genere == "Maschi", "Males", "Females"))
           
           plot_ly(data = pop_chart, type = "bar", orientation = "h", 
                   name = ~gender_eng, color = ~gender_eng,
                   x = ~POP, y = ~Età) %>%
             layout(title = "", 
                    font = list(family = "Oswald", size = 16), 
                    barmode = "relative", 
                    xaxis = list(title = "Resident population by age"),
                    yaxis = list(title = "")
             )
         }
       })})
       
       # ----------------------------------"Bilancio demografico"-----------


    observe(
      if(input$var3=="Saldo naturale"){
         output$year_map2 <- renderPlotly({
           
           if (input$reg2 == "Totale"){
             bil_chart <- bil %>%
               filter(Anno >= input$daterange3[1], Anno <= input$daterange3[2]) %>%
               pivot_longer(c("Nati vivi", "Morti"), names_to = "Totale", 
                            values_to = "N") %>%
               mutate(POP = if_else(Totale == "Nati vivi", 
                                    N, -N)) %>%
               group_by(Anno, Totale) %>% 
               summarise(POP = sum(POP))
             
           } else {
             
             bil_chart <- bil %>%
               filter(Regione == input$reg2, Anno >= input$daterange3[1], 
                      Anno <= input$daterange3[2]) %>%
               pivot_longer(c("Nati vivi", "Morti"), names_to = "Totale", 
                            values_to = "N") %>%
               mutate(POP = if_else(Totale == "Nati vivi", 
                                    N, -N))
             
           }
           
           if (flagState() == "italy") {
           
           plot_ly(data = bil_chart, type = "bar", orientation = "h", 
                   name = ~Totale, color = ~Totale,
                   x = ~POP, y = ~Anno) %>%
             layout(title = "", 
                    font = list(family = "Oswald", size = 16), 
                    barmode = "relative", 
                    xaxis = list(title = "Saldo naturale"),
                    yaxis = list(title = "")
             )
           } else {
             bil_chart <- bil_chart %>%
               mutate(Totale_ENG = ifelse(Totale == "Nati vivi", "Live births", "Deaths"))
             plot_ly(data = bil_chart, type = "bar", orientation = "h", 
                     name = ~Totale_ENG, color = ~Totale_ENG,
                     x = ~POP, y = ~Anno) %>%
               layout(title = "", 
                      font = list(family = "Oswald", size = 16), 
                      barmode = "relative", 
                      xaxis = list(title = "Natural balance"),
                      yaxis = list(title = "")
                      )
           }
         })
         
         ##
         } else {
           output$year_map2 <- renderPlotly({
             
             if (input$reg2 == "Totale"){
               if (flagState() == "italy") {
               bil_chart <- bil %>%
                 filter(Anno >= input$daterange3[1], Anno <= input$daterange3[2]) %>%
                 pivot_longer(c("Immigrati dall'estero", "Emigrati per l'estero"), 
                              names_to = "Totale", 
                              values_to = "N") %>%
                 mutate(POP = if_else(Totale == "Immigrati dall'estero", 
                                      N, -N)) %>%
                 group_by(Anno, Totale) %>% 
                 summarise(POP = sum(POP))
               } else {
                 bil_chart <- bil %>%
                   filter(Anno >= input$daterange3[1], Anno <= input$daterange3[2]) %>%
                   pivot_longer(c("Immigrati dall'estero", "Emigrati per l'estero"), 
                                names_to = "Totale", 
                                values_to = "N") %>%
                   mutate(POP = if_else(Totale == "Immigrati dall'estero", 
                                        N, -N), 
                          Totale == if_else(Totale == "Immigrati dall'estero",
                                            "Immigrated from abroad", 
                                            "Emigrated abroad")) %>%
                   group_by(Anno, Totale) %>% 
                   summarise(POP = sum(POP))
             }
               } else {
                 if (flagState() == "italy") {
               bil_chart <- bil %>%
                 filter(Regione == input$reg2, Anno >= input$daterange3[1], 
                        Anno <= input$daterange3[2]) %>%
                 pivot_longer(c("Immigrati dall'estero", "Emigrati per l'estero"), names_to = "Totale", 
                              values_to = "N") %>%
                 mutate(POP = if_else(Totale == "Immigrati dall'estero", 
                                      N, -N)) 
                 } else {
                   bil_chart <- bil %>%
                     filter(Regione == input$reg2, Anno >= input$daterange3[1], 
                            Anno <= input$daterange3[2]) %>%
                     pivot_longer(c("Immigrati dall'estero", "Emigrati per l'estero"), names_to = "Totale", 
                                  values_to = "N") 
                   bil_chart <- bil_chart %>%
                     mutate(POP = if_else(Totale == "Immigrati dall'estero", 
                                          N, -N))
                   print(bil_chart)
               }
               
             }
             if (flagState() == "italy") {
             plot_ly(data = bil_chart, type = "bar", orientation = "h", 
                     name = ~Totale, color = ~Totale,
                     x = ~POP, y = ~Anno) %>%
               layout(title = "", 
                      font = list(family = "Oswald", size = 16), 
                      barmode = "relative", 
                      yaxis = list(title = ""),
                      xaxis = list(title = "Saldo migratorio con l'estero"))
               } else {
                bil_chart <- bil_chart %>%
                   mutate(Totale_ENG = if_else(Totale == "Immigrati dall'estero",
                                           "Immigrated from abroad", 
                                           "Emigrated abroad"))
                 plot_ly(data = bil_chart, type = "bar", orientation = "h", 
                         name = ~Totale_ENG, color = ~Totale_ENG,
                         x = ~POP, y = ~Anno) %>%
                   layout(title = "", 
                          font = list(family = "Oswald", size = 16), 
                          barmode = "relative", 
                          yaxis = list(title = ""),
                          xaxis = list(title = "Foreign migration balance"))
                 
               }
           })
      }
      )
    
    # ----------------------------------"Lavoro"-----------
    
    observe(print(input$rank_list_2))
    observe(
      if (length(input$rank_list_2) >0){
        output$year_viz3 <- renderPlotly({
      if (flagState() == "italy") {
        occ_plot <- occ %>%
          filter(Anno >= input$daterange4[1], 
                 Anno <= input$daterange4[2],
                 Variabile %in% input$rank_list_2) %>%
          mutate(Valore = as.numeric(Valore))
        
          plot_ly(occ_plot, x = "Anno", y = "Valore", group_by = "Variabile", 
                  type = 'scatter', mode = 'lines') %>%
            layout(xaxis = list(rangeslider = list(visible = T), title = ""),
                   font = list(family = "Oswald", size = 16), 
                   yaxis = list(title = "%"))
      } else {
        occ_plot <- occ %>%
          filter(Anno >= input$daterange4[1], 
                 Anno <= input$daterange4[2],
                 Variable %in% input$rank_list_2) %>%
          mutate(Valore = as.numeric(Valore))
        
          plot_ly(occ_plot, x = ~Anno, y = ~Valore, color = ~Variable, 
                  type = 'scatter', mode = 'lines') %>%
            layout(xaxis = list(rangeslider = list(visible = T), title = ""),
                   font = list(family = "Oswald", size = 16), 
                   yaxis = list(title = "%"))
        
      }
        }
    )}
    )
 
}

# Run the application 
shinyApp(ui = ui, server = server)
