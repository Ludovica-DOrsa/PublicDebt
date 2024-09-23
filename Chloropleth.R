library(plotly)
library(rjson)
library(readxl)

pop <-  read_xlsx(path = "demo.xlsx", sheet = "Popolazione per Regione")
geojson <- rjson::fromJSON(file="limits_IT_regions.geojson")

g <- list(
  fitbounds = "locations",
  visible = FALSE
)

pop_chart <- pop %>%
  filter(Regione != "Totale") %>%
  group_by(Regione, Anno, `Codice regione`) %>% 
  summarise(Popolazione = sum(Totale))

fig <- plot_ly() 
fig <- fig %>% add_trace(
  type="choropleth",
  geojson=geojson,
  locations=pop_chart$`Codice regione`,
  z=pop_chart$Popolazione,
  colorscale="Viridis",
  featureidkey="properties.reg_istat_code_num",
  frame = pop_chart$Anno
)
fig <- fig %>% layout(
  geo = g
)
fig <- fig %>% colorbar(title = "Residenti Totali")
fig <- fig %>% layout(
  title = "2013 Montreal Election"
)
fig <- fig %>%
  animation_opts(
    frame = 1000,  # duration of the frame in milliseconds
    easing = "linear",
    redraw = TRUE
  ) %>%
  animation_slider(
    currentvalue = list(prefix = "Year: ")  # To display the current year
  ) %>%
  animation_button(
    label = "Play"
  )

fig