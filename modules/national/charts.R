gh_total <- pop_est %>% 
  mutate(date = as.Date(date)) %>% 
  filter(date == current_month) 
  
gh_total <- sum(gh_total$pop_est_cdr)

gh_trend <- pop_est %>%
  mutate(date = as.Date(date)) %>%
  filter(date >= as.Date("2024-12-01") - months(11),
         district == current_district) %>%
  group_by(date) %>%
  summarise(population = sum(pop_est_cdr)) %>%
  hchart("line", hcaes(x = date, y = population)) %>%
  hc_xAxis(type = "datetime", dateTimeLabelFormats = list(month = "%b"),
           tickInterval = 30 * 24 * 3600 * 1000) %>%
  hc_yAxis(title = list(text = "Population estimate")) %>%
  hc_title(text = "Population Estimate Over Time") %>%
  hc_add_theme(hc_theme_flat()) %>%
  hc_plotOptions(line = list(marker = list(enabled = FALSE))) %>%
  hc_tooltip(shared = TRUE, crosshairs = TRUE)


reg_data <- pop_est %>%
  mutate(date = as.Date(date)) %>%
  filter(date == current_month) %>%
  group_by(region) %>%
  summarise(population = sum(pop_est_cdr)) %>% 
  right_join(geo_data, by = "region") %>% 
  st_as_sf()


# Define color palette
pop_palette <- colorNumeric(
  palette = c("#B7EDE6", "#8A005E"), domain = reg_data$population)

# Create the map
regional_map <- leaflet() %>%
  addMapboxTiles(style_url = fm_basemap, access_token = fm_token, 
                 attribution = 'Flowminder / Mapbox', group = "Basemap") %>%
  addPolygons(
    data = reg_data,
    fillColor = ~pop_palette(population),  # Apply new color palette
    fillOpacity = 0.7,
    color = "black",
    weight = 1,
    label = ~paste(region, ": ", population),  # Show region names & population
    labelOptions = labelOptions(
      style = list("font-weight" = "bold", "color" = "black"),  # Improve readability
      textsize = "12px",
      direction = "auto"
    )
  ) %>% 
  setView(lng = -1.0232, lat = 8.0305, zoom = 6) %>%
  addLegend(
    pal = pop_palette,
    values = reg_data$population,
    title = "Population",
    position = "bottomright"
  )

