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

# make regional map
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
      direction = "auto")) %>% 
  setView(lng = -1.0232, lat = 8.0305, zoom = 6) %>%
  addLegend(
    pal = pop_palette,
    values = reg_data$population,
    title = "Population",
    position = "bottomright"
  )

# compare population projection and the current population
pop_projected <- pop_projection %>% 
  left_join(as.data.frame(reg_data) %>% select(region, population), 
            by = c("Geographic_Area"="region"))

pop_projected$population[1] <- gh_total
pop_projected <- pop_projected %>% 
  filter(!Geographic_Area == "Ghana") %>% 
  mutate(projections = as.numeric(projections), 
                        population = as.numeric(population),
         id = row_number()) %>% 
  mutate(id = id - 1,
         dif = c(population - projections)/population *100) %>%
  mutate(lit = ifelse(dif > 0, TRUE, FALSE)) 
  # pivot_longer(c(projections, population), names_to = "type", values_to = "values")

projected_diff <- pop_projected %>% 
  ggplot(aes(reorder(Geographic_Area, dif), dif, fill = lit)) +
  geom_col(width = .8, show.legend = FALSE) +
  scale_fill_manual(values = c('TRUE' = "#27B288", "FALSE" = "#701F53")) +
  labs(fill = NULL, y = "Percent", x = NULL, title = "Percent Difference Between Dynamic Population and Projections Population") +
  theme_minimal() +
  theme(axis.ticks = element_line(),
        panel.grid.major.x = element_blank(),
        axis.text.x = element_text(angle = 45))


projected_diff <- ggplotly(projected_diff) %>% 
  hide_legend()

# compute relocations
regional_map <- relocations %>% 
  filter(to_region == "Greater Accra", date == current_month,
         !from_region == "Greater Accra") %>% 
  group_by(from_region) %>% 
  summarise(flow = sum(flow)) %>% 
  left_join(reg_data %>% select(!population), by = c("from_region" = "region")) %>% 
  # mutate(centroid = st_centroid(geom)) %>% 
  st_as_sf()

# Update color palette
reg_palette <- colorNumeric(
  palette = c("#B7EDE6", "#8A005E"), domain = regional_map$flow)

# relocations to greater accra
regional_map <- leaflet() %>%
  addMapboxTiles(style_url = fm_basemap, access_token = fm_token,
                 attribution = 'Flowminder / Mapbox', group = "Basemap") %>%
  addPolygons(
    data = regional_map,
    color = "#CBA45A", weight = 1, fillColor = ~reg_palette(flow), 
    fillOpacity = .8,
    label = ~paste(from_region, ":", flow),  # Add region names as labels
    labelOptions = labelOptions(
      style = list("font-weight" = "bold", "color" = "black"),  # Improve readability
      textsize = "12px",
      direction = "auto"),
    highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE)
  ) %>% 
  setView(lng = -1.0232, lat = 8.0305, zoom = 6) %>%
  addLegend(
    pal = reg_palette,
    values = regional_map$flow,
    title = "Population",
    position = "bottomright"
  )

