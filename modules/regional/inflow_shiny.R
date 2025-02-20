# Load required libraries
library(shiny)
library(ggplot2)
library(sf)
library(dplyr)
library(purrr)

# source("modules/imports.R")
# source("modules/regional/inflow_reg.R")
pop_est = read.csv('/Users/agyei_boadi/Documents/there/pop_release/data/GHA_pop_estimates_2022_2024.csv')
relocations = read.csv('/Users/agyei_boadi/Documents/there/pop_release/data/GHA_reloc_estimates_2020_2024.csv') %>%
  mutate(date = as.Date(date))

# pop_projection = read.csv('data/Population Projections_20250214-151840.csv')

geo_data = st_read('../../data/gss_phc2021_bnd_edgematched_20240821151831.gpkg',
                   layer = "gss_phc2021_16_regions", quiet = TRUE) %>%
  st_make_valid() %>%
  select(!region) %>%
  rename(region = region_name)

# Function to create curved lines with gradient colors
create_curved_gradient <- function(x1, y1, x2, y2, curvature = 0.2, n = 100) {
  # Calculate control points for the Bezier curve
  mid_x <- (x1 + x2) / 2
  mid_y <- (y1 + y2) / 2
  control_x <- mid_x + curvature * (y2 - y1)
  control_y <- mid_y - curvature * (x2 - x1)
  
  # Generate points along the Bezier curve
  t <- seq(0, 1, length.out = n)
  x <- (1 - t)^2 * x1 + 2 * (1 - t) * t * control_x + t^2 * x2
  y <- (1 - t)^2 * y1 + 2 * (1 - t) * t * control_y + t^2 * y2
  
  tibble(
    x = x,
    y = y,
    gradient = t  # Gradient from 0 (origin) to 1 (destination)
  )
}



# Define UI
ui <- fluidPage(
  selectInput("region", "Select Region:",
              choices = unique(relocations$to_region),
              selected = "Greater Accra"),
  selectInput("date", "Select Date:",
              choices = unique(relocations$date),
              selected = max(relocations$date)),
  plotOutput("flow_map")
)

# Define server logic
server <- function(input, output, session) {
  
  # Reactive expression to filter data based on selected region and date
  filtered_data <- reactive({
    relocations %>%
      filter(to_region == input$region, date == input$date) %>%
      group_by(from_region) %>%
      summarise(flow = sum(flow)) %>%
      left_join(reg_data %>% select(!population), by = c("from_region" = "region")) %>%
      mutate(centroid = st_centroid(geom)) %>%
      st_as_sf()
  })
  
  # Reactive expression to calculate the centroid of the selected region
  selected_region_centroid <- reactive({
    selected_region_centroid <- filtered_data() %>%
      filter(from_region == input$region) 
    
    selected_region_centroid <- st_coordinates(selected_region_centroid$centroid ) %>% 
      as.data.frame()
    selected_region_centroid
  })
  
    # Reactive expression to calculate the centroid of the selected region
  # filtered_data <- reactive({
  #   
  #   filtered_data <- filtered_data[!filtered_data$from_region == input$region,]
  #   filtered_data <- st_as_sf(filtered_data)
  #   filtered_data
  # })
  
  
  # Reactive expression to create curved gradient lines
  curved_gradient_lines <- reactive({
    filtered_data() %>%
      filter(!from_region == input$region) %>%
      mutate(
        x_o = st_coordinates(centroid)[, 1],
        y_o = st_coordinates(centroid)[, 2],
        x_d = selected_region_centroid()$X,
        y_d = selected_region_centroid()$Y
      ) %>%
      pmap_dfr(function(x_o, y_o, x_d, y_d, flow, ...) {
        create_curved_gradient(x_o, y_o, x_d, y_d, curvature = 0.2, n = 1000) %>%
          mutate(id = paste(x_o, y_o, x_d, y_d, sep = "_"), flow = flow)
      })
  })
  
  # Render the flow map using ggplot2
  output$flow_map <- renderPlot({
    inflow <- ggplot() +
      geom_sf(
        data = geo_data, aes(geometry = geom),
        colour = "#ab976f", fill = "#EAE5DF", show.legend = FALSE) +
      geom_sf(data = filtered_data(), aes(geometry = geom, fill = flow)) +
      geom_path(
        data = curved_gradient_lines(),
        aes(x = x, y = y, group = id, color = gradient, linewidth = flow),
        alpha = 0.8, lineend = "round", show.legend = FALSE) +
      scale_size(range = c(0.1, 2)) +
      scale_fill_gradientn(limits = c(0, NA),
                           colors = rev(c("#8A005E", "#992649", "#A65432", '#AB7A22',
                                          "#A99B3B", "#A0BA69", "#98D399", "#9CE5C6",
                                          "#B7EDE6"))) +
      scale_color_gradient(high = "#A3F2CF", low = "#3D084B") +
    labs(x = NULL, y = NULL, fill = NULL) +
    theme_void() +
    theme(
      axis.text = element_blank(),
      panel.grid.major = element_blank()
    ) +
    guides(fill = guide_colourbar(theme = theme(
      legend.key.width  = unit(1, "lines"),
      legend.key.height = unit(20, "lines"),
      legend.text = element_text(size = 15)
    )))
    
    ggsave("flow_svg1.svg", inflow)
    inflow
    
    })
}

shinyApp(ui, server)
