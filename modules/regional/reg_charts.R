regional_map <- relocations %>% 
  filter(to_region == "Greater Accra", date == current_month) %>% 
  group_by(from_region) %>% 
  summarise(flow = sum(flow)) %>% 
  left_join(reg_data %>% select(!population), by = c("from_region" = "region")) %>% 
  mutate(centroid = st_centroid(geom)) %>%
  st_as_sf()

# Define Greater Accra's centroid (you can manually set this or extract it from your data)
greater_accra_centroid <- regional_map %>% filter(from_region == "Greater Accra")
greater_accra_centroid <- st_coordinates(greater_accra_centroid$centroid ) %>% 
  as.data.frame()

regional_map <- regional_map[!regional_map$from_region == "Greater Accra",]
regional_map <- st_as_sf(regional_map)

regional_map <- regional_map %>% 
  mutate(x_o = st_coordinates(centroid)[,1],
         y_o = st_coordinates(centroid)[,2],
         x_d = greater_accra_centroid$X,
         y_d = greater_accra_centroid$Y) 

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

# Create curved gradient lines for each relocation
curved_gradient_lines <- regional_map %>%
  pmap_dfr(function(x_o, y_o, x_d, y_d, flow, ...) {
    create_curved_gradient(x_o, y_o, x_d, y_d, curvature = 0.2, n = 1000) %>%
      mutate(id = paste(x_o, y_o, x_d, y_d, sep = "_"), # Unique ID for each line
             flow = flow)  # Add flow_prop to the interpolated points
  })

# curved_gradient_lines <- as.data.frame(curved_gradient_lines)

inflow_to_reg <- ggplot() +
  geom_sf(
    data = geo_data, aes(geometry = geom),
    colour = "#ab976f", fill = "#EAE5DF", show.legend = FALSE) +
  geom_sf(data = regional_map, aes(geometry = geom, fill = flow)) +
  geom_sf_text(data = geo_data, aes(label = region), size = 4, colour = "black") +
  geom_path(
    data = curved_gradient_lines,
    aes(x = x, y = y, group = id, color = gradient, linewidth = flow),
    alpha = 0.8, lineend = "round", show.legend = FALSE) +
  scale_size(range = c(0.1, 2)) +  # Adjust line thickness based on flow_prop
  scale_fill_gradientn(limits = c(0, NA), 
                       colors = rev(c("#8A005E", "#992649", "#A65432", '#AB7A22', 
                                      "#A99B3B", "#A0BA69", "#98D399", "#9CE5C6",
                                      "#B7EDE6"))) +
  scale_color_gradient(high = "#A3F2CF", low = "#3D084B") +  # Gradient from light to dark
  labs(x = NULL, y = NULL, fill = NULL) +
  theme_void() +
  # theme_minimal() +
  theme(
    axis.text = element_blank(),
    panel.grid.major = element_blank()
  ) +
  guides(fill = guide_colourbar(theme = theme(
    legend.key.width  = unit(1, "lines"),
    legend.key.height = unit(20, "lines"),
    legend.text = element_text(size = 15)
  )))

ggsave("src/img/flow_svg1.svg", inflow_to_reg,
       width = 6, height = 8)


# departures
regional_map <- relocations %>% 
  filter(from_region == "Greater Accra", date == current_month) %>% 
  group_by(to_region) %>% 
  summarise(flow = sum(flow)) %>% 
  left_join(reg_data %>% select(!population), by = c("to_region" = "region")) %>% 
  mutate(centroid = st_centroid(geom)) %>%
  st_as_sf()

# Define Greater Accra's centroid (you can manually set this or extract it from your data)
greater_accra_centroid <- regional_map %>% filter(to_region == "Greater Accra")
greater_accra_centroid <- st_coordinates(greater_accra_centroid$centroid ) %>% 
  as.data.frame()

regional_map <- regional_map[!regional_map$to_region == "Greater Accra",]
regional_map <- st_as_sf(regional_map)

regional_map <- regional_map %>% 
  mutate(x_o = st_coordinates(centroid)[,1],
         y_o = st_coordinates(centroid)[,2],
         x_d = greater_accra_centroid$X,
         y_d = greater_accra_centroid$Y) 

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

# Create curved gradient lines for each relocation
curved_gradient_lines <- regional_map %>%
  pmap_dfr(function(x_o, y_o, x_d, y_d, flow, ...) {
    create_curved_gradient(x_o, y_o, x_d, y_d, curvature = -0.1, n = 1000) %>%
      mutate(id = paste(x_o, y_o, x_d, y_d, sep = "_"), # Unique ID for each line
             flow = flow)  # Add flow_prop to the interpolated points
  })

# curved_gradient_lines <- as.data.frame(curved_gradient_lines)

outflow_to_reg <- ggplot() +
  geom_sf(
    data = geo_data, aes(geometry = geom),
    colour = "#ab976f", fill = "#EAE5DF", show.legend = FALSE) +
  geom_sf(data = regional_map, aes(geometry = geom, fill = flow)) +
  geom_sf_text(data = geo_data, aes(label = region), size = 4, colour = "black") +
  geom_path(
    data = curved_gradient_lines,
    aes(x = x, y = y, group = id, color = gradient, linewidth = flow),
    alpha = 0.8, lineend = "round", show.legend = FALSE) +
  scale_size(range = c(0.1, 2)) +  # Adjust line thickness based on flow_prop
  scale_fill_gradientn(limits = c(0, NA), 
                       colors = rev(c("#8A005E", "#992649", "#A65432", '#AB7A22', 
                                      "#A99B3B", "#A0BA69", "#98D399", "#9CE5C6",
                                      "#B7EDE6"))) +
  scale_color_gradient(low = "#3D084B", high = "#A3F2CF") +  # Gradient from light to dark
  labs(x = NULL, y = NULL, fill = NULL) +
  theme_void() +
  # theme_minimal() +
  theme(
    axis.text = element_blank(),
    panel.grid.major = element_blank()
  ) +
  guides(fill = guide_colourbar(theme = theme(
    legend.key.width  = unit(1, "lines"),
    legend.key.height = unit(20, "lines"),
    legend.text = element_text(size = 15)
  )))

ggsave("src/img/outflow_svg.svg", outflow_to_reg, 
       width = 6, height = 8)
