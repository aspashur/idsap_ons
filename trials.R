# projected_diff <- pop_projected %>%
#   ggplot(aes(values, Geographic_Area, group = Geographic_Area, colour = type)) +
#   geom_line(colour = "#27A0CC") +
#   geom_point(size = 2) +
#   labs(colour = NULL) +
#   scale_colour_manual(values = c("population" = "#034174", "projections" = "#CBA45A")) +
#   scale_x_continuous(labels = scales::comma, limits = nicelimits) +
#   theme_minimal() +
#   theme(panel.grid.minor.x = element_blank(),
#         panel.grid.major.x = element_blank(),
#         axis.ticks = element_line(),
#         legend.position = "top")

pop_projected %>% 
  hchart("scatter", hcaes(x=values, y=id, group = id)) %>% 
  hchart("line", hcaes(x=values, y=id, group = id)) 

highchart() %>%
  hc_add_series(
    data = pop_projected,
    type = "line",
    hcaes(x = values, y = id, group = id),
    color = "#27A0CC", marker = FALSE,
    lineWidth = 3, showInLegend = FALSE,
  ) %>%
  hc_chart(type = "scatter") %>%  # Scatter plot for points
  hc_add_series(
    data = pop_projected,
    type = "scatter",
    hcaes(x = values, y = id, group = type, color = type),
    marker = list(symbols = "circle", radius = 4)
  ) %>% 
  hc_yAxis(title = list(text = "Geographic Area"),
           categories = unique(pop_projected$Geographic_Area),  # Ensure categorical y-axis
           reversed = TRUE) %>%  
  hc_legend(align = "center", verticalAlign = "bottom", layout = "horizontal") %>%
  hc_exporting(enabled = TRUE)  


highchart() %>% 
  hc_add_series(data = pop_projected, type  = "scatter",  
                hcaes(x = Geographic_Area, y = values, group = type, colour = type)) %>% 
  hc_xAxis(title = list(text = "Geographic Area"),
           categories = unique(pop_projected$Geographic_Area),  # Ensure categorical y-axis
           reversed = TRUE)


highchart() %>% 
  hc_add_series(data = pop_projected, type  = "scatter",  
                hcaes(x = Geographic_Area, y = projections, color = "#27A0CC")) %>% 
  hc_xAxis(title = list(text = "Geographic Area"),
           categories = unique(pop_projected$Geographic_Area),  # Ensure categorical y-axis
           reversed = TRUE)





library(highcharter)
library(dplyr)

# Define custom colors
custom_colors <- c("population" = "#27A0CC", "projection" = "#701F53")  

highchart() %>%
  # Add line chart (without legend)
  hc_add_series(
    data = pop_projected,
    type = "line",
    hcaes(x = values, y = id, group = id),
    color = "#27A0CC", 
    marker = list(enabled = FALSE),  # Use enabled instead of marker = FALSE
    lineWidth = 3, 
    showInLegend = FALSE
  ) %>%
  hc_add_series(
    data = pop_projected %>% filter(type == "population"), 
    type = "scatter",
    hcaes(x = values, y = id),
    name = "Population",
    color = "#8A005E",  # Manually assign color
    marker = list(symbol = "circle", radius = 4),
    showInLegend = TRUE
  ) %>%
  hc_add_series(
    data = pop_projected %>% filter(type == "projection"), 
    type = "scatter",
    hcaes(x = values, y = id),
    name = "Projection",
    color = "#B7EDE6",  # Manually assign color
    marker = list(symbol = "circle", radius = 4),
    showInLegend = TRUE
  ) %>%
  
  # Y-Axis (Categorical)
  hc_yAxis(
    title = list(text = "Geographic Area"),
    categories = unique(pop_projected$Geographic_Area),  
    reversed = TRUE
  ) %>%  
  
  # Legend positioning
  hc_legend(align = "center", verticalAlign = "bottom", layout = "horizontal") %>%
  
  # Enable export options
  hc_exporting(enabled = TRUE)

############
library(echarts4r)

pop_projected %>%
  e_charts(x = values) %>%
  e_line(serie = Geographic_Area) %>%
  e_scatter(serie = Geographic_Area, symbol_size = 8) %>%
  e_color(color = c("blue", "red")) %>%
  e_legend(orient = "horizontal", bottom = "0%") %>%
  e_theme("shine")


library(echart4r)

# Assuming pop_projected is your data frame
pop_projected %>%
  group_by(Geographic_Area) %>%  # Group by Geographic_Area
  e_charts(Geographic_Area, timeline = FALSE) %>%  # Initialize the chart with Geographic_Area on the y-axis
  e_line(values, bind = type) %>%  # Add lines for values, colored by type
  e_scatter(values, bind = type, symbol_size = 10) %>%  # Add points for values, colored by type
  e_color(c( "blue", "red")) %>%  # Specify custom colors for types
  e_x_axis(name = "Values") %>%  # Add x-axis label
  e_y_axis(name = "Geographic Area") %>%  # Add y-axis label
  e_tooltip(trigger = "item") %>%  # Enable tooltips
  e_legend(
    bottom = 0,  # Move legend to the bottom
    data = c("population", "projections")  # Show legend only for types
  ) %>%
  e_title("Population Projections by Geographic Area")  # Add a title


library(echart4r)
library(dplyr)

# Assuming pop_projected is your data frame
pop_projected %>%
  # group_by(Geographic_Area) %>%  # Group by type to create separate lines for each type
  e_charts(Geographic_Area, timeline = FALSE) %>%  # Initialize the chart with Geographic_Area on the y-axis
  e_line(values, name = "Population", color = "blue") %>%  # Add lines for population
  # e_line(values, name = "Projections", color = "red") %>%  # Add lines for projections
  e_scatter(population, 
            symbol_size = 10, color = "blue", name = "Population") %>%  # Add points for population
  e_scatter(projections, symbol_size = 10, color = "red", name = "Projections") %>%  # Add points for projections
  e_y_axis(name = "Values") %>%  # Add x-axis label
  e_x_axis(name = "Geographic Area") %>%  # Add y-axis label
  e_tooltip(trigger = "item") %>%  # Enable tooltips
  e_legend(bottom = 0, data = c("Population", "Projections")) %>%  # Show legend only for types
  e_title("Population Projections by Geographic Area")  # Add a title



library(echart4r)
library(dplyr)

# Assuming pop_projected is your data frame
pop_projectedd %>%
  # group_by(Geographic_Area) %>% 
  e_charts(Geographic_Area, timeline = FALSE) %>%  
  e_scatter(values, symbol_size = 10, bind = type) %>%  
  e_color(c("blue", "red")) %>%
  e_line() %>% 
  e_x_axis(name = "Values") %>%  
  e_y_axis(name = "Geographic Area") %>% 
  e_tooltip(trigger = "item") %>%  
  e_legend(bottom = 0, data = c("population", "projections")) %>%  
  e_title("Population Projections by Geographic Area") 



mtcars |>
  e_charts(drat) |>
  e_line(mpg) |>
  e_area(qsec) |>
  e_color(
    c("red", "blue"),
    "white"
  )






