library(ggplot2)
library(ggalt)
library(dplyr)
library(tidyr)
library(lubridate)
library(highcharter)
library(echarts4r)
library(sf)
library(leaflet)
library(leaflet.extras)
library(mapboxapi)
library(plotly)


# set constants
fm_basemap <- "mapbox://styles/flowstef/ckv0sw2yz241714qkbkph05ps"
fm_token <- "pk.eyJ1IjoiZmxvd3N0ZWYiLCJhIjoiY2wzaWlmczU0MHRkYzNkcXd6Z3ozZjc4eSJ9.fk_otUi6egM87vth7oeScg"


# this will make the labels of the bar chart a bit nicer, by ending above the highest data point
nicelimits <- function(x) {
  range(scales::extended_breaks(only.loose = TRUE)(x))
}

