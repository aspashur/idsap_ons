## import data
pop_est = read.csv('../../pop_release/data/GHA_pop_estimates_2022_2024.csv')
relocations = read.csv('../../pop_release/data/GHA_reloc_estimates_2020_2024.csv') %>% 
  mutate(date = as.Date(date))

pop_projection = read.csv('data/Population Projections_20250214-151840.csv')

current_district = 'Achiase'
current_month = as.Date("2024-12-01")
pop_est %>% 
  filter(district == current_district) %>% 
  slice_head(n =1) %>% 
  pull(region) -> current_region

pop_est %>% 
  filter(district == current_district) %>% 
  slice_head(n = 1) %>% 
  pull(dist_code) -> code

geo_data = st_read('data/gss_phc2021_bnd_edgematched_20240821151831.gpkg',
                   layer = "gss_phc2021_16_regions", quiet = TRUE) %>% 
  st_make_valid() %>%  
  select(!region) %>%
  rename(region = region_name)