library(tidyverse)
library(tidycensus)
library(tigris)
library(sf)
library(viridis)
options(tigris_use_cache = TRUE)
census_api_key("a16f3636406d2b544871e2ae49bb318c3ddcacba")

theme_set(theme_minimal())
theme_minimal()



allegheny <- get_acs(state = "PA", 
                     county = "Allegheny County", 
                     geography = "tract", 
                     variables = c(median_income = "B19013_001"), 
                     geometry = TRUE,
                     cb = FALSE)

head(allegheny)

st_erase <- function(x, y) {
  st_difference(x, st_union(st_combine(y)))
}

allegheny_water <- area_water("PA", "Allegheny", class = "sf")

allegheny_erase <- st_erase(allegheny, allegheny_water)

allegheny_erase %>%
  ggplot(aes(fill = estimate, color = estimate)) + 
  geom_sf() + 
  coord_sf(crs = 26911) + 
  scale_fill_viridis("Median household income", option = "magma") + 
  scale_color_viridis("Median household income", option = "magma") +
  labs(title = "Allegheny County",
       subtitle = "American Community Survey") +
  theme(axis.text = element_blank())

racevars <- c(White = "P0050003", 
              Black = "P0050004", 
              Asian = "P0050006", 
              Hispanic = "P0040003")

allegheny_race <- get_decennial(geography = "tract", 
                                variables = racevars,
                                state = "PA", 
                                county = "Allegheny", 
                                geometry = TRUE,
                                summary_var = "P0010001") 

get_decennial(geography = "county", 
              variables = racevars,
              state = "PA", 
              county = "Allegheny", 
              geometry = TRUE,
              summary_var = "P0010001") %>% 
  mutate(value = value / summary_value)

head(allegheny_race)

allegheny_race %>%
  mutate(pct = 100 * (value / summary_value)) %>%
  ggplot(aes(fill = pct, color = pct)) +
  facet_wrap(~variable) +
  geom_sf() +
  coord_sf(crs = 26915) + 
  scale_fill_viridis() +
  scale_color_viridis()

v15 <- load_variables(2016, "acs5", cache = TRUE)

View(v15)


vars <- load_variables(2016, "acs5", cache = TRUE)

pa <- get_acs(state = "PA",
                     geography = "tract", 
                     variables = "B19013_001", 
                     geometry = TRUE)

head(pa)

pa %>%
  ggplot(aes(fill = estimate, color = estimate)) + 
  geom_sf() + 
  #coord_sf(crs = 26911) + 
  scale_fill_viridis(option = "magma") + 
  scale_color_viridis(option = "magma")

racevars <- c("P0050003", 
              "P0050004", 
              "P0050006", 
              "P0040003")

pa_race <- get_decennial(geography = "tract", 
                                variables = racevars,
                                state = "PA", 
                                geometry = TRUE,
                                summary_var = "P0010001") 

head(pa_race)

pa_race %>%
  mutate(pct = 100 * (value / summary_value)) %>%
  ggplot(aes(fill = pct, color = pct)) +
  facet_wrap(~variable) +
  geom_sf() +
  coord_sf(crs = 26915) + 
  scale_fill_viridis() +
  scale_color_viridis()



