
# Load packages
packages <- c("tidyverse", "USAboundaries", "sf")
for (pkg in packages) library(pkg, character.only = TRUE)

# Load the beer_states dataset from the tidytuesday repository
beer_states <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-31/beer_states.csv')

# Get total production and percentage of beer per state, year, and type
beer_states_summary <- beer_states %>% 
  filter(state != "total") %>% 
  count(state, year, type, name = "production", wt = barrels) %>%
  group_by(year, type) %>% 
  mutate(percentage = production/sum(production, na.rm = TRUE)*100)

# Get the US boundaries data from the USAboundaries package
# (Only the states included in the beer_states dataset)
states <- beer_states$state %>%
  unique() %>%
  str_subset("[^total]") 
state_boundaries <- us_states(states = states)

# Combine the beer production information and US boundaries
my_data <- left_join(beer_states_summary, state_boundaries, by = c("state" = "state_abbr"))

# Create map (year 2019) with geom_sf()
my_map <- my_data %>% 
  filter(year == 2019) %>% 
  ggplot() +
  geom_sf(aes(fill = percentage, geometry = geometry)) +
  geom_sf_label(aes(label = state,  geometry = geometry),
                label.size = 0, size = 2, alpha = 0.6,
                label.padding = unit(0.1, "lines")) +
  coord_sf(xlim = c(-180, -65), expand = FALSE) +
  facet_wrap(~type, ncol = 2) +
  ggtitle("Total US Beer Production/Use in 2019") +
  scale_fill_viridis_c() +
  theme(axis.title = element_blank())

# Export chart
ggsave("US_beer_production_2019.png", 
       height = 7.7, width = 13.7, dpi = 300)

