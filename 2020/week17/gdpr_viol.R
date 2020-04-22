
# Load  packages
packages <- c(
  "tidyverse",
  "lubridate",
  "gridExtra",
  "ggimage",
  "rsvg"
  )
for (pkg in packages) library(pkg, character.only = TRUE)

# Import data
gdpr_violations <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-21/gdpr_violations.tsv')

# Parse date
gdpr_violations <- gdpr_violations %>% 
  mutate(date = readr::parse_datetime(date, format = "%m/%d/%Y"))

# Summary between 2018 and 2020
gdpr_by_country <- gdpr_violations %>% 
  filter(year(date) >= 2018) %>% 
  group_by(name, picture) %>% 
  summarise(price = sum(price, na.rm = TRUE),
            n = n()) %>% 
  ungroup()

# Plot
plot1 <- gdpr_by_country %>% 
  ggplot() +
  geom_image(aes(price, n, image = picture), size = 0.07) +
  ggtitle("Total number of GDPR violations by country (2018-2020)") +
  labs(x = "Price (\u20AC)", y = "Number of GDPR violations") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major.x = element_blank())
# Save
ggsave("gdpr.png", plot1, width = 6, height = 6, unit = "in", dpi = 300)
