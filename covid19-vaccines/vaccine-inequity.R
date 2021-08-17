library(tidyverse)

population <- readRDS("data/wb-countries-population-2019-2020-2021.rds") %>%
  select(
    iso_code = country_code,
    yr2021
  )

vaccines_by_incomegrp <- read_csv("https://github.com/owid/covid-19-data/raw/master/public/data/vaccinations/vaccinations.csv") %>%
  group_by(iso_code) %>%
  slice(which.max(date)) %>%
  filter(iso_code %in% c("OWID_LMC", "OWID_LIC", "OWID_UMC", "OWID_HIC")) %>%
  mutate(
    iso_code = str_remove(iso_code, "OWID_")
  ) %>%
  select(
    location,
    iso_code,
    people_vaccinated,
    people_fully_vaccinated
  ) %>%
  left_join(
    population,
    by = "iso_code"
  ) %>%
  ungroup() %>%
  mutate(
    pct_world = yr2021 / sum(yr2021),
    pct_vaccinated = people_vaccinated / yr2021,
    pct_fully_vaccinated = people_fully_vaccinated / yr2021,
    lbl = glue::glue("People: {scales::comma(yr2021)}<br><span style='color:grey50;'>Fraction of world population: {sprintf('%.2f%%', pct_world * 100)}</span><br><span style = 'color:darkblue;'>People vaccinated: {sprintf('%.2f%%', pct_vaccinated * 100)}</span><br><span style = 'color:darkgreen;'>People fully vaccinated: {sprintf('%.2f%%', pct_fully_vaccinated * 100)}</span>"),
    location = factor(
      location,
      levels = c("Low income", "Lower middle income",
                 "Upper middle income", "High income"),
      ordered = TRUE
    )
  )

p1 <- ggplot(
  vaccines_by_incomegrp,
  aes(y = location)
) +
  geom_col(aes(x = yr2021), fill = "grey50", color = "grey50") +
  geom_col(aes(x = people_vaccinated), fill = "darkblue", color = "darkblue") +
  geom_col(aes(x = people_fully_vaccinated), fill = "darkgreen", color = "darkgreen") +
  ggtext::geom_richtext(
    aes(x = yr2021, label = lbl),
    hjust = 0,
    nudge_x = 3e6,
    size = 7,
    label.size = 0) +
  scale_x_continuous(labels = scales::comma, expand = expansion(mult = c(0, .7))) +
  labs(
    x = "Population (2021)",
    y = "",
    title = "COVID-19 Vaccine Inequity: The most have received the least",
    subtitle = "Sources: OWID, World Bank",
    caption = glue::glue("@jmcastagnetto, Jesus M. Castagnetto ({Sys.Date()})")
  ) +
  hrbrthemes::theme_ipsum(20) +
  theme(
    axis.text.y = element_text(size = 20),
    axis.title.x = element_text(size = 20),
    plot.title.position = "plot",
    plot.title = element_text(size = 30),
    plot.subtitle = element_text(color = "grey70", size = 24),
    plot.caption = element_text(family = "Inconsolata", size = 18),
    plot.background = element_rect(fill = "white")
  )

ggsave(
  p1,
  filename = "covid19-vaccines/vaccine-inequity.png",
  width = 17,
  height = 12
)
