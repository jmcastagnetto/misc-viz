library(tidyverse)
library(lemon)
library(glue)
library(ggthemes)

vacunas <- readRDS(url("https://github.com/jmcastagnetto/covid-19-peru-vacunas/raw/main/datos/vacunas_covid_rangoedad_owid.rds"))

plot_df <- vacunas %>%
  filter(last_day_of_epi_week == max(last_day_of_epi_week)) %>%
  mutate(
    age_group_min = as.integer(age_group_min),
    age_group_max = as.integer(age_group_max),
    age_range = if_else(
      !is.na(age_group_max),
      glue("{age_group_min}-{age_group_max}"),
      glue("{age_group_min}+")
    )
  ) %>%
  select(
    age_range,
    people_vaccinated_per_hundred,
    people_fully_vaccinated_per_hundred,
    people_recieving_booster_per_hundred
  ) %>%
  pivot_longer(
    cols = starts_with("people_"),
    names_to = "vaccination_status",
    names_prefix = "people_",
    values_to = "pct"
  ) %>%
  mutate(
    vaccination_status = str_replace_all(
      vaccination_status,
      c(
        "^vaccinated_per_hundred$" = "At least 1 dose",
        "^fully_vaccinated_per_hundred$" = "Fully vaccinated (2 doses)",
        "recieving_booster_per_hundred" = "Recieved booster dose"
      )
    ),
    vaccination_status = factor(
      vaccination_status,
      levels = c(
        "At least 1 dose",
        "Fully vaccinated (2 doses)",
        "Recieved booster dose"
      ),
      ordered = TRUE
    ),
    pct = pct / 100
  )

pvac <- ggplot(
  plot_df,
  aes(y = age_range, x = pct,
      fill = vaccination_status,
      group = vaccination_status)
) +
  geom_col(position = position_dodge(.9), width = .85) +
  geom_text(
    aes(label = sprintf("%.1f%%", pct * 100)),
    position = position_dodge(width = .9),
    hjust = -0.1,
    size = 4
  ) +
  scale_x_continuous(
    labels = scales::percent
  ) +
  scale_fill_brewer(
    palette = "Paired",
    guide = guide_legend(reverse = TRUE)
  ) +
  coord_flex_cart(
    left = brackets_vertical(direction = "right"),
    bottom = capped_horizontal()
  ) +
  ggthemes::theme_tufte(base_size = 20) +
  theme(
    panel.border=element_blank(),
    axis.line = element_line(),
    legend.position = c(.8, .08),
    legend.title = element_text(face = "bold"),
    legend.background = element_rect(color = "grey20", size = .2),
    legend.margin = margin(2, 2, 2, 2),
    legend.key.height = unit(1.5, "line"),
    plot.title.position = "plot",
    plot.title = element_text(size = 24),
    plot.subtitle = element_text(size = 18, color = "grey50"),
    plot.caption = element_text(size = 14, family = "Inconsolata"),
    plot.background = element_rect(fill = "white")
  ) +
  labs(
    x = "",
    y = "",
    fill = "Vaccination Status: ",
    title = "COVID-19 Vaccination in Peru: Percentage of the population covered by age group",
    subtitle = "Data Source: MINSA Open Data (Epidemiological week ending on 2022-03-26)",
    caption = "@jmcastagnetto, Jesus M. Castagnetto"
  )
pvac
ggsave(
  pvac,
  filename = "covid19/20220328-vaccine-coverage-age-group-peru.png",
  height = 12,
  width = 15
)
