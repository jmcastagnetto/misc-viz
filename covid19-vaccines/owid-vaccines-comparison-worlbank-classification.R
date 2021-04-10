library(tidyverse)

today <- Sys.Date()

# vaccination data
owid_url <- "https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/vaccinations.csv"

# current country classifications
wb_url <- "http://databank.worldbank.org/data/download/site-content/CLASS.xls"

vaccinations <- rio::import(owid_url)

wb_classification <- rio::import(wb_url,
                                 skip = 3,
                                 na = c("", "NA", "..")) %>%
  select(-1, -2, -5) %>%
  filter(!is.na(Economy) & Economy != "x") %>%
  janitor::clean_names()

vaccinations_continent <- vaccinations %>%
  filter(str_starts(iso_code, "OWID"))

vaccinations_country_latest <- vaccinations %>%
  filter(str_starts(iso_code, pattern = "OWID", negate = TRUE)) %>%
  left_join(
    wb_classification %>%
      select(code, income_group),
    by = c("iso_code" = "code")
  ) %>%
  mutate(
    income_group = replace_na(income_group, "Unclassified") %>%
      factor(
        levels = c(
          "Unclassified",
          "Low income",
          "Lower middle income",
          "Upper middle income",
          "High income"
        ),
        ordered = TRUE
      )
  ) %>%
  arrange(location, date) %>%
  group_by(location) %>%
  summarise_all(
    last
  )

income_group_lbls <- vaccinations_country_latest %>%
  filter(!is.na(people_vaccinated_per_hundred)) %>%
  mutate(
    pos = as.numeric(income_group)
  ) %>%
  group_by(income_group) %>%
  summarise(
    lbl = paste0(income_group, "\n(", n(), " countries)") %>%
      fct_reorder(pos)
  )

txt_note <- data.frame(
  y = 4.5,
  x = 30,
  txt = "To date, the **COVID-19** vaccination gap between countries is noticeable.<br/><br/>The richest countries have been able to vaccinate a bigger fraction of their populations.<br/><br/>The poorest are behind by a lot, and not catching up soon.<br/><br/>This reflects the wholesale purchase of the available and future vaccine stock by richer countries."
)


p1 <- ggplot(
  vaccinations_country_latest %>%
    filter(!is.na(people_vaccinated_per_hundred)) %>%
    left_join(
      income_group_lbls,
      by = "income_group"
    ),
  aes(x = people_vaccinated_per_hundred,
      y = lbl,
      group = lbl,
      fill = lbl
  )
) +
  ggridges::stat_density_ridges(
    quantile_lines = TRUE,
    quantiles = c(.5),
    jittered_points = TRUE,
    point_shape = "|",
    point_size = 3,
    alpha = .7,
    scale = 1.2
  ) +
  ggtext::geom_textbox(
    data = txt_note,
    aes(x = x, y = y, label = txt),
    hjust = 0,
    vjust = 1,
    inherit.aes = FALSE,
    width = .6,
    box.size = 0,
    size = 9,
    fill = rgb(1, 1, 1, .5)
  ) +
  scale_fill_brewer(
    palette = "YlOrRd",
    type = "seq"
  ) +
  hrbrthemes::theme_ipsum_rc(
    grid = FALSE,
    plot_title_size = 32,
    axis_text_size = 22,
    axis_title_size = 24,
    axis_title_face = "italic",
    caption_family = "Inconsolata",
    caption_size = 16
  ) +
  theme(
    plot.title = element_text(hjust = .5),
    plot.subtitle = element_text(size = 28, color = "red", hjust = .5),
    legend.position = "none"
  ) +
  labs(
    x = "People vaccinated per hundred (one dosis)",
    y = "World Bank Income Classification",
    title = "The extend of the asymmetry in COVID-19 vaccination campaigns",
    subtitle = "An example of inequity in vaccine distribution",
    caption = glue::glue("@jmcastagnetto, Jesus M. Castagnetto, Updated on {today}\nSource: OWID vaccine data, https://github.com/owid/covid-19-data/ | See also: https://launchandscalefaster.org/COVID-19")
  )

#p1
ggsave(
  plot = p1,
  filename = "covid19-vaccines/asymmetric-vaccine-extent-covid19-owid.png",
  width = 20,
  height = 12
)

