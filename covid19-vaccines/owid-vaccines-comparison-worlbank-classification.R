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

gdp <- wbstats::wb_data("NY.GDP.PCAP.PP.CD", start_date = 2019, end_date = 2019)

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
    income_group = factor(
        income_group,
        levels = c(
          "Low income",
          "Lower middle income",
          "Upper middle income",
          "High income"
        ),
        ordered = TRUE
      )
  ) %>%
  left_join(
    gdp %>%
      select(iso3c, gdp2019 = 5),
    by = c("iso_code" = "iso3c")
  ) %>%
  arrange(location, date) %>%
  group_by(location) %>%
  summarise_all(
    last
  )

income_group_lbls <- vaccinations_country_latest %>%
  ungroup() %>%
  filter(!is.na(people_vaccinated_per_hundred) &
           !is.na(income_group)) %>%
  mutate(
    pos = as.numeric(income_group)
  ) %>%
  group_by(income_group) %>%
  summarise(
    lbl = paste0(income_group, "\n(", n(), " countries)") %>%
      fct_reorder(pos)
  ) %>%
  distinct()

vaccinations_country_latest <- vaccinations_country_latest %>%
  left_join(
    income_group_lbls,
    by = "income_group"
  )

txt_note <- data.frame(
  y = 3.5,
  x = 30,
  txt = "To date, the **COVID-19** vaccination gap between countries is noticeable.<br/><br/>The richest countries have been able to vaccinate a bigger fraction of their populations.<br/><br/>The poorest are behind by a lot, and not catching up soon.<br/><br/>This reflects the wholesale purchase of the available and future vaccine stock by richer countries."
)


p1 <- ggplot(
  vaccinations_country_latest %>%
    filter(!is.na(people_vaccinated_per_hundred) & !is.na(income_group)),
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

# p1
ggsave(
  plot = p1,
  filename = "covid19-vaccines/asymmetric-vaccine-extent-covid19-owid.png",
  width = 20,
  height = 12
)

txt_note2 <- data.frame(
  y = .75,
  x = 500,
  txt = "The **COVID-19** vaccination campaings in countries is noticeable different.<br/><br/>The richest countries have been able to vaccinate a bigger fraction of their populations.<br/><br/>The poorest are behind by a lot, and not catching up soon.<br/><br/>This reflects the wholesale purchase of the available and future vaccine stock by richer countries.<br/><br/>To date, two countries buckle the trend: *Buthan* and *Maldives*"
)

p2 <- ggplot(
  vaccinations_country_latest %>%
    filter(!is.na(income_group) &
             !is.na(people_vaccinated_per_hundred) &
             !is.na(gdp2019)),
  aes(x = gdp2019, y = people_vaccinated_per_hundred / 100)
) +
  geom_point(aes(color = income_group,
                 shape = income_group),
             show.legend = FALSE,
             size = 5) +
  ggforce::geom_mark_hull(
    aes(group = income_group,
        color = income_group,
        label = lbl),
    label.colour = "grey40",
    con.type = "straight",
    con.colour = "grey40",
    con.arrow = arrow(length = unit(4, "mm"), type = "closed"),
    #con.linetype = "dashed",
    con.cap = 1,
    linetype = "dashed",
    concavity = .5,
    show.legend = FALSE
  ) +
  ggforce::geom_mark_circle(
    data = vaccinations_country_latest %>%
      filter(iso_code == "BTN" | iso_code == "MDV"),
    aes(group = location,
        label = location,
        description = glue::glue("{people_vaccinated_per_hundred}%")),
    con.type = "straight",
    con.arrow = arrow(length = unit(2, "mm"))
  ) +
  ggtext::geom_textbox(
    data = txt_note2,
    aes(x = x, y = y, label = txt),
    hjust = 0,
    vjust = 1,
    inherit.aes = FALSE,
    width = .27,
    box.size = 0,
    size = 5.4,
    fill = NA,
    #fill = rgb(1, 1, 1, .5),
    family = "GNUTypewriter"
  ) +
  scale_shape_manual(
    values = c("circle", "triangle", "square", "diamond")
  ) +
  scale_color_brewer(palette = "Set1", type = "qual") +
  scale_x_log10(labels = scales::comma) +
  scale_y_continuous(labels = scales::percent) +
  annotation_logticks(sides = "b") +
  labs(
    x = "GDP per capita in current USD",
    y = "Percentage of population vaccinated",
    title = "Comparing the extent of the COVID-19 vaccination and GDP per capita",
    subtitle = "A demonstration of the unequal distribution of COVID-19 vaccines",
    caption = glue::glue("@jmcastagnetto, Jesus M. Castagnetto, Updated on {today}\nSources: OWID vaccine data (https://github.com/owid/covid-19-data/), World Bank | See also: https://launchandscalefaster.org/COVID-19")
  ) +
  ggthemes::theme_few(18, base_family = "GnuTypewriter") +
  theme(
    plot.title.position = "plot",
    plot.title = element_text(size = 25),
    plot.subtitle = element_text(size = 22, color = "red"),
    plot.caption = element_text(family = "Inconsolata")
  )

ggsave(
  plot = p2,
  filename = "covid19-vaccines/covid19-vaccination-vs-gdp.png",
  width = 15,
  height = 10
)
