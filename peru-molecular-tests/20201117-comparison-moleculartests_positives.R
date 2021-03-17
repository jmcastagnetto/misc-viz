library(tidyverse)
library(ggtext)

# Mandatory lockdown: 2020-03-16 - 2020-06-30
ini_lkdn <- lubridate::ymd("2020-03-16")
end_lkdn <- lubridate::ymd("2020-06-30")

load("data/datos_abiertos_minsa_covid-19_peru.Rdata")
rm(reconstruido)

pm <- readRDS("data/pm_covid19_ins_peru.rds")
pm_epiweek <- pm %>%
  mutate(
    epiweek = lubridate::epiweek(fechatomamuestra),
    wed_of_wk = lubridate::floor_date(fechatomamuestra,
                                      unit = "week") + 3 # Wed of each week
  ) %>%
  group_by(epiweek, wed_of_wk) %>%
  summarise(
    pos_tests = sum(resultado == "POSITIVO"),
    neg_tests = sum(resultado == "NEGATIVO"),
    tot_tests = pos_tests + neg_tests
  )

cases_epiweek <- casos %>%
  mutate(
    epiweek = lubridate::epiweek(fecha_resultado),
    wed_of_wk = lubridate::floor_date(fecha_resultado,
                                      unit = "week") + 3 # Wed of each week
  ) %>%
  group_by(epiweek, wed_of_wk) %>%
  tally(name = "n_cases")

deaths_epiweek <- fallecimientos %>%
  mutate(
    epiweek = lubridate::epiweek(fecha_fallecimiento),
    wed_of_wk = lubridate::floor_date(fecha_fallecimiento,
                                      unit = "week") + 3 # Wed of each week
  ) %>%
  group_by(epiweek, wed_of_wk) %>%
  tally(name = "n_deaths")

combined <- pm_epiweek %>%
  full_join(
    cases_epiweek,
    by = c("epiweek", "wed_of_wk")
  ) %>%
  full_join(
    deaths_epiweek,
    by = c("epiweek", "wed_of_wk")
  ) %>%
  ungroup() %>%
  slice(1:(n() - 1)) # remove the last epiweek, usually incomplete

plot_caption <-

p_base <- ggplot(
  combined,
  aes(x = wed_of_wk)
) +
  annotate(
    geom = "rect",
    xmin = ini_lkdn,
    xmax = end_lkdn,
    ymin = 0,
    ymax = max(combined$n_cases, na.rm = TRUE),
    fill = "cyan",
    alpha = .3
  ) +
  annotate(
    geom = "text",
    x = ini_lkdn + (end_lkdn - ini_lkdn) / 2,
    y = .9 * max(combined$n_cases, na.rm = TRUE),
    label = "Mandatory\nLockdown",
    family = "Inconsolata Bold",
    size = 8,
    color = "black"
  ) +
  geom_point(aes(y = tot_tests), color = "red", alpha = .5) +
  geom_point(aes(y = n_cases), color = "black", alpha = .5) +
  geom_point(aes(y = (n_deaths*20)), color = "blue", alpha = .5) +
  scale_y_continuous(
    labels = scales::comma,
    limits = c(0, NA),
    sec.axis = sec_axis(
      trans = ~. / 20,
      name = "Deaths",
      labels = scales::comma
    )
  ) +
  scale_x_date(
    date_breaks = "4 weeks",
    date_labels = "Wk: %V\n%b %d",
    date_minor_breaks = "1 week"
  ) +
  labs(
    x = "",
    y = "<span style='color:black'>Positive cases</span> & <span style='color:red'>Molecular tests</span>",
    title = "COVID-19 in Perú: Weekly number of positive cases, molecular tests and deaths",
    caption = "**Sources**: <span style='color:blue;'>Deaths: bit.ly/covid19minsafallecidos</span>, <span style='color:black'>Positive cases: bit.ly/covid19inspruebmolec</span>, <span style='color:red'>Molecular tests: bit.ly/covid19minsapositivos</span><br/>@jmcastagnetto, Jesus M. Castagnetto"
  ) +
  theme_minimal(18) +
  theme(
    axis.title.y.left = element_markdown(),
    axis.text.y.right = element_text(color = "blue"),
    axis.title.y.right = element_text(color = "blue"),
    plot.subtitle = element_markdown(),
    plot.caption = element_markdown(family = "Inconsolata")
  )

sub1 <- paste0("Using open data on <span style='color:blue;'>Deaths</span>, <span style='color:black'>Positive cases</span>, and <span style='color:red'>Molecular tests</span>. Weeks: [",
                  min(combined$epiweek),
                  " - ", max(combined$epiweek),
                  "].")
sub2 <- paste0("Using open data on <span style='color:blue;'>Deaths</span>, <span style='color:black'>Positive cases</span>, and <span style='color:red'>Molecular tests</span>. Weeks: [",
               min(combined$epiweek),
               " - ", max(combined$epiweek),
               "]. (GAM smoothing added)")

p1 <- p_base +
  geom_line(aes(y = tot_tests), color = "red") +
  geom_line(aes(y = n_cases), color = "black") +
  geom_line(aes(y = (n_deaths*20)), color = "blue") +
  labs(
    subtitle = sub1
  )

p2 <- p_base +
  geom_smooth(aes(y = tot_tests),
            method = "gam",
            formula = y ~ s(x, bs = "cs"),
            linetype = "dashed",
            color = "red",
            se = TRUE) +
  geom_smooth(aes(y = n_cases),
              method = "gam",
              formula = y ~ s(x, bs = "cs"),
              color = "black",
              linetype = "dashed",
              se = TRUE) +
  geom_smooth(aes(y = (n_deaths*20)),
              method = "gam",
              formula = y ~ s(x, bs = "cs"),
              linetype = "dashed",
              color = "blue",
              se = TRUE) +
  labs(
    subtitle = sub2
  )

ggsave(
  plot = p1,
  filename = "plots/20201117-comparison-moleculartests-positives-lines.png",
  width = 14,
  height = 9
)

ggsave(
  plot = p2,
  filename = "plots/20201117-comparison-moleculartests-positives-gam.png",
  width = 14,
  height = 9
)
