library(tidyverse)
library(clock)
load("~/devel/github/covid-19-peru-limpiar-datos-minsa/datos/datos_abiertos_minsa_covid-19_peru.Rdata")
rm(list = c("casos", "reconstruido"))

por_edad <- fallecimientos %>%
  mutate(
    rango_edad = cut(edad,
                     c(seq(0, 80, 20), 130),
                     include.lowest = TRUE,
                     right = FALSE,
                     labels = c(
                       "0-19",
                       "20-39",
                       "40-59",
                       "60-79",
                       "80+"
                     )
    ),
    epi_year = lubridate::epiyear(fecha_fallecimiento),
    epi_week = lubridate::epiweek(fecha_fallecimiento),
    monday = iso_year_week_day(epi_year, epi_week, 1) %>%
      as_date()
  )

peru_por_edad <- por_edad %>%
  group_by(monday, rango_edad, sexo) %>%
  tally() %>%
  filter(!is.na(rango_edad) & sexo %in% c("Femenino", "Masculino"))

fecha_corte <- unique(fallecimientos$fecha_corte)

p1 <- ggplot(
  peru_por_edad,
  aes(x = monday, y = n, group = sexo, color = sexo)
) +
  geom_vline(xintercept = as.Date("2020-12-31"), color = "grey50", linetype = "dashed") +
  geom_point() +
  geom_smooth(method = "gam") +
  scale_y_continuous(labels = scales::label_comma(accuracy = 1), limits = c(0, NA)) +
  scale_x_date(date_breaks = "2 month", date_labels = "%b\n%Y") +
  facet_wrap(~rango_edad, scales = "free_y") +
  theme_linedraw(base_size = 18) +
  theme(
    legend.position = c(.8, .2),
    axis.text.x = element_text(size = 12),
    plot.subtitle = element_text(color = "grey50"),
    plot.caption = element_text(family = "Inconsolata")
  ) +
  labs(
    title = "COVID-19 (Perú): Fallecidos confirmados por semana, grupo etáreo y sexo",
    subtitle = glue::glue("Fuente: Datos abiertos del MINSA al {fecha_corte}. Curvas aproximadas usando GAM: y ~ s(x, bs = 'cs')"),
    caption = glue::glue("@jmcastagnetto, Jesus M. Castagnetto ({Sys.Date()})"),
    color = "Sexo",
    x = "",
    y = ""
  )

ggsave(
  p1,
  file = "covid19/fallecimientos-2020-2021-semanales-rango-edad-sexo.png",
  width = 18,
  height = 12
)
