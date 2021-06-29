library(tidyverse)
library(lubridate)
library(hrbrthemes)
library(patchwork)

# casos y fallecimientos --------------------------------------------------
csv_file1 <- "https://github.com/jmcastagnetto/covid-19-peru-data/raw/main/datos/covid-19-peru-data.csv"

peru <- read_csv(
  csv_file1,
  col_types = cols(
    .default = col_integer(),
    country = col_character(),
    iso3c = col_character(),
    region = col_character(),
    date = col_date(format = "")
  )
) %>%
  filter(is.na(region)) %>%
  select(date, confirmed, deaths) %>%
  mutate(
    cases = confirmed - lag(confirmed),
    dths = deaths - lag(deaths),
    yr = as.character(year(date))
  )

# hospitalizados y ventilación mecánica -----------------------------------

csv_file2 <- "https://github.com/jmcastagnetto/covid-19-peru-data/raw/main/datos/covid-19-peru-detalle-hospitalizados.csv"

hosp <- read_csv(
  csv_file2,
  col_types = cols(
    .default = col_integer(),
    fecha = col_date(format = "")
  )
) %>%
  select(fecha, hospitalizados, ventilacion_mecanica) %>%
  mutate(
    yr = as.character(year(fecha))
  )


# Plots -------------------------------------------------------------------

pe <- Sys.setlocale("LC_TIME", "es_PE.utf8")
caption_str <- glue::glue("Fuente: MINSA // Curvas aproximadas usando GAM // @jmcastagnetto, Jesus M. Castagnetto ({Sys.Date()})")

mk_plot <- function(df,
                    title_str,
                    gam_formula,
                    ylims = NULL) {
  tmp <- ggplot(
    df,
    aes(x = x, y = y, color = grp)
  ) +
    geom_point(size = .5,
               show.legend = FALSE) +
    geom_smooth(method = "gam",
                formula = gam_formula,
                size = 1,
                alpha = .7,
                show.legend = FALSE) +
    geom_vline(xintercept = ymd("2021-01-01"),
               color = "gray40",
               linetype = "dashed")
  if (!is.null(ylims)) {
    tmp <- tmp +
      scale_y_continuous(labels = scales::comma, limits = ylims)
  } else {
    tmp <- tmp +
      scale_y_continuous(labels = scales::comma)
  }
  tmp +
    scale_x_date(date_breaks = "2 months", date_labels = "%b\n%Y") +
    labs(
      x = "",
      y = "",
      title = title_str
    ) +
    theme_ipsum_rc(
      plot_title_size = 16,
      base_size = 14
    ) +
    theme(
      plot.title = element_text(color = "grey40"),
      plot.title.position = "plot"
    )
}

gam_frm1 <- y ~ s(x, bs = "tp", k = 12)

p1 <- mk_plot(
  peru %>% select(x = date, y = cases, grp = yr),
  "Casos positivos reportados",
  gam_frm1,
  c(0, 15000)
)

p2 <- mk_plot(
  peru %>% select(x = date, y = dths, grp = yr),
  "Fallecimientos reportados",
  gam_frm1,
  c(0, 500)
)

gam_frm2 <- y ~ s(x, bs = "tp", k = 10)

p3 <- mk_plot(
  hosp %>% select(x = fecha, y = hospitalizados, grp = yr),
  "Personas hospitalizadas",
  gam_frm2
)

p4 <- mk_plot(
  hosp %>% select(x = fecha, y = ventilacion_mecanica, grp = yr),
  "Personas con ventilación mecánica",
  gam_frm2,
  c(0, 3000)
)

pcomb <- (p1 / p2) | (p3 / p4)

pfinal <- pcomb +
  plot_annotation(
    title = "Evolución de COVID-19 en el Perú desde el 2020",
    caption = caption_str
  ) &
  theme(
    plot.title = element_text(family = "Roboto Condensed", face = "bold", size = 32),
    plot.caption = element_text(family = "Inconsolata", size = 16)
  )

ggsave(
  pfinal,
  filename = "covid19/20210628-covid19-peru-casos-fallecimientos-hosp-uci.png",
  width = 14,
  height = 9
)

