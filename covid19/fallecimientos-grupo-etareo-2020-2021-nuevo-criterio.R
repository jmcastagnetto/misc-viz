library(tidyverse)
library(lubridate)
library(ggforce)
library(ggtext)

csv_url <- "https://github.com/jmcastagnetto/covid-19-peru-limpiar-datos-minsa/raw/main/datos/fallecidos_covid-utf8-limpio.csv.xz"
fn <- tempfile()
download.file(csv_url, destfile = fn)
dths <- read_csv(fn) %>%
  mutate(
    yr = epiyear(fecha_fallecimiento),
    wk = epiweek(fecha_fallecimiento),
    mt = month(fecha_fallecimiento)
  )

por_edad <- dths %>%
  mutate(
    grupo = cut(
      edad,
      breaks = c(seq(0, 80, by = 10), 150),
      labels = c(
        "Edades: 0 - 9",
        "Edades: 10 - 19",
        "Edades: 20 - 29",
        "Edades: 30 - 39",
        "Edades: 40 - 49",
        "Edades: 50 - 59",
        "Edades: 60 - 69",
        "Edades: 70 - 79",
        "Edades: 80+"
      ),
      include.lowest = TRUE,
      right = FALSE
    )
  )


plot_df <- por_edad %>%
  group_by(yr, wk, grupo) %>%
  tally() %>%
  ungroup() %>%
  group_by(yr, grupo) %>%
  mutate(
    r_wk = wk - min(wk) + 1,
    n_ac = cumsum(n)
  ) %>%
  arrange(yr, grupo, r_wk)

max_date <- max(dths$fecha_fallecimiento, na.rm = TRUE)

extreme_vals <- plot_df %>%
  group_by(yr, grupo) %>%
  summarise(
    min_wk = min(wk),
    max_wk = max(wk),
    max_r_wk = max(r_wk),
    max_n_ac = max(n_ac)
  ) %>%
  mutate(
    lbl = glue::glue("Semana: {yr}-{max_wk}\nFallecidos {format(max_n_ac, big.mark = ',')}")
  )


p1 <- ggplot(
  plot_df %>% filter(!is.na(grupo)),
  aes(x = r_wk, y = n_ac,
      group = as.factor(yr),
      color = as.factor(yr)
  )
) +
  geom_line(size = 2) +
  geom_mark_circle(
    data = extreme_vals %>% filter(!is.na(grupo)),
    aes(x = max_r_wk, y = max_n_ac,
        label = lbl),
    show.legend = FALSE,
    con.cap = 0,
    label.fill = rgb(1, 1, 1, .6),
    expand = unit(1, "mm")
  ) +
  scale_y_continuous(labels = scales::comma) +
  scale_color_brewer(palette = "Dark2", type = "qual") +
  facet_wrap(~grupo, scales = "free_y") +
  labs(
    x = "Semanas desde el inicio del registro (para cada año)",
    y = "",
    color = "Año Epidemiológico",
    title = "Fallecimientos acumulados por COVID-19 en Perú",
    subtitle = glue::glue("Fuente: MINSA al {max_date} (https://www.datosabiertos.gob.pe/dataset/fallecidos-por-covid-19-ministerio-de-salud-minsa)"),
    caption = glue::glue("@jmcastagnetto, Jesus M. Castagnetto ({Sys.Date()})")
  ) +
  theme_linedraw(16) +
  theme(
    plot.background = element_rect(fill = "white"),
    plot.title.position = "plot",
    plot.title = element_text(size = 32),
    plot.subtitle = element_text(size = 12, color = "gray40"),
    plot.caption = element_text(family = "Inconsolata", size = 14),
    strip.text = element_text(face = "bold"),
    legend.position = "top",
    legend.text = element_text(size = 20),
    legend.title = element_text(size = 14)
  )

ggsave(
  plot = p1,
  filename = "covid19/fallecimientos-acumulados-2020-2021-rangos-edad.png",
  width = 16,
  height = 12
)
