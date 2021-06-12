library(tidyverse)
library(lubridate)
library(ggforce)
library(ggtext)

csv_url <- "https://github.com/jmcastagnetto/covid-19-peru-limpiar-datos-minsa/raw/main/datos/fallecidos_covid-utf8-limpio.csv.xz"
fn <- tempfile()
download.file(csv_url, destfile = fn)
dths <- read_delim(fn, delim = "\t") %>%
  mutate(
    yr = epiyear(fecha_fallecimiento),
    wk = epiweek(fecha_fallecimiento),
    mt = month(fecha_fallecimiento)
  )

plot_df <- dths %>%
  group_by(yr, wk) %>%
  tally() %>%
  ungroup() %>%
  group_by(yr) %>%
  mutate(
    r_wk = wk - min(wk) + 1,
    n_ac = cumsum(n)
  )

extreme_vals <- plot_df %>%
  group_by(yr) %>%
  summarise(
    min_wk = min(wk),
    max_wk = max(wk),
    max_r_wk = max(r_wk),
    max_n_ac = max(n_ac)
  ) %>%
  mutate(
    lbl = glue::glue("Semana: {yr}-{max_wk}\nFallecidos {format(max_n_ac, big.mark = ',')}")
  )

annotation_df <- tibble(
  x = 22,
  y = 5e4,
  txt_annotation = glue::glue("En el 2020, entre las semanas {extreme_vals$min_wk[1]} y {extreme_vals$max_wk[1]} se tuvieron {format(extreme_vals$max_n_ac[1], big.mark=',')} fallecidos por COVID-19 a nivel nacional. Entre las semanas {extreme_vals$min_wk[2]} y {extreme_vals$max_wk[2]}, del 2021, ya **hemos acumulado {format(extreme_vals$max_n_ac[2], big.mark=',')} fallecidos por la misma causa**. *No podemos dejar de cuidarnos, pues aún nos queda mucho que recorrer antes que esto termine*.")
)


p1 <- ggplot(
  plot_df,
  aes(x = r_wk, y = n_ac,
      group = as.factor(yr),
      color = as.factor(yr)
  )
) +
  geom_line(size = 2) +
  geom_mark_circle(
    data = extreme_vals,
    aes(x = max_r_wk, y = max_n_ac,
        label = lbl),
    show.legend = FALSE
  ) +
  geom_textbox(
    data = annotation_df,
    aes(x = x, y = y, group = 1, label = txt_annotation),
    color = "black",
    fill = "gray90",
    box.size = 0,
    width = unit(.45, "npc"),
    hjust = 0,
    vjust = 1,
    size = 6,
    inherit.aes = FALSE
  ) +
  scale_y_continuous(labels = scales::comma, limits = c(0, 1e5)) +
  scale_color_brewer(palette = "Dark2", type = "qual") +
  labs(
    x = "Semanas desde el inicio del registro (para cada año)",
    y = "",
    color = "Año Epidemiológico",
    title = "Fallecimientos acumulados por COVID-19 en Perú",
    subtitle = "Usando los datos de MINSA con el nuevo criterio de clasificación",
    caption = "@jmcastagnetto, Jesus M. Castagnetto (2021-06-12)"
  ) +
  theme_minimal(16) +
  theme(
    plot.title.position = "plot",
    plot.title = element_text(size = 32),
    plot.subtitle = element_text(size = 24, color = "gray40"),
    plot.caption = element_text(family = "Inconsolata", size = 14),
    legend.position = c(.2, .8),
    legend.text = element_text(size = 26),
    legend.title = element_text(size = 20)
  )

ggsave(
  plot = p1,
  filename = "covid19/fallecimientos-acumulados-2020-2021-nuevo-criterio.png",
  width = 12,
  height = 9
)
