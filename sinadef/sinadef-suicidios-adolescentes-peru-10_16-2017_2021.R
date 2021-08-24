library(tidyverse)
library(archive)

url_sinadef <- "https://cloud.minsa.gob.pe/s/g9KdDRtek42X3pg/download"
download.file(url_sinadef, destfile = "/tmp/sinadef.7z")
csv_file <- archive_read(
  archive = "/tmp/sinadef.7z",
  file = "TB_SINADEF.csv",
  mode = "r",
  format = "7zip"
)

csv_spec <- cols(
  .default = col_character(),
  EDAD = col_number(),
  AÑO = col_number(),
  MES = col_number()
)

sinadef <- read_csv(
  csv_file,
  col_types = csv_spec
) %>%
  janitor::clean_names() %>%
  rename(año = ano) %>%
  mutate(
    fecha = lubridate::dmy(fecha),
    en_peru = (pais_domicilio == "PERU")
  )

min_date <- min(sinadef$fecha, na.rm = TRUE)
max_date <- max(sinadef$fecha, na.rm = TRUE)
adolescentes <- sinadef %>%
  filter(
    en_peru &
    tiempo_edad == "AÑOS" &
      (edad > 9 & edad < 18) &
      str_detect(muerte_violenta, "SUICIDIO")
  ) %>%
  group_by(
    año, mes, sexo
  ) %>%
  tally()


plot_df <- adolescentes %>%
  mutate(
    fecha = as.Date(glue::glue("{año}-{sprintf('%02d', mes)}-{01}")),
    sexo = if_else(sexo == "M", "Sexo: Masculino", "Sexo: Femenino")
  ) %>%
  arrange(sexo, fecha) %>%
  group_by(sexo, año) %>%
  mutate(
    n_acum = cumsum(n),
    año = factor(año),
    mes_lbl = month.abb[mes],
    mes_lbl = fct_reorder(
      mes_lbl,
      mes
    )
  )

lbl_lines <- plot_df %>%
  group_by(sexo, año) %>%
  filter(n_acum == max(n_acum)) %>%
  mutate(
    lbl = glue::glue("{año} (N: {n_acum})")
  )

p1 <- ggplot(
  plot_df,
  aes(x = mes_lbl, y = n_acum, group = año, color = año)
) +
  geom_line(size = 1, show.legend = FALSE) +
  ggrepel::geom_label_repel(
    data = lbl_lines,
    aes(x = mes_lbl, y = n_acum, group = año, label = lbl),
    hjust = 0,
    nudge_x = .2,
    label.size = 0,
    size = 5,
    fontface = "bold",
    show.legend = FALSE
  ) +
  scale_x_discrete(expand = expansion(add = c(0, 3))) +
  scale_y_continuous(n.breaks = 7) +
  scale_color_brewer(palette = "Dark2") +
  facet_wrap(~sexo) +
  theme_minimal(18) +
  theme(
    plot.title = element_text(size = 32),
    plot.subtitle = element_text(size = 24, color = "grey50"),
    plot.caption = element_text(family = "Inconsolata"),
    plot.background = element_rect(fill = "white"),
    strip.text = element_text(size = 22, face = "bold")
  ) +
  labs(
    x = "Mes del año",
    y = "Cantidad acumulada",
    title = "Fallecimientos por suicidio en adolescentes (10 a 17 años, Perú)",
    subtitle = glue::glue("Fuente: SINADEF (del {min_date} al {max_date})"),
    caption = glue::glue("@jmcastagnetto, Jesus M. Castagnetto ({Sys.Date()})")
  )

ggsave(
  p1,
  filename = "sinadef/sinadef-suicidios-adolescentes-peru-10_16-2017_2021.png",
  width = 18,
  height = 9
)
