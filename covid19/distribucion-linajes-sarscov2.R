library(tidyverse)
library(RCurl)
library(lubridate)

ins_data_url <- "https://datos.ins.gob.pe/dataset/7d9fdcc3-2b60-4486-8169-f176c1a3f724/resource/1980b70a-97d7-48a5-92fe-b54740450f76/download/pmgenoma_20agosto2021.csv"
csv_data <- getURL(ins_data_url, ssl.verifypeer = FALSE )
variantes <- read_csv(csv_data) %>%
  janitor::clean_names() %>%
  filter(!is.na(fecha_muestra)) %>%
  rename(fecha_corte = i_fecha_corte) %>%
  mutate(
    fecha_corte = ymd(fecha_corte),
    fecha_muestra = ymd(fecha_muestra),
    resultado = str_replace(resultado, "LINAJE", "Linaje") %>%
      str_trim() %>% str_squish(),
    resultado_tipo = substr(resultado, 1, 8) %>%
      str_replace("Sublinaj", "Sublinaje C"),
    epi_week = epiweek(fecha_muestra),
    epi_year = epiyear(fecha_muestra),
    departamento = departamento_paciente %>%
      str_replace("SIN DATOS", "*Desconocido*") %>%
      replace_na("*Desconocido*"),
    departamento = if_else(
      departamento == "LIMA" & provincia_paciente == "LIMA",
      "LIMA PROVINCIA",
      departamento
    )
  )

por_variante <- variantes %>%
  group_by(epi_week, resultado_tipo) %>%
  tally()

p1 <- ggplot(
  por_variante %>% filter(epi_week < 32),
  aes(x = epi_week, y = n, group = resultado_tipo, fill = resultado_tipo)
) +
  geom_col(width = 1) +
  scale_fill_brewer(type = "qual", palette = "Dark2") +
  theme_minimal(18) +
  theme(
    plot.title.position = "plot",
    plot.title = element_text(size = 26),
    plot.subtitle = element_text(size = 20, color = "grey50"),
    plot.caption = element_text(family = "Inconsolata", size = 14),
    plot.background = element_rect(fill = "white"),
    legend.position = c(.1, .6)
  ) +
  labs(
    fill = "Linajes",
    title = "COVID-19 Perú: Distribución de linajes SARS-COV-2 a nivel nacional",
    subtitle = "Fuente: INS/MINSA, hasta la semana 31 del 2021",
    caption = glue::glue("@jmcastagnetto, Jesus M. Castagnetto ({Sys.Date()})"),
    x = "Semana epidemiológica",
    y = "Frecuencia"
  )

ggsave(
  p1,
  filename = "covid19/distribucion-linajes-sarscov2-peru.png",
  width = 14,
  height = 10
)

por_dpto <- variantes %>%
  group_by(epi_week, resultado_tipo, departamento) %>%
  tally()

p2 <- ggplot(
  por_dpto %>% filter(epi_week < 32),
  aes(x = epi_week, y = n, group = resultado_tipo, fill = resultado_tipo)
) +
  geom_col(width = 1) +
  scale_fill_brewer(type = "qual", palette = "Dark2") +
  guides(
    fill = guide_legend(direction = "horizontal")
  ) +
  facet_wrap(~departamento, scales = "free_y") +
  theme_minimal(18) +
  theme(
    plot.title.position = "plot",
    plot.title = element_text(size = 28),
    plot.subtitle = element_text(size = 22, color = "grey50"),
    plot.caption = element_text(family = "Inconsolata", size = 14),
    plot.background = element_rect(fill = "white"),
    legend.position = c(.8, .1)
  ) +
  labs(
    fill = "Linajes",
    title = "COVID-19 Perú: Distribución de linajes SARS-COV-2 por Departamento",
    subtitle = "Fuente: INS/MINSA, hasta la semana 31 del 2021",
    caption = glue::glue("@jmcastagnetto, Jesus M. Castagnetto ({Sys.Date()})"),
    x = "Semana epidemiológica",
    y = "Frecuencia"
  )

ggsave(
  p2,
  filename = "covid19/distribucion-linajes-sarscov2-pordpto.png",
  width = 18,
  height = 14
)
