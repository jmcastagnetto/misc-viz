library(tidyverse)
library(gt)

load("2021-vacunacion-covid19-peru/vacunas-preproc.Rdata")

fecha_corte <- unique(vacunas$FECHA_CORTE)
tbl_dosis <- table(vacunas$DOSIS)
n_vacunas <- nrow(vacunas)
n_dosis1 <- tbl_dosis[1]
n_dosis2 <- tbl_dosis[2]
n_uuids <- length(unique(vacunas$UUID))

tbl <- tribble(
  ~item, ~Cantidad, ~Porcentaje,
  "Total de personas vacunadas", n_uuids, NA,
  "Personas que recibieron ambas dosis", nrow(ambas), nrow(ambas) / n_uuids,
  "Personas que recibieron sólo la primera dosis", nrow(solo_v1), nrow(solo_v1) / n_uuids,
  "Personas que recibieron sólo la segunda dosis", nrow(solo_v2), nrow(solo_v2) / n_uuids
) %>%
gt(rowname_col = "item") %>%
  tab_header(
    title = "Perú: Resúmen de vacunación por COVID-19",
    subtitle = glue::glue("Fuente: Datos abiertos del MINSA, al {fecha_corte}")
  ) %>%
  fmt_number(
    columns = "Cantidad",
    decimals = 0
  ) %>%
  fmt_percent(
    columns = "Porcentaje", decimals = 3
  ) %>%
  fmt_missing(
    columns = "Porcentaje",
    missing_text = ""
  ) %>%
  tab_footnote(
    footnote = "Posibles errores de registro",
    locations = cells_body(
      columns = c("Cantidad", "Porcentaje"),
      rows = "Personas que recibieron sólo la segunda dosis"
    )
  ) %>%
  tab_source_note(
    source_note = md(glue::glue("`@jmcastagnetto, Jesus M. Castagnetto, {Sys.Date()}`"))
  ) %>%
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      rows = "Total de personas vacunadas"
    )
  ) %>%
  tab_style(
    style = list(
      cell_text(color = "blue")
    ),
    locations = cells_body(
      columns = "Porcentaje"
    )
  ) %>%
  tab_style(
    style = list(
      cell_text(style = "italic", color = "red")
    ),
    locations = cells_body(
      rows = "Personas que recibieron sólo la segunda dosis"
    )
  ) %>%
  tab_options(
    table.font.names = c("Roboto"),
    table.font.size = 24,
    source_notes.font.size = 12,
    footnotes.font.size = 16
  )
tbl
gtsave(
  tbl,
  filename = "2021-vacunacion-covid19-peru/covid19-resumen-vacunacion-peru.png"
)

