library(tidyverse)
library(dbplyr)
library(DBI)
library(gt)

# para descargar la versión mas reciente de los datos
# db_url <- "https://ronderos.pe/datasette/elecciones_peru_2021.db"
# fn <- tempfile()
# download.file(db_url, fn)

# descargado temporalmente en este dir
fn <- "~/tmp/elecciones_peru_2021.db"
con <- dbConnect(RSQLite::SQLite(), fn)

pres <- tbl(con, "presidencial")

actas <- pres %>%
  select(
    mesa,
    v1_CCODI_UBIGEO,
    v1_DEPARTAMENTO,
    v1_TNOMB_LOCAL,
    v1_TOT_CIUDADANOS_VOTARON,
    v1_NNUME_HABILM,
    v2_CCODI_UBIGEO,
    v2_TNOMB_LOCAL,
    v2_TOT_CIUDADANOS_VOTARON,
    v2_NNUME_HABILM
  ) %>%
  collect() %>%
  mutate(
    mismo_local = (v1_TNOMB_LOCAL == v2_TNOMB_LOCAL),
    mismo_ubigeo = (v1_CCODI_UBIGEO == v2_CCODI_UBIGEO)
  )

actas <- actas_raw %>%
  filter(!is.na(v1_CCODI_UBIGEO))

cambios_mesa <- actas %>%
  mutate(
    donde = if_else(
      v1_DEPARTAMENTO %in% c("AFRICA", "AMERICA", "ASIA", "EUROPA", "OCEANIA"),
      "Extranjero",
      "Perú"
    )
  ) %>%
  group_by(donde, mismo_local, mismo_ubigeo) %>%
  tally() %>%
  group_by(donde) %>%
  mutate(
    pct_donde = n / sum(n)
  ) %>%
  ungroup() %>%
  mutate(
    pct_total = n / sum(n)
  )

gt(cambios_mesa, groupname_col = "donde") %>%
  tab_header(
    title = "Cambios de locación de mesas entre primera y segunda vuelta",
    subtitle = "Elecciones Generales y Presidenciales 2021, Perú"
  ) %>%
  tab_source_note(
    source_note = md("**Fuente de datos: https://ronderos.pe**<br>`@jmcastagnetto, Jesus M. Castagnetto (2021-06-18)`")
  ) %>%
  cols_label(
      mismo_local = md("**En el mismo local**"),
      mismo_ubigeo = md("**En el mismo distrito/ciudad**"),
      n = md("**Cantidad de mesas**"),
      pct_donde = md("**Porcentaje por grupo**"),
      pct_total = md("**Porcentaje total**")
  ) %>%
  fmt_percent(
    columns = c(pct_donde, pct_total)
  ) %>%
  tab_style(
    style = cell_text(weight = "bold", style = "italic"),
    locations = cells_row_groups()
  ) %>%
  summary_rows(
    groups = TRUE,
    columns = c(n),
    fns = list("Sub total" = ~sum(.)),
    formatter = fmt_number,
    decimals = 0
  ) %>%
  summary_rows(
    groups = TRUE,
    columns = c(pct_donde, pct_total),
    fns = list("Sub total" = ~sum(.)),
    formatter = fmt_percent
  ) %>%
  grand_summary_rows(
    columns = c(n),
    fns = list("Total" = ~sum(.)),
    formatter = fmt_number,
    decimals = 0
  ) %>%
  grand_summary_rows(
    columns = c(pct_total),
    fns = list("Total" = ~sum(.)),
    formatter = fmt_percent
  ) %>%
  opt_table_lines() %>%
  gtsave(
    filename = "2021-peru-general-elections/cambios-mesas-electorales-1ra-2da-vueltas.png"
  )


