library(tidyverse)
library(gt)

primera <- readRDS("2021-peru-general-elections/onpe-datos-primera-vuelta.rds")
segunda <- readRDS("2021-peru-general-elections/onpe-datos-segunda-vuelta.rds")

por_estado <- primera %>%
  group_by(donde, descrip_estado_acta) %>%
  tally(name = "n1") %>%
  left_join(
    segunda %>%
      group_by(donde, descrip_estado_acta) %>%
      tally(name = "n2"),
    by = c("donde", "descrip_estado_acta")
  ) %>%
  arrange(donde, desc(n1)) %>%
  mutate(
    n1 = replace_na(n1, 0),
    n2 = replace_na(n2, 0)
  ) %>%
  group_by(donde) %>%
  mutate(
    p1_donde = n1 / sum(n1),
    p2_donde = n2 / sum(n2)
  ) %>%
  ungroup() %>%
  mutate(
    p1_total = n1 / sum(n1),
    p2_total = n2 / sum(n2)
  ) %>%
  select(
    donde,
    estado = descrip_estado_acta,
    n1,
    p1_donde,
    p1_total,
    n2,
    p2_donde,
    p2_total
  )

gt(
  por_estado,
  groupname_col = "donde",
  rowname_col = "estado"
) %>%
  tab_header(
    title = "Estados de las actas en primera y segunda vueltas electorales",
    subtitle = "Elecciones Presidenciales 2021, Perú"
  ) %>%
  tab_source_note(
    source_note = md(glue::glue("**Fuente de datos: ONPE** // @jmcastagnetto, Jesus M. Castagnetto ({Sys.time()})"))
  ) %>%
  cols_label(
    n1 = md("**Número de actas**"),
    p1_donde = md("**Porcentaje por grupo**"),
    p1_total = md("**Porcentaje total**"),
    n2 = md("**Número de actas**"),
    p2_donde = md("**Porcentaje por grupo**"),
    p2_total = md("**Porcentaje total**")
  ) %>%
  tab_spanner(
    label = md("*Primera vuelta*"),
    id = "v1",
    columns = c(n1, p1_donde, p1_total)
  ) %>%
  tab_spanner(
    label = md("*Segunda vuelta*"),
    id = "v2",
    columns = c(n2, p2_donde, p2_total)
  ) %>%
  fmt_percent(
    columns = c(p1_donde, p1_total, p2_donde, p2_total)
  ) %>%
  fmt_number(
    columns = c(n1, n2),
    decimals = 0
  ) %>%
  summary_rows(
    groups = TRUE,
    columns = c(n1, n2),
    fns = list("Sub total" = ~sum(.)),
    formatter = fmt_number,
    decimals = 0
  ) %>%
  summary_rows(
    groups = TRUE,
    columns = c(p1_donde, p1_total, p2_donde, p2_total),
    fns = list("Sub total" = ~sum(.)),
    formatter = fmt_percent
  ) %>%
  grand_summary_rows(
    columns = c(n1, n2),
    fns = list("Total" = ~sum(.)),
    formatter = fmt_number,
    decimals = 0
  ) %>%
  grand_summary_rows(
    columns = c(p1_total, p2_total),
    fns = list("Total" = ~sum(.)),
    formatter = fmt_percent
  ) %>%
  opt_table_lines() %>%
  tab_style(
    style = cell_fill(color = "cyan", alpha = .3),
    locations = list(
      cells_column_spanners(spanners = "v2"),
      cells_body(columns = c(n2, p2_donde, p2_total)),
      cells_column_labels(columns = c(n2, p2_donde, p2_total))
    )
  ) %>%
  tab_style(
    style = cell_text(align = "right", size = "medium", font = "Inconsolata"),
    location = cells_source_notes()
  ) %>%
  tab_style(
    style = cell_text(size = "medium", color = "#555555"),
    locations = cells_title(groups = "subtitle")
  ) %>%
  tab_style(
    style = cell_text(size = "x-large", weight = "bold"),
    locations = cells_title(groups = "title")
  ) %>%
  tab_options(
    summary_row.background.color = "#ACEACE",
    grand_summary_row.background.color = "#000000"
  ) %>%
  gtsave(
    filename = "2021-peru-general-elections/estados-de-actas-primera-segunda-vueltas-onpe.png"
  )
