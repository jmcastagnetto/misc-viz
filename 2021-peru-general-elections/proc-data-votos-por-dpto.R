library(tidyverse)
library(fs)

csv_list <- dir_ls(
  path = "2021-peru-general-elections/raw_presid_data/",
  glob = "*.csv"
)

partidos_df <- tibble()
votos_df <- tibble()

for (fn in csv_list) {
  dpto <- read_lines(
    fn,
    skip = 2,
    n_max = 1
  ) %>%
    str_remove_all('"')
  actas <- read_csv(
    fn,
    skip = 34,
    n_max = 1
  )
  mesas <- read_csv(
    fn,
    skip = 39,
    n_max = 1
  )
  tmp_partidos <- read_csv(
    fn,
    skip = 10,
    n_max = 18,
    col_names = c("partido", "votos", "pct_validos")
  ) %>%
    mutate(
      votos = as.numeric(votos),
      pct_validos = as.numeric(pct_validos),
      departamento = dpto
    ) %>%
    relocate(
      departamento,
      .before = partido
    )
  tmp_votos <- read_csv(
    fn,
    skip = 28,
    n_max = 4,
    col_names = c("metrica", "cantidad", "dummy")
  ) %>%
    select(-dummy) %>%
    mutate(
      cantidad = as.numeric(cantidad),
      departamento = dpto
    ) %>%
    pivot_wider(
      id_cols = departamento,
      names_from = metrica,
      values_from = cantidad
    ) %>%
    janitor::clean_names() %>%
    mutate(
      actas_totales = (actas %>% pull(1))[1],
      actas_procesadas = (actas %>% pull(2))[1],
      actas_en_jne = (actas %>% pull(3))[1],
      pct_actas_procesadas = actas_procesadas / actas_totales,
      mesas_instaladas = (mesas %>% pull(1))[1],
      mesas_no_instaladas = (mesas %>% pull(2))[1],
      mesas_por_informar = (mesas %>% pull(3))[1],
      mesas_totales = mesas_instaladas + mesas_no_instaladas + mesas_por_informar
    )
  partidos_df <- bind_rows(partidos_df, tmp_partidos)
  votos_df <- bind_rows(votos_df, tmp_votos)
}

saveRDS(
  partidos_df,
  file = "2021-peru-general-elections/resultados-partidos-dpto.rds"
)

saveRDS(
  votos_df,
  file = "2021-peru-general-elections/estado-votos-dpto.rds"
)