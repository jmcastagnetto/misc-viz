library(tidyverse)
library(ggsankey)
library(glue)

load("2021-vacunacion-covid19-peru/vacunas-preproc.Rdata")

fecha_corte <- unique(vacunas$FECHA_CORTE)

vacunados = length(unique(vacunas$UUID)) %>% format(big.mark = ",")
ambas_dosis = nrow(ambas) %>% format(big.mark = ",")
solo_d1 = nrow(solo_v1) %>% format(big.mark = ",")
solo_d2 = nrow(solo_v2) %>% format(big.mark = ",")
masculino = vacunas %>%
  filter(SEXO == "MASCULINO") %>%
  select(UUID, SEXO) %>%
  distinct() %>%
  nrow() %>%
  format(big.mark = ",")
femenino = vacunas %>%
  filter(SEXO == "FEMENINO") %>%
  select(UUID, SEXO) %>%
  distinct() %>%
  nrow() %>%
  format(big.mark = ",")
sinopharm = vacunas %>%
  filter(FABRICANTE == "SINOPHARM") %>%
  select(UUID, FABRICANTE) %>%
  distinct() %>%
  nrow() %>%
  format(big.mark = ",")
pfizer = vacunas %>%
  filter(FABRICANTE == "PFIZER") %>%
  select(UUID, FABRICANTE) %>%
  distinct() %>%
  nrow() %>%
  format(big.mark = ",")
mismo_lugar <- sum(
  (ambas$DEPARTAMENTO_1 == ambas$DEPARTAMENTO_2) &
    (ambas$PROVINCIA_1 == ambas$PROVINCIA_2) &
    (ambas$DISTRITO_1 == ambas$DISTRITO_2)
) %>%
  format(big.mark = ",")
dif_distrito <- sum(
  (ambas$DEPARTAMENTO_1 == ambas$DEPARTAMENTO_2) &
    (ambas$PROVINCIA_1 == ambas$PROVINCIA_2) &
    (ambas$DISTRITO_1 != ambas$DISTRITO_2)
) %>%
  format(big.mark = ",")
dif_provincia <- sum(
  (ambas$DEPARTAMENTO_1 == ambas$DEPARTAMENTO_2) &
    (ambas$PROVINCIA_1 != ambas$PROVINCIA_2)
) %>%
  format(big.mark = ",")
dif_departamento <- sum(
  (ambas$DEPARTAMENTO_1 != ambas$DEPARTAMENTO_2)
) %>%
  format(big.mark = ",")
indeterminado <- (nrow(solo_v1) + nrow(solo_v2)) %>%
  format(big.mark = ",")


lbl_vacunados <- glue("Vacunados\nN: {vacunados}")
lbl_masculino <- glue("Masculino\nN: {masculino}")
lbl_femenino <- glue("Femenino\nN: {femenino}")
lbl_ambas <- glue("Ambas dosis\nN: {ambas_dosis}")
lbl_d1 <- glue("Sólo Primera Dosis\nN: {solo_d1}")
lbl_d2 <- glue("Sólo Segunda Dosis\nN: {solo_d2}")
lbl_sinopharm <- glue("SINOPHARM\nN: {sinopharm}")
lbl_pfizer <- glue("PFIZER\nN: {pfizer}")
lbl_mismo <- glue("Se vacunó en\nel mismo lugar\nN: {mismo_lugar}")
lbl_difdist <- glue("Diferente Distrito en\nla misma Provincia\nN: {dif_distrito}")
lbl_difprov <- glue("Diferente Provincia en\nel mismo Departamento\nN: {dif_provincia}")
lbl_difdpto <- glue("Diferente\nDepartamento\nN: {dif_departamento}")
lbl_indeterminado <- glue("Aún no tiene\nambas dosis\nregistradas\nN: {indeterminado}")

personas <- vacunas %>%
  ungroup() %>%
  select(UUID) %>%
  distinct() %>%
  mutate(
    dosis = case_when(
      (UUID %in% ambas$UUID) ~ lbl_ambas,
      (UUID %in% solo_v1$UUID) ~ lbl_d1,
      (UUID %in% solo_v2$UUID) ~ lbl_d2
    )
  ) %>%
  left_join(
    ambas %>%
      select(-GRUPO_RIESGO, -SEXO,
             -FABRICANTE_1, -FABRICANTE_2,
             -EDAD_1, -EDAD_2,
             -DOSIS_1, -DOSIS_2,
             -FECHA_VACUNACION_1,
             -FECHA_VACUNACION_2),
    by = "UUID"
  ) %>%
  mutate(
    locación = case_when(
      (DEPARTAMENTO_1 == DEPARTAMENTO_2) &
        (PROVINCIA_1 == PROVINCIA_2) &
        (DISTRITO_1 == DISTRITO_2) ~ lbl_mismo,
      (DEPARTAMENTO_1 == DEPARTAMENTO_2) &
        (PROVINCIA_1 == PROVINCIA_2) &
        (DISTRITO_1 != DISTRITO_2) ~ lbl_difdist,
      (DEPARTAMENTO_1 == DEPARTAMENTO_2) &
        (PROVINCIA_1 != PROVINCIA_2) ~ lbl_difprov,
      (DEPARTAMENTO_1 != DEPARTAMENTO_2) ~ lbl_difdpto
    )
  ) %>%
  left_join(
    vacunas %>%
      select(UUID, FABRICANTE, GRUPO_RIESGO, SEXO) %>%
      distinct(),
    by = "UUID"
  ) %>%
  select(
    -UUID,
    -DEPARTAMENTO_1,
    -PROVINCIA_1,
    -DISTRITO_1,
    -DEPARTAMENTO_2,
    -PROVINCIA_2,
    -DISTRITO_2
  ) %>%
  distinct() %>%
  add_column(
    total = lbl_vacunados,
    .before = 1
  ) %>%
  mutate(
    FABRICANTE = case_when(
      FABRICANTE == "SINOPHARM" ~ lbl_sinopharm,
      FABRICANTE == "PFIZER" ~ lbl_pfizer,
    ),
    SEXO = case_when(
      SEXO == "MASCULINO" ~ lbl_masculino,
      SEXO == "FEMENINO" ~ lbl_femenino
    )
  ) %>%
  rename_all(str_to_title)

personas_sankey_df <- make_long(personas,
                                Total,
                                Sexo,
                                Fabricante,
                                Dosis,
                                Locación) %>%
  mutate(
    node = replace_na(node, lbl_indeterminado),
    next_node = if_else(
      x == "Dosis" &
        next_x == "Locación" &
        is.na(next_node),
      lbl_indeterminado,
      next_node
    )
  )

p1 <- ggplot(
  personas_sankey_df,
  aes(x = x,
      next_x = next_x,
      node = node,
      next_node = next_node,
      fill = factor(node))
) +
  geom_sankey(show.legend = FALSE,
              flow.alpha = .7,
              node.fill = NA,
              width = 0) +
  geom_sankey_label(
    aes(label = node),
    size = 5,
    color = "white",
    fill = "gray40") +
  labs(
    x = "",
    title = "Perú: Campaña de vacunación contra el COVID-19",
    subtitle = glue("Fuente: Datos abiertos del MINSA, al {fecha_corte}"),
    caption = glue("@jmcastagnetto, Jesus M. Castagnetto ({Sys.Date()})")
  ) +
  theme_sankey(base_size = 24) +
  theme(
    plot.subtitle = element_text(color = "gray50"),
    plot.caption = element_text(family = "Inconsolata")
  )

ggsave(
  plot = p1,
  filename = "2021-vacunacion-covid19-peru/covi19-peru-vacunacion-sankey.png",
  width = 14,
  height = 9
)
