library(tidyverse)

vacunas_url <- url("https://github.com/jmcastagnetto/covid-19-peru-vacunas/raw/main/datos/vacunas_covid.rds")
vacunas <- readRDS(vacunas_url)

fecha_corte <- unique(vacunas$FECHA_CORTE)
v_d1 <- vacunas %>%
  filter(DOSIS == 1)
v_d2 <- vacunas %>%
  filter(DOSIS == 2)
n_vacunas <- nrow(vacunas)
n_dosis1 <- nrow(v_d1)
n_dosis2 <- nrow(v_d2)

n_uuids <- length(unique(vacunas$UUID))

vcomb <- v_d1 %>%
  select(
    UUID,
    GRUPO_RIESGO,
    EDAD_1 = EDAD,
    SEXO,
    FECHA_VACUNACION_1 = FECHA_VACUNACION,
    DOSIS_1 = DOSIS,
    DEPARTAMENTO_1 = DEPARTAMENTO,
    PROVINCIA_1 = PROVINCIA,
    DISTRITO_1 = DISTRITO,
    FABRICANTE_1 = FABRICANTE
  ) %>%
  full_join(
    v_d2 %>%
      select(
        UUID,
        EDAD_2 = EDAD,
        FECHA_VACUNACION_2 = FECHA_VACUNACION,
        DOSIS_2 = DOSIS,
        DEPARTAMENTO_2 = DEPARTAMENTO,
        PROVINCIA_2 = PROVINCIA,
        DISTRITO_2 = DISTRITO,
        FABRICANTE_2 = FABRICANTE
      ),
    by = "UUID"
  )

solo_v1 <- vcomb %>%
  filter(is.na(FECHA_VACUNACION_2))

solo_v2 <- vcomb %>%
  filter(is.na(FECHA_VACUNACION_1))

ambas <- vcomb %>%
  filter(!is.na(FECHA_VACUNACION_1) & !is.na(FECHA_VACUNACION_2))

# sanity check
(nrow(solo_v1) + nrow(solo_v2) + 2*nrow(ambas)) == nrow(vacunas)

save(
  vacunas,
  solo_v1,
  solo_v2,
  ambas,
  file = "2021-vacunacion-covid19-peru/vacunas-preproc.Rdata"
)
