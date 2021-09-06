library(tidyverse)

download.file(
  url = "https://cloud.minsa.gob.pe/s/BosSrQ5wDf86xxg/download",
  destfile = "covid19-hosp-vac-uci-fallecidos/TB_HOSP_VAC_FALLECIDOS.csv"
)
R.utils::gzip("covid19-hosp-vac-uci-fallecidos/TB_HOSP_VAC_FALLECIDOS.csv")

hvf <- read_csv(
  "covid19-hosp-vac-uci-fallecidos/TB_HOSP_VAC_FALLECIDOS.csv.gz",
  col_types = cols(
    .default = col_character(),
    eess_renaes = col_integer(),
    id_eess = col_integer(),
    id_persona = col_integer(),
    anho_nac = col_integer(),
    con_oxigeno = col_integer(),
    con_ventilacion = col_integer(),
    flag_vacuna = col_integer(),
    fecha_ingreso_hosp = col_date(format = "%d/%m/%Y"),
    fecha_ingreso_uci = col_date(format = "%d/%m/%Y"),
    fecha_ingreso_ucin = col_date(format = "%d/%m/%Y"),
    fecha_segumiento_hosp_ultimo = col_date(format = "%d/%m/%Y"),
    fecha_dosis1 = col_date(format = "%d/%m/%Y"),
    fecha_dosis2 = col_date(format = "%d/%m/%Y"),
    cdc_fecha_fallecido_covid = col_date(format = "%d/%m/%Y"),
    con_oxigeno = col_logical(),
    con_ventilacion = col_logical(),
    flag_uci = col_logical(),
    cdc_fallecido_covid = col_logical(),
    cdc_positividad = col_logical()
  )
) %>%
  mutate(
    edad = lubridate::year(Sys.Date()) - anho_nac,
    edad = if_else(edad < 0, 0, edad),
    rango_edad = cut(
      edad,
      c(seq(0, 80, 20), 130),
      include.lowest = TRUE,
      right = FALSE,
      labels = c("0-19",
                 "20-39",
                 "40-59",
                 "60-79",
                 "80+")
    ) %>%
      fct_explicit_na("(Desconocido)")
  ) %>%
  mutate_at(
    vars(sexo, eess_diresa, eess_red, eess_nombre, evolucion_hosp_ultimo,
         ubigeo_inei_domicilio, dep_domicilio, prov_domicilio, dist_domicilio,
         fabricante_vacuna
    ),
    factor
  )

saveRDS(hvf,
        "covid19-hosp-vac-uci-fallecidos/hosp_vac_fallecido.rds",
        compress = "xz")
