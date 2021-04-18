library(multidplyr)
library(tidyverse)

cluster <- new_cluster(4)
cluster_library(cluster, "tidyverse")

# fuente del RDS: https://github.com/jmcastagnetto/sinadef-peru-fallecimientos
sinadef_url <- url("https://github.com/jmcastagnetto/sinadef-peru-fallecimientos/raw/main/datos/sinadef-procesado.rds")
sinadef <- readRDS(sinadef_url)

sinadef_sexo_edad <- sinadef %>%
  filter(tiempo_edad == "AÑOS" &
           !is.na(edad) &
           !is.na(sexo) &
           sexo %in% c("MASCULINO", "FEMENINO") &
           pais_domicilio == "PERU"
         )

nrow(sinadef_sexo_edad) / nrow(sinadef)

en_peru <- sinadef_sexo_edad %>%
  group_by(sexo) %>%
  partition(cluster) %>%
  mutate(
    epi_week = lubridate::epiweek(fecha),
    epi_year = lubridate::epiyear(fecha),
    grupo_etario = cut(
      edad,
      breaks = c(0, 20, 40, 60, 80, 200),
      labels = c(
        "0-19",
        "20-39",
        "40-59",
        "60-79",
        "80+"
      ),
      include.lowest = TRUE,
      right = FALSE
    )
  ) %>%
  collect() %>%
  ungroup() %>%
  group_by(epi_year, epi_week, grupo_etario, sexo) %>%
  partition(cluster) %>%
  summarise(
    n = n()
  ) %>%
  collect()

saveRDS(
  en_peru,
  file = "sinadef-evolucion-grupo-etareo/fallecidos_peru_grupo_etario_sexo.rds"
)

