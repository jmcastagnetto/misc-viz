library(tidyverse)
library(ggtext)

# Fuente: https://www.web.onpe.gob.pe/claridadCiudadano2/
# Extraído manualmente el 2021-05-15 - Elecciones Generales 2021
# A nivel del organizaciones políticas

# Financiamiento privado
# https://www.web.onpe.gob.pe/claridadCiudadano2/reporte2/getGastosIngresosOP
fp_data <- '
{
	"data": [
		{
			"N_ORGANIZACION_PK": 242,
			"ORGANIZACION": "PODEMOS PERU",
			"TIPO_ORGANIZACION": "PARTIDO POLÍTICO",
			"CODIGO_TIPO_ORGANIZACION": 1,
			"C_SIGLAS": "PP",
			"MONTO_INGRESO": 2743376.59,
			"MONTO_GASTO": 2064338.67,
			"C_NOMBRE_IMAGEN": "242",
			"FECHA": "19/03/2021",
			"FECHA_PRESENTACION": "Mar 19, 2021 12:00:00 AM",
			"ESTADO_FECHA": 2
		},
		{
			"N_ORGANIZACION_PK": 4,
			"ORGANIZACION": "ALIANZA PARA EL PROGRESO",
			"TIPO_ORGANIZACION": "PARTIDO POLÍTICO",
			"CODIGO_TIPO_ORGANIZACION": 1,
			"C_SIGLAS": "PP",
			"MONTO_INGRESO": 1714969.95,
			"MONTO_GASTO": 2413126.31,
			"C_NOMBRE_IMAGEN": "alianza_progreso",
			"FECHA": "19/03/2021",
			"FECHA_PRESENTACION": "Mar 19, 2021 12:00:00 AM",
			"ESTADO_FECHA": 2
		},
		{
			"N_ORGANIZACION_PK": 28,
			"ORGANIZACION": "AVANZA PAIS - PARTIDO DE INTEGRACION SOCIAL",
			"TIPO_ORGANIZACION": "PARTIDO POLÍTICO",
			"CODIGO_TIPO_ORGANIZACION": 1,
			"C_SIGLAS": "PP",
			"MONTO_INGRESO": 1351940.43,
			"MONTO_GASTO": 1649227.83,
			"C_NOMBRE_IMAGEN": "123",
			"FECHA": "19/03/2021",
			"FECHA_PRESENTACION": "Mar 19, 2021 12:00:00 AM",
			"ESTADO_FECHA": 2
		},
		{
			"N_ORGANIZACION_PK": 5,
			"ORGANIZACION": "PARTIDO DEMOCRÁTICO SOMOS PERÚ",
			"TIPO_ORGANIZACION": "PARTIDO POLÍTICO",
			"CODIGO_TIPO_ORGANIZACION": 1,
			"C_SIGLAS": "PP",
			"MONTO_INGRESO": 621833,
			"MONTO_GASTO": 451954.07,
			"C_NOMBRE_IMAGEN": "somos_peru",
			"FECHA": "19/03/2021",
			"FECHA_PRESENTACION": "Mar 19, 2021 12:00:00 AM",
			"ESTADO_FECHA": 2
		},
		{
			"N_ORGANIZACION_PK": 253,
			"ORGANIZACION": "VICTORIA NACIONAL",
			"TIPO_ORGANIZACION": "PARTIDO POLÍTICO",
			"CODIGO_TIPO_ORGANIZACION": 1,
			"C_SIGLAS": "PP",
			"MONTO_INGRESO": 590146,
			"MONTO_GASTO": 670904.21,
			"C_NOMBRE_IMAGEN": "253",
			"FECHA": "19/03/2021",
			"FECHA_PRESENTACION": "Mar 19, 2021 12:00:00 AM",
			"ESTADO_FECHA": 2
		},
		{
			"N_ORGANIZACION_PK": 240,
			"ORGANIZACION": "PARTIDO POLITICO CONTIGO",
			"TIPO_ORGANIZACION": "PARTIDO POLÍTICO",
			"CODIGO_TIPO_ORGANIZACION": 1,
			"C_SIGLAS": "PP",
			"MONTO_INGRESO": 221280,
			"MONTO_GASTO": 218794.26,
			"C_NOMBRE_IMAGEN": "240",
			"FECHA": "19/03/2021",
			"FECHA_PRESENTACION": "Mar 19, 2021 12:00:00 AM",
			"ESTADO_FECHA": 2
		},
		{
			"N_ORGANIZACION_PK": 239,
			"ORGANIZACION": "PARTIDO MORADO",
			"TIPO_ORGANIZACION": "PARTIDO POLÍTICO",
			"CODIGO_TIPO_ORGANIZACION": 1,
			"C_SIGLAS": "PP",
			"MONTO_INGRESO": 176869.79,
			"MONTO_GASTO": 174869.79,
			"C_NOMBRE_IMAGEN": "239",
			"FECHA": "19/03/2021",
			"FECHA_PRESENTACION": "Mar 19, 2021 12:00:00 AM",
			"ESTADO_FECHA": 2
		},
		{
			"N_ORGANIZACION_PK": 8,
			"ORGANIZACION": "PARTIDO POPULAR CRISTIANO - PPC",
			"TIPO_ORGANIZACION": "PARTIDO POLÍTICO",
			"CODIGO_TIPO_ORGANIZACION": 1,
			"C_SIGLAS": "PP",
			"MONTO_INGRESO": 107881.94,
			"MONTO_GASTO": 102195,
			"C_NOMBRE_IMAGEN": "ppc",
			"FECHA": "19/03/2021",
			"FECHA_PRESENTACION": "Mar 19, 2021 12:00:00 AM",
			"ESTADO_FECHA": 2
		},
		{
			"N_ORGANIZACION_PK": 20,
			"ORGANIZACION": "UNION POR EL PERU",
			"TIPO_ORGANIZACION": "PARTIDO POLÍTICO",
			"CODIGO_TIPO_ORGANIZACION": 1,
			"C_SIGLAS": "PP",
			"MONTO_INGRESO": 71395.2,
			"MONTO_GASTO": 70949.01,
			"C_NOMBRE_IMAGEN": "img20",
			"FECHA": "19/03/2021",
			"FECHA_PRESENTACION": "Mar 19, 2021 12:00:00 AM",
			"ESTADO_FECHA": 2
		},
		{
			"N_ORGANIZACION_PK": 3,
			"ORGANIZACION": "EL FRENTE AMPLIO POR JUSTICIA, VIDA Y LIBERTAD",
			"TIPO_ORGANIZACION": "PARTIDO POLÍTICO",
			"CODIGO_TIPO_ORGANIZACION": 1,
			"C_SIGLAS": "PP",
			"MONTO_INGRESO": 50043.1,
			"MONTO_GASTO": 23642,
			"C_NOMBRE_IMAGEN": "frente_amplio",
			"FECHA": "19/03/2021",
			"FECHA_PRESENTACION": "Mar 19, 2021 12:00:00 AM",
			"ESTADO_FECHA": 2
		},
		{
			"N_ORGANIZACION_PK": 1,
			"ORGANIZACION": "FUERZA POPULAR",
			"TIPO_ORGANIZACION": "PARTIDO POLÍTICO",
			"CODIGO_TIPO_ORGANIZACION": 1,
			"C_SIGLAS": "PP",
			"MONTO_INGRESO": 48298.52,
			"MONTO_GASTO": 87068.77,
			"C_NOMBRE_IMAGEN": "fuerza_popular",
			"FECHA": "19/03/2021",
			"FECHA_PRESENTACION": "Mar 19, 2021 12:00:00 AM",
			"ESTADO_FECHA": 2
		},
		{
			"N_ORGANIZACION_PK": 24,
			"ORGANIZACION": "DEMOCRACIA DIRECTA",
			"TIPO_ORGANIZACION": "PARTIDO POLÍTICO",
			"CODIGO_TIPO_ORGANIZACION": 1,
			"C_SIGLAS": "PP",
			"MONTO_INGRESO": 39728,
			"MONTO_GASTO": 36223.67,
			"C_NOMBRE_IMAGEN": "124",
			"FECHA": "23/03/2021",
			"FECHA_PRESENTACION": "Mar 23, 2021 12:00:00 AM",
			"ESTADO_FECHA": 1
		},
		{
			"N_ORGANIZACION_PK": 23,
			"ORGANIZACION": "JUNTOS POR EL PERU",
			"TIPO_ORGANIZACION": "PARTIDO POLÍTICO",
			"CODIGO_TIPO_ORGANIZACION": 1,
			"C_SIGLAS": "PP",
			"MONTO_INGRESO": 31560,
			"MONTO_GASTO": 31650,
			"C_NOMBRE_IMAGEN": "128",
			"FECHA": "22/03/2021",
			"FECHA_PRESENTACION": "Mar 22, 2021 12:00:00 AM",
			"ESTADO_FECHA": 1
		},
		{
			"N_ORGANIZACION_PK": 241,
			"ORGANIZACION": "PARTIDO POLITICO NACIONAL PERU LIBRE",
			"TIPO_ORGANIZACION": "PARTIDO POLÍTICO",
			"CODIGO_TIPO_ORGANIZACION": 1,
			"C_SIGLAS": "PP",
			"MONTO_INGRESO": 24350,
			"MONTO_GASTO": 11719.67,
			"C_NOMBRE_IMAGEN": "241",
			"FECHA": "22/03/2021",
			"FECHA_PRESENTACION": "Mar 22, 2021 12:00:00 AM",
			"ESTADO_FECHA": 1
		},
		{
			"N_ORGANIZACION_PK": 205,
			"ORGANIZACION": "PARTIDO NACIONALISTA PERUANO",
			"TIPO_ORGANIZACION": "PARTIDO POLÍTICO",
			"CODIGO_TIPO_ORGANIZACION": 1,
			"C_SIGLAS": "PP",
			"MONTO_INGRESO": 15893.1,
			"MONTO_GASTO": 49370.89,
			"C_NOMBRE_IMAGEN": "205",
			"FECHA": "19/03/2021",
			"FECHA_PRESENTACION": "Mar 19, 2021 12:00:00 AM",
			"ESTADO_FECHA": 2
		},
		{
			"N_ORGANIZACION_PK": 21,
			"ORGANIZACION": "FRENTE POPULAR AGRICOLA FIA DEL PERU - FREPAP",
			"TIPO_ORGANIZACION": "PARTIDO POLÍTICO",
			"CODIGO_TIPO_ORGANIZACION": 1,
			"C_SIGLAS": "PP",
			"MONTO_INGRESO": 0,
			"MONTO_GASTO": 0,
			"C_NOMBRE_IMAGEN": "126",
			"FECHA": "19/03/2021",
			"FECHA_PRESENTACION": "Mar 19, 2021 12:00:00 AM",
			"ESTADO_FECHA": 2
		}
	],
	"success": true
}
'

tmp <- jsonlite::fromJSON(fp_data, simplifyDataFrame = TRUE)
df <- tmp$data %>%
  select(
    org = ORGANIZACION, MONTO_INGRESO, MONTO_GASTO
  ) %>%
  mutate(
    org = fct_reorder(org, MONTO_INGRESO)
  ) %>%
  pivot_longer(
    cols = c(MONTO_INGRESO, MONTO_GASTO),
    names_to = "concepto",
    names_prefix = "MONTO_",
    values_to = "monto"
  )

p1 <- ggplot(
  df,
  aes(x = monto, y = org, fill = concepto)
) +
  geom_col(position = position_dodge()) +
  scale_x_continuous(labels = scales::comma, limits = c(0, 3e6)) +
  labs(
    fill = "Concepto:",
    x = "Monto (en S/.)",
    y = "",
    title = "*Financiamiento Privado*<br/>Ingresos y gastos reportados por Organizaciones Políticas.",
    subtitle = "Perú, Elecciones Generales 2021",
    caption = "Fuente: ONPE (Claridad: https://www.web.onpe.gob.pe/claridadCiudadano2/, extraído el 2021-05-15)\n@jmcastagnetto, Jesus M. Castagnetto."
  ) +
  theme_minimal(base_size = 18) +
  theme(
    legend.position = "bottom",
    plot.title.position = "plot",
    plot.title = element_markdown(size = 24),
    plot.subtitle = element_text(color = "grey40"),
    plot.caption = element_text(family = "Inconsolata"),
    plot.margin = unit(rep(1, 4), "cm")
  )

ggsave(
  p1,
  filename = "2021-peru-general-elections/20210515-onpe-claridad-financiamiento-privado.png",
  width = 14,
  height = 10
)
