library(tidyverse)
library(htmltab)
library(patchwork)

# congreso_2021_url <- "https://www.resultados.eleccionesgenerales2021.pe/EG2021/EleccionesCongresales/GenRl"
#
# download.file(congreso_2021_url, destfile = "2021-peru-general-elections/raw_congreso_table/congreso-tabla.html")

tbl <- htmltab(
  doc ="2021-peru-general-elections/raw_congreso_table/congreso-tabla.html",
  which = '//*[@id="table1"]'
)

cnames <- tbl[1,]
congreso_tbl <- tbl[2:nrow(tbl), ]
colnames(congreso_tbl) <- cnames
congreso_tbl <- congreso_tbl %>%
  janitor::clean_names() %>%
  mutate(
    total = str_remove_all(total, ",") %>% as.numeric(),
    percent_validos = str_remove(percent_validos, "%") %>% as.numeric()
  )

valla <- 5

df <- congreso_tbl %>%
  filter(percent_validos > valla)

cifras <- c()
for (voto in df$total) {
  cifras <- c(cifras, voto / 1:130)
}
cifras2 <- rev(sort(cifras))
cifra_repartidora <- cifras2[130]

congreso <- df %>%
  mutate(
    Curules = trunc(total / cifra_repartidora),
    total = format(total, big.mark = ","),
    percent_validos = sprintf("%.2f%%", percent_validos)
  ) %>%
  rename(
    "Organización Política" = 1,
    "Votos válidos" = 2,
    "% de votos válidos" = 3
  )

library(ggpol)

congreso_plot_df <- df %>%
  mutate(
    curules = trunc(total / cifra_repartidora)
  )

p1 <- ggplot(congreso_plot_df) +
  geom_parliament(
    aes(seats = curules, fill = organizacion_politica)) +
  scale_fill_brewer(
    type = "div", palette = "PuOr",
    labels = congreso_plot_df$organizacion_politica
  ) +
  labs(
    #fill = "Organización Política"
    fill = ""
  ) +
  theme_void() +
  theme(
    legend.margin = margin(.5, 0, .5, 0, unit = "lines")
  ) +
  coord_fixed() +
  guides(
    fill = guide_legend(nrow = 3)
  )

comp_congreso <- p1 /
wrap_elements(
  gridExtra::tableGrob(
    congreso,
    theme = gridExtra::ttheme_default(
      base_size = 12
    )
  )
) +
  plot_layout(
    height = c(1.5, 1),
    guides = "collect"
  ) +
  plot_annotation(
    title = "Perú: Elecciones Generales 2021",
    subtitle = "Posible composición del Congreso (Fuente: ONPE, 2021-04-13 14:45)",
    caption = "@jmcastagnetto, Jesus M. Castagnetto 2021-04-13"
  ) &
  theme(
    plot.title = element_text(size = 24),
    plot.subtitle = element_text(size = 22, color = "grey50"),
    plot.caption = element_text(family = "Inconsolata", size = 16),
    legend.position = "top"
  )

ggsave(
  plot = comp_congreso,
  filename = "2021-peru-general-elections/posible_composicion_congreso_2021.png",
  width = 11,
  height = 10
)
