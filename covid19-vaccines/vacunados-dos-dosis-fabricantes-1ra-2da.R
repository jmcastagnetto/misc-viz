library(tidyverse)
library(GGally)
library(fst)

vac2 <- read_fst(
  "~/devel/github/covid-19-peru-vacunas/datos/vacunados-dos-dosis.fst",
  columns = c("fabricante_1", "fabricante_2")
)

my_pct_fmt <- function(pct) {
  if_else(
    pct < 1E-2,
    sprintf("%.4f%%", pct),
    sprintf("%.2f%%", pct)
  )
}

p1 <- ggplot(
  vac2,
  aes(y = fabricante_1, x = fabricante_2)
) +
  geom_bin_2d(show.legend = FALSE, color = "black") +
  stat_bin_2d(
    geom = "label",
    aes(
      label = paste0(
        str_trim(format(stat(count), big.mark = ",")),
        "\n",
        my_pct_fmt(100 * stat(count) / nrow(vac2))
      )
    ),
    label.size = 0,
    fill = "white",
    size = 8,
    fontface = "bold"
  ) +
  scale_fill_distiller(palette = "Dark2") +
  scale_x_discrete(position = "top") +
  scale_y_discrete(limits = rev) +
  theme_minimal(16) +
  theme(
    panel.grid = element_blank(),
    axis.title = element_text(face = "bold", size = 20),
    axis.text = element_text(color = "black"),
    plot.title.position = "plot",
    plot.title = element_text(size = 26),
    plot.subtitle = element_text(size = 22, color = "grey50"),
    plot.caption = element_text(family = "Inconsolata", size = 18),
    plot.background = element_rect(fill = "white"),
    plot.margin = unit(rep(.5, 4), "cm")
  ) +
  labs(
    title = "Vacunación COVID-19 en Perú: Fabricantes de la primera y segunda dosis",
    subtitle = "Para las personas que recibieron dos dosis - Fuente: MINSA (datos al 2021-09-18)",
    caption = "@jmcastagnetto, Jesus M. Castagnetto",
    x = "Segunda dosis",
    y = "Primera dosis"
  )

ggsave(
  p1,
  filename = "covid19-vaccines/vacunados-dos-dosis-fabricantes-1ra-2da.png",
  width = 14,
  height = 12
)

