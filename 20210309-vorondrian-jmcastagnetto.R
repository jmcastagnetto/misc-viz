library(tidyverse)
library(ggfx)

p1 <- ggplot(
  randu,
  aes(x, y, z)
) +
  with_inner_glow(
    with_outer_glow(
      ggforce::geom_voronoi_tile(aes(color = z/y > x,
                                   fill = z, alpha = x),
                               size = 1,
                               show.legend = FALSE),
      color = "yellow",
      sigma = 10
    ),
    color = "cyan",
    sigma = 10
  )+
  paletteer::scale_color_paletteer_d("khroma::bright") +
  paletteer::scale_fill_paletteer_c("scico::lajolla") +
  coord_equal() +
  labs(
    caption = "Vorondrian (2021, Jesus M. Castagnetto)     "
  ) +
  theme_void() +
  theme(
    plot.margin = unit(rep(1, 4), "cm"),
    plot.caption.position = "panel",
    plot.caption = element_text(family = "GFS Artemisia", size = 18)
  )

ggsave(
  plot = p1,
  filename = "plots/20210309-vorondrian-jmcastagnetto.png",
  width = 12,
  height = 12
)
