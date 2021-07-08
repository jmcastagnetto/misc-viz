library(tidyverse)
library(randomcoloR)
library(ggsankey)
library(ggtrack)

plot_df <- read_csv("covid19/desinformantes_saludconlupa.csv") %>%
  select(country, profession, topics) %>%
  separate_rows(
    topics,
    sep = ","
  ) %>%
  distinct() %>%
  make_long(country, profession, topics)

p1 <- ggplot(
  plot_df,
  aes(
    x = x,
    next_x = next_x,
    node = node,
    label = node,
    next_node = next_node,
    fill = factor(node)
  )
) +
  geom_sankey(
    flow.alpha = .7,
    width = .3,
  ) +
  geom_sankey_label(
    fill = "white",
    size = 4
  ) +
  scale_fill_manual(
    values = distinctColorPalette(47)
  ) +
  scale_x_discrete(
    labels = c(
      "country" = "País",
      "profession" = "Ocupación",
      "topics" = "Temas"
    )
  ) +
  theme_sankey(base_size = 12) +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = .5, size = 22),
    plot.subtitle = element_text(color = "grey50", hjust = .5, size = 18),
    axis.text.x = element_text(size = 18, face = "bold",
                               color = "black")
  ) +
  labs(
    x = "",
    y = "",
    title = "COVID-19 - \"Desinformantes\": De donde vienen, que hacen y sobre que desinforman",
    subtitle = "Fuente: Serie \"Desinformantes\" de @saludconlupa"
  )

p2 <- ggtrack(
  p1,
  qr_content = "https://github.com/jmcastagnetto/misc-viz",
  logo = "common/for-tagline-jmcastagnetto-twitter2.png",
  height_plot = 35,
  height_tracker = 2.5,
  plot.background = element_rect(fill = "gray90", color = "white"),
  plot.margin = margin(0, 1, 0, 1, "cm")
)

ggsave(
  plot = p2,
  filename = "covid19/desinformantes-sankey-plot.png",
  width = 16,
  height = 16
)
