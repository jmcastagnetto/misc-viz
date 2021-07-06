library(tidyverse)
library(tidygraph)
library(ggraph)
library(ggtrack)

df <- read_csv("covid19/desinformantes_saludconlupa.csv") %>%
  select(-id, -description, -img, -url) %>%
  separate_rows(
    topics,
    sep = ","
  )

df1 <- df %>%
  select(from = 3, to = 4) %>%
  mutate_all(
    str_wrap,
    width = 20,
  ) %>%
  distinct() %>%
  as_tbl_graph()

set.seed(13579)
p1 <- ggraph(df1, layout = "dh") +
  geom_edge_link(color = "red") +
  geom_node_label(
    aes(label = name)
  ) +
  scale_color_brewer(palette = "Dark2") +
  labs(
    title = "COVID-19: Relación entre profesión de la persona y tema de desinformación",
    subtitle = "Fuente: Serie \"Desinformantes\" de @saludconlupa"
  ) +
  theme(
    plot.title = element_text(size = 18),
    plot.subtitle = element_text(size = 16, color = "gray40"),
    plot.margin = unit(rep(.5, 4), "cm")
  )

p2 <- ggtrack(
  p1,
  qr_content = "https://github.com/jmcastagnetto/tidytuesday-kludges",
  logo = "common/for-tagline-jmcastagnetto-twitter2.png",
  height_plot = 25,
  height_tracker = 2.5,
  plot.background = element_rect(fill = "gray90", color = "white"),
  plot.margin = margin(0, 1, 0, 1, "cm")
)


ggsave(
  plot = p2,
  filename = "covid19/desinformantes-profesion-tema.png",
  width = 16,
  height = 12
)
