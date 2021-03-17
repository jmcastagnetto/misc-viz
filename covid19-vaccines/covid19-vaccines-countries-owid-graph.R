library(tidyverse)
library(sysfonts)
library(tidygraph)
library(ggraph)

today <- Sys.Date()
font_add("Noto Color Emoji", regular = "/usr/share/fonts/truetype/noto/NotoColorEmoji.ttf")

owid_url <- "https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/locations.csv"

tmp <- vroom::vroom(owid_url) %>%
  filter(str_detect(iso_code, "OWID", negate = TRUE)) %>%
  separate_rows(
    vaccines,
    sep = ","
  ) %>%
  mutate(
    vaccines = str_trim(vaccines) %>% str_squish() %>% str_replace("/ ", "/"),
    symbol = countrycode::countrycode(iso_code, origin = "iso3c",
                                      destination = "unicode.symbol")
  ) %>%
  select(
    symbol,
    vaccines
  ) %>%
  distinct() %>%
  rename(
    from = 1,
    to = 2
  )

vaccines_graph <- tmp %>%
  as_tbl_graph() %>%
  mutate(Popularity = centrality_degree(mode = 'in'))

set.seed(13579)
g1 <- ggraph(
  vaccines_graph,
  layout = "fr"
) +
  geom_edge_diagonal(
    color = "grey70",
    show.legend = FALSE
  ) +
  geom_node_point(
    aes(size = Popularity),
    color = "cyan",
    alpha = .7,
    show.legend = FALSE
  ) +
  geom_node_text(
    aes(label = name),
    repel = TRUE,
    max.overlaps = 100,
    show.legend = FALSE,
    family = "Noto Color Emoji",
    size = 5,
    hjust = .5,
    vjust = .5
  ) +
  scale_color_fermenter(palette = "Dark2") +
  theme_graph(
    foreground = 'steelblue',
    fg_text_colour = 'white',
    base_size = 20,
    title_family = "Roboto",
    title_size = 36,
    subtitle_size = 28,
    caption_size = 18,
    caption_family = "Inconsolata",
    caption_face = "plain"
  ) +
  labs(
    title = "What vaccines each country uses",
    subtitle = "From OWID vaccine data (https://github.com/owid/covid-19-data/)",
    caption = glue::glue("@jmcastagnetto, Jesus M. Castagnetto, Updated on {today}")
  )
g1
ggsave(
  plot = g1,
  filename = "covid19-vaccines/covid19-vaccines-countries-owid-graph.png",
  width = 20,
  height = 20
)
