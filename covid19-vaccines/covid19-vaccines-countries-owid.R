library(tidyverse)
library(gt)
library(sysfonts)

font_add("Noto Colo Emoji", regular = "/usr/share/fonts/truetype/noto/NotoColorEmoji.ttf")

owid_url <- "https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/locations.csv"

raw <- vroom::vroom(owid_url)

vaccines <- raw %>%
  separate_rows(
    vaccines,
    sep = ","
  ) %>%
  mutate(
    vaccines = str_trim(vaccines) %>% str_squish() %>% str_replace("/ ", "/")#,
    #loc_date = glue::glue("{location} ({last_observation_date})")
  ) %>%
  group_by(vaccines) %>%
  summarise(
    n = n(),
    Countries = paste(location, collapse = ", ")
  ) %>%
  mutate(
    vaccines = paste0(vaccines, " (N=", n, ")")
  ) %>%
  select(-n) %>%
  rename(
    Vaccine = vaccines
  )

today <- Sys.Date()

tab <- vaccines %>%
  gt() %>%
  tab_header(
    title = md("**Which countries are using each COVID-19 Vaccine**"),
    subtitle = "From OWID vaccine data (https://github.com/owid/covid-19-data/)"
  ) %>%
  tab_source_note(
    md(glue::glue("**`@jmcastagnetto, Jesus M. Castagnetto, Updated on {today}`**"))
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "gray70"),
      cell_text(style = "italic", weight = "bold")
    ),
    locations = cells_column_labels(columns = c("Vaccine", "Countries"))
  )

gtsave(
  tab,
  filename = "covid19-vaccines/covid19-vaccines-countries.html"
)

gtsave(
  tab,
  filename = "covid19-vaccines/covid19-vaccines-countries.png"
)

# network

library(tidygraph)
library(ggraph)

vaccines_tmp <- raw %>%
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

vaccines_graph <- vaccines_tmp %>%
  as_tbl_graph() %>%
  mutate(Popularity = centrality_degree(mode = 'in'))

ggraph(
  vaccines_graph,
  layout = "stress"
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
    aes(label = name), #size = Popularity*100),
    repel = TRUE,
    max.overlaps = 100,
    show.legend = FALSE,
    family = "Noto Color Emoji"
  ) +
  scale_color_fermenter(palette = "Dark2") +
  theme_graph(foreground = 'steelblue', fg_text_colour = 'white')
