# Código simple para extraer información básica del artículo
# "Desinformantes" en https://saludconlupa.com/series/desinformantes/

library(tidyverse)
library(rvest)

# documento principal
base_url <- "https://saludconlupa.com"
series_url <- paste0(base_url, "/series/desinformantes/")
doc <- read_html(series_url)
elements <- html_elements(doc, xpath = "//div[@class='item']")
info <- html_elements(doc,  xpath = "//div[@class='item-name']/a")
l1 <- elements %>%
  html_attrs() %>%
  enframe()
l2 <- info %>%
  html_attrs() %>%
  enframe()
d1 <- bind_rows(lapply(l1$value, as.data.frame.list))
d2 <- bind_rows(lapply(l2$value, as.data.frame.list))

# nombres y descripciones de los meta tags
n1 <- tibble()
for(url in d2$href) {
  url_a <- paste0(base_url, url)
  doc_a <- read_html(url_a)
  meta_a <- html_elements(doc_a, xpath = "//meta[@itemprop]") %>%
    html_attrs() %>%
    enframe()
  meta_a1 <- bind_cols(lapply(meta_a$value, as.data.frame.list))
  df <- tibble(
    url = meta_a1[, 2],
    name = meta_a1[, 4],
    description = meta_a1[, 6],
    img = meta_a1[, 8]
  )
  n1 = bind_rows(n1, df)
}

final_df <- bind_cols(n1, d1) %>%
  mutate(
    name = str_remove(name, " - Desinformantes")
  ) %>%
  select(
    id = data.id,
    name,
    description,
    country = data.country,
    profession = data.profession,
    topics = data.tags,
    img,
    url
  ) %>%
  mutate(
    # corrigiendo el país de uno de los registros
    country = if_else(
      id == 1513,
      "Perú",
      country
    )
  )

write_csv(
  final_df,
  file = "covid19-desinformantes-ojopublico/desinformantes_saludconlupa.csv"
)
