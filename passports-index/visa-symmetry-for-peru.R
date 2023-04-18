library(tidyverse)
library(sf)
library(rnaturalearth)
library(countrycode)
library(ggtext)
library(patchwork)

world <- ne_countries(scale = 110, returnclass = "sf") %>%
  filter(sov_a3 != "ATA") # remove Antartica

visa_df <- read_csv("https://github.com/ilyankou/passport-index-dataset/raw/master/passport-index-tidy-iso3.csv")

mk_visa_map <- function(world, visa_df, iso3) {
  country_name <- countrycode(iso3, origin = "iso3c", destination = "country.name")
  from_country <- visa_df %>%
    filter(Passport == iso3) %>%
    rename(
      to = Destination,
      req_1 = Requirement
    ) %>%
    select(-Passport)

  to_country <- visa_df %>%
    filter(Destination == iso3) %>%
    rename(
      from = Passport,
      req_2 = Requirement
    ) %>%
    select(-Destination)

  novisa <- c("visa free", "120", "10", "120", "14", "15",
              "180", "21", "240", "28", "30", "31", "360",
              "42", "45", "60", "7", "90")
  yesvisa <- c("e-visa", "visa on arrival", "visa required")
  noadmission <- c("no admission", "covid ban")

  merged_df <- from_country %>%
    left_join(
      to_country,
      by = join_by(to == from)
    ) %>%
    add_column(
      country_1 = iso3,
      .before = 1
    ) %>%
    rename(
      country_2 = to,
      req_1_2 = req_1,
      req_2_1 = req_2
    ) %>%
    mutate(
      situation = case_when(
        country_1 == country_2 ~ "country",
        req_1_2 %in% noadmission ~ "forbidden",
        (req_1_2 %in% novisa) & (req_2_1 %in% novisa) ~ "symmetric",
        (req_1_2 %in% yesvisa) & (req_2_1 %in% yesvisa) ~ "symmetric",
        (req_1_2 %in% novisa) & (req_2_1 %in% yesvisa) ~ "favors 1",
        (req_1_2 %in% yesvisa) & (req_2_1 %in% novisa) ~ "favors 2",
        TRUE ~ NA_character_
      ),
      visa_need = case_when(
        country_1 == country_2 ~ "country",
        req_1_2 %in% novisa ~ "novisa",
        req_1_2 %in% yesvisa ~ "yesvisa",
        req_1_2 %in% noadmission ~ "noadmission"
      )
    )

  world_df <- world %>%
    left_join(
      merged_df,
      by = c("adm0_a3" = "country_2")
    )

  p1 <- ggplot(world_df) +
    geom_sf(aes(fill = visa_need)) +
    scale_fill_manual(
      values = c(
        "noadmission" = "gray30",
        "yesvisa" = "#1F78B4",
        "novisa" = "#B2DF8A",
        "country" = "peru"
      ),
      labels = c(
        "noadmission" = "Forbidden",
        "novisa" = glue::glue("No visa needed\nfrom {country_name}"),
        "yesvisa" = glue::glue("Visa needed\nfrom {country_name}"),
        "country" = country_name
      ),
      na.value = "white"
    ) +
    guides(
      fill = guide_legend(title = "Visa requirements")
    ) +
    coord_sf(crs = "+proj=robin") +
    theme_void(
      base_family = "Atkinson Hyperlegible",
      base_size = 16
    ) +
    theme(
      legend.position = "bottom"
    )

  p2 <- ggplot(world_df) +
    geom_sf(aes(fill = situation)) +
    scale_fill_manual(
      values = c(
        "forbidden" = "gray30",
        "symmetric" = "#A6CEE3",
        "favors 2" = "#1F78B4",
        "favors 1" = "#B2DF8A",
        "country" = "peru"
      ),
      labels = c(
        "forbidden" = "Forbidden",
        "symmetric" = "Symmetric or\nequivalent",
        "favors 1" = glue::glue("Favorable\nto {country_name}"),
        "favors 2" = "Favorable to the\nother country",
        "country" = country_name
      ),
      na.value = "white"
    ) +
    guides(
      fill = guide_legend(title = "Visa reciprocity")
    ) +
    coord_sf(crs = "+proj=robin") +
    theme_void(
      base_family = "Atkinson Hyperlegible",
      base_size = 16
    ) +
    theme(
      legend.position = "bottom"
    )

  annot <- ggplot() +
    annotate(
      geom = "text_box",
      x = 1,
      y = 1,
      label = glue::glue("The map on the top shows where you will need a visa (or perhaps are barred from entry) if you have a passport from **{country_name}**. Whereas, the map on the bottom shows you whether the visa requirements are the same (or equivalent) between **{country_name}** and another country, or if the situation favors **{country_name}** (e.g. *no need for a visa*), or favors the other country (*you need a visa to go to their country, but they do not need one to come to yours*)"),
      family = "Atkinson Hyperlegible",
      size = 9,
      width = unit(12, "cm"),
      box.padding = unit(rep(.5, 4), "cm")
    ) +
    theme_void()

layout <- "
AAAAAACCC
AAAAAACCC
BBBBBBCCC
BBBBBBCCC
"

  (p1 + p2 + annot ) +
   plot_layout(design = layout) +
    plot_annotation(
      title = glue::glue("Visa requirements and reciprocity if you have a passport from {country_name}"),
      subtitle = "Data source: 2023 Passport Index Dataset (https://github.com/ilyankou/passport-index-dataset)",
      caption = "@jmcastagnetto@mastodon.social, Jesus M. Castagnetto"
    ) &
    theme(
      plot.title.position = "plot",
      plot.title = element_text(family = "Atkinson Hyperlegible", size = 32),
      plot.subtitle = element_text(family = "Atkinson Hyperlegible",
                                   size = 20, color = "grey40"),
      plot.caption = element_text(family = "Inconsolata", size = 14),
      plot.margin = unit(rep(1, 4), "cm"),
      plot.background = element_rect(fill = "white", color = "white")
    )

}


ggsave(
  plot = mk_visa_map(world, visa_df, "PER"),
  filename = "passports-index/peru-visa-symmetry.png",
  width = 18,
  height = 14
)

ggsave(
  plot = mk_visa_map(world, visa_df, "USA"),
  filename = "passports-index/usa-visa-symmetry.png",
  width = 18,
  height = 14
)

ggsave(
  plot = mk_visa_map(world, visa_df, "ISR"),
  filename = "passports-index/israel-visa-symmetry.png",
  width = 18,
  height = 14
)

ggsave(
  plot = mk_visa_map(world, visa_df, "ARG"),
  filename = "passports-index/argentina-visa-symmetry.png",
  width = 18,
  height = 14
)

