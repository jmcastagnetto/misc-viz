# Source: https://www.visualcapitalist.com/the-most-used-languages-on-the-internet/
# 2021-04-01
library(tidyverse)


lang <- read_csv("data/languages-internet-spoken.csv")
cnames <- colnames(lang)
lang <- lang %>%
  janitor::clean_names() %>%
  mutate(
    rank_spoken = row_number(desc(share_of_speaking_population)),
    percent_of_top_10m_websites = percent_of_top_10m_websites / 100,
    share_of_speaking_population = share_of_speaking_population / 100
  )


# Bidirectional bar chart -------------------------------------------------

bar_df <- lang %>%
  select(2:4) %>%
  mutate(
    percent_of_top_10m_websites = -1 * percent_of_top_10m_websites,
    language = fct_reorder(language, percent_of_top_10m_websites, .desc = TRUE),
  ) %>%
  pivot_longer(
    cols = 2:3,
    names_to = "metric",
    values_to = "percent"
  )

p1 <- ggplot(
  bar_df,
  aes(x = percent, y = language)
) +
  geom_col(aes(fill = metric), show.legend = FALSE) +
  geom_label(
    data = bar_df %>% filter(metric == "percent_of_top_10m_websites"),
    aes(label = sprintf("%.1f%%", -100*percent)),
    hjust = 1,
    nudge_x = -.01,
    size = 5,
    label.size = 0
  ) +
  geom_label(
    data = bar_df %>% filter(metric != "percent_of_top_10m_websites"),
    aes(label = sprintf("%.1f%%", 100*percent)),
    hjust = 0,
    nudge_x = .01,
    size = 5,
    label.size = 0
  ) +
  annotate(
    geom = "label",
    x = -.6,
    y = 12,
    label = "Percent of top\n10M websites\nusing a particular\nlanguage",
    size = 14,
    hjust = 0,
    label.size = 0,
    color = "#e41a1c",
    alpha = .7
  ) +
  annotate(
    geom = "label",
    x = .21,
    y = 12,
    label = "Percent of\npeople\nspeaking\na language",
    size = 14,
    hjust = 0,
    label.size = 0,
    color = "#377eb8",
    alpha = .7
  ) +
  scale_x_continuous(
    breaks = c(-.6, -.4, -.2, 0, .2),
#    labels = c("60%", "40%", "20%", "0", "20%"), # no need for this
    limits = c(-.65, .5)
  ) +
  scale_fill_manual(
    values = c("#e41a1c", "#377eb8")
  ) +
  labs(
    x = "",
    y = "",
    title = "Visualizing the most used languages on the Internet",
    subtitle = "A reimagining of the graph at: https://www.visualcapitalist.com/the-most-used-languages-on-the-internet/",
    caption = "@jmcastagnetto, Jesus M. Castagnetto, 2021-04-01"
  ) +
  ggthemes::theme_tufte(
    base_size = 20,
    base_family = "Roboto"
  ) +
  theme(
    plot.margin = unit(rep(.5, 4), "cm"),
    plot.title.position = "plot",
    plot.subtitle = element_text(color = "grey50"),
    axis.text.x = element_blank(),
    plot.caption = element_text(family = "Inconsolata"),
    panel.grid.major.y = element_line(linetype = "dashed", size = .4, color = "gray70")
  )

ggsave(
  plot = p1,
  filename = "lang-internet-spoken.R/lang-comparison-bars.png",
  width = 16,
  height = 10
)


# Parallel coords plot ----------------------------------------------------

slope_df <- lang %>%
  mutate(
    lbl_internet = glue::glue("{rank}. {language} ({sprintf('%.1f%%', 100 * percent_of_top_10m_websites)})"),
    lbl_spoken = glue::glue("{rank_spoken}. {language} ({sprintf('%.1f%%', 100 * share_of_speaking_population)})"),
    type = if_else(
      rank == rank_spoken,
      "same",
      if_else(
        rank > rank_spoken,
        "more",
        "less"
      )
    )
  ) %>%
  select(2, lbl_internet, lbl_spoken, type, 1, 5) %>%
  pivot_longer(
    cols = 5:6,
    names_to = "metric",
    values_to = "value"
  )

p2 <- ggplot(
  slope_df,
  aes(x = metric, y = value, color = type)
) +
  annotate(
    geom = "segment",
    x = 1,
    xend = 1,
    y = -1,
    yend = 20,
    color = "grey70",
    linetype = "dashed"
  ) +
  annotate(
    geom = "segment",
    x = 2,
    xend = 2,
    y = -1,
    yend = 20,
    color = "grey70",
    linetype = "dashed"
  ) +
  geom_line(aes(group = language), size = 2, alpha = .5, show.legend = FALSE) +
  geom_point(show.legend = FALSE, size = 6) +
  geom_label(
    data = slope_df %>%
      filter(metric == "rank") %>%
      select(metric, value, type, lbl_internet) %>%
      distinct(),
    aes(label = lbl_internet),
    size = 6,
    label.size = 0,
    hjust = 1,
    nudge_x = -.04,
    show.legend = FALSE
    #color = "black"
  ) +
  geom_label(
    data = slope_df %>%
      filter(metric == "rank_spoken") %>%
      select(metric, value, type, lbl_spoken) %>%
      distinct(),
    aes(label = lbl_spoken),
    size = 6,
    label.size = 0,
    hjust = 0,
    nudge_x = .04,
    show.legend = FALSE
    #color = "black"
  ) +
  annotate(
    geom = "label",
    label = "Ranking of\nlanguages for\nthe top 10M sites",
    vjust = 1,
    x = 1,
    y = -4,
    size = 8,
    fontface = "bold",
    label.size = 0,
    alpha = .7
  ) +
  annotate(
    geom = "label",
    label ="Ranking of\nspoken languages\nin the world",
    vjust = 1,
    x = 2,
    y = -4,
    size = 8,
    fontface = "bold",
    label.size = 0,
    alpha = .7
  ) +
  scale_x_discrete(expand = expansion(add = 1)) +
  scale_y_reverse()+
  scale_color_manual(
    values = c(
      "same" = "grey50",
      "more" = "#e41a1c",
      "less" = "#377eb8"
    )
  ) +
  labs(
    title = "Visualizing the most used languages on the Internet",
    subtitle = "A reimagining of the graph at: https://www.visualcapitalist.com/the-most-used-languages-on-the-internet/",
    caption = "@jmcastagnetto, Jesus M. Castagnetto, 2021-04-01"
  ) +
  theme_minimal(base_size = 20, base_family = "Roboto") +
  theme(

    plot.title.position = "panel",
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    plot.subtitle = element_text(size = 17, color = "gray50"),
    plot.caption = element_text(family = "Inconsolata")
  )

ggsave(
  plot = p2,
  filename = "lang-internet-spoken.R/lang-comparison-parallel.png",
  width = 12,
  height = 14
)

