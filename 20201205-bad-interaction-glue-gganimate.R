library(tidyverse)
library(gganimate)
library(palmerpenguins)
data(penguins)

p0 <- ggplot(
  penguins %>%
    mutate(
      sex = as.character(sex) %>% replace_na("unknown"),
      coolness = as.factor(sample(1:5, nrow(penguins), replace = TRUE))
    ) %>%
    filter(!is.na(flipper_length_mm)),
  aes(x = bill_length_mm, y = flipper_length_mm,
      group = species, color = coolness)
) +
  labs(
    title = "Palmer Penguines dataset - Species: {closest_state}",
    subtitle = paste0(
      "Island(s): ",
      origin = paste(unique(penguins$island), collapse = ", ")
    )
  ) +
  facet_wrap(~sex)


# This code works ---------------------------------------------------------
p1 <- p0 +
  labs(caption = "No glue") +
  geom_text(aes(label = paste0("[", coolness, "]")), show.legend = FALSE)

anim1 <- p1 +
  transition_states(
    states = species
  )

animate(
  anim1,
  width = 600,
  height = 300,
  nframes = 30,
  fps = 10
)

# This code generates an error --------------------------------------------
p2 <- p0 +
  labs(caption = "With glue") +
  geom_text(aes(label = glue::glue("[{coolness}]")), show.legend = FALSE)

anim2 <- p2 +
  transition_states(
    states = species
  )

animate(
  anim2,
  width = 900,
  height = 300,
  nframes = 30,
  fps = 10
)
