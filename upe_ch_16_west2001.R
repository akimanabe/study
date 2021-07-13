library(tidyverse)
dat <-
  readr::read_csv("/home/rstudio/data/study/west_data.csv") %>%
  dplyr::select(1, 2, 3) %>%
  dplyr::rename(Mass = 2,
                Species = 3) %>%
  dplyr::mutate(Species = factor(Species))

ggplot(dat) +
  geom_point(aes(x = Time, y = Mass, color = Species)) +
  geom_line(aes(x = Time, y = Mass, color = Species)) +
  scale_x_log10() +
  scale_y_log10() +
  theme_minimal(base_size = 16) +
  theme(legend.position = "none")
