# logistic

logi <- function(a, b, m) {
  a*m - b*(m^2)
}

vb   <- function(a, b, m, k) {
  a*m^k - b*m
}

gomp <- function(a, b, m) {
  a*m-b*m*log(m)
}

a <- 1
b <- 1.5
#instantaneous growth rate
tibble::tibble(m = seq(0, 2, by = 0.01), a = 0.75, b = 0.66, k = 0.75) %>%
  dplyr::mutate(logistic = logi(a, b, m),
                bertalanffy = vb(a, b, m, k),
                gompertz = gomp(a, b, m)) %>%
  tidyr::pivot_longer(cols = c(logistic, bertalanffy, gompertz), names_to = "model", values_to = "dmdt") %>%
  ggplot() +
  geom_line(aes(m, dmdt, color = model)) +
  coord_cartesian(ylim = c(0, 1))

#cumsum
tibble::tibble(m = seq(0.1, 5, by = 0.1), a = 1, b = 0.75, k = 0.75) %>%
  dplyr::mutate(logistic = logi(a, b, m),
                bertalanffy = vb(a, b, m, k),
                gompertz = gomp(a, b, m)) %>%
  dplyr::mutate(logistic = dplyr::if_else(logistic < 0, 0, logistic),
                bertalanffy = dplyr::if_else(bertalanffy < 0, 0, bertalanffy),
                gompertz = dplyr::if_else(gompertz < 0, 0, gompertz)) %>%
  dplyr::mutate(logistic = cumsum(logistic),
                bertalanffy = cumsum(bertalanffy),
                gompertz = cumsum(gompertz)) %>% 
  tidyr::pivot_longer(cols = c(logistic, bertalanffy, gompertz), names_to = "model", values_to = "dmdt") %>%
  ggplot() +
  geom_line(aes(m, dmdt, color = model))

library(fishgr)
