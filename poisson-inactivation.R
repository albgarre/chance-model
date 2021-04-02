
library(tidyverse)

D <- 2
N0 <- 1e6

niter <- 100

## Bigelow

p1 <- tibble(t = seq(0, 14, length = 8)) %>%
    mutate(p = 10^(-t/D),
           lambda = N0*p) %>%
    # ggplot() +
    geom_line(aes(x = t, y = lambda), data = ., colour = "blue")
        # scale_y_log10()

p <- tibble(t = seq(0, 14, length = 8)) %>%
    mutate(p = 10^(-t/D),
           lambda = N0*p) %>%
    split(.$t) %>%
    map(., ~ rpois(niter, lambda = .$lambda)) %>%
    imap_dfr(., ~ tibble(t = as.numeric(.y), N = .x)) %>%
    ggplot() +
        geom_point(aes(x = t, y = N)) +
        scale_y_log10()
p + p1

## Beta = 2

p1 <- tibble(t = seq(0, 4.5, length = 12)) %>%
    mutate(p = 10^(-(t/D)^2),
           lambda = N0*p) %>%
    # ggplot() +
    geom_line(aes(x = t, y = lambda), data = ., colour = "blue")
# scale_y_log10()

p <- tibble(t = seq(0, 4.5, length = 12)) %>%
    mutate(p = 10^(-(t/D)^2),
           lambda = N0*p) %>%
    split(.$t) %>%
    map(., ~ rpois(niter, lambda = .$lambda)) %>%
    imap_dfr(., ~ tibble(t = as.numeric(.y), N = .x)) %>%
    ggplot() +
    geom_point(aes(x = t, y = N)) +
    scale_y_log10()

p + p1

## Beta = 0.5

p1 <- tibble(t = seq(0, 75, length = 12)) %>%
    mutate(p = 10^(-(t/D)^.5),
           lambda = N0*p) %>%
    # ggplot() +
    geom_line(aes(x = t, y = lambda), data = ., colour = "blue")
# scale_y_log10()

p <- tibble(t = seq(0, 75, length = 12)) %>%
    mutate(p = 10^(-(t/D)^.5),
           lambda = N0*p) %>%
    split(.$t) %>%
    map(., ~ rpois(niter, lambda = .$lambda)) %>%
    imap_dfr(., ~ tibble(t = as.numeric(.y), N = .x)) %>%
    ggplot() +
    geom_point(aes(x = t, y = N)) +
    scale_y_log10()

p + p1


## pmf

tibble(D = c(1.5, 2, 2.5),
       t = 15) %>%
    mutate(p = 10^(-t/D),
           lambda = N0*p) %>%
    split(.$D) %>%
    map_dfr(., ~ tibble(D = .$D, N = 0:10, P = dpois(N, lambda = .$lambda))) %>%
    ggplot() +
        geom_col(aes(x = N, y = P, fill = factor(D)), position = "dodge")































