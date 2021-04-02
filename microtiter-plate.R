
library(tidyverse)
library(cowplot)

## Model parameters

D <- 5
max_t <- 20

## Probability of zero count (Figure 3)

beta <- 1

p1 <- tibble(N0 = 10^(1:6)) %>%
    split(.$N0) %>%
    map(.,
        ~ tibble(N0 = .$N0,
                 t = seq(0, 50, length = 200))
    ) %>%
    map(.,
        ~ mutate(., 
                 p = 10^(-(t/D)^beta),
                 lambda = p*N0)
    ) %>%
    map_dfr(.,
            ~ mutate(., 
                     chance = dbinom(0, size = N0, prob = p),
                     `chance + N0` = dpois(0, lambda = lambda))
    ) %>%
    select(N0, t, chance, `chance + N0`) %>%
    pivot_longer(-c(N0, t), names_to = "Source of variation", values_to = "p") %>%
    mutate(`Initial count (log CFU/ml)` = factor(log10(N0))) %>%
    ggplot() +
    geom_line(aes(x = t, y = p, 
                  linetype = `Initial count (log CFU/ml)`,
                  size = `Source of variation`,
                  colour = `Source of variation`)) +
    # geom_vline(xintercept = 6*D, linetype = 2) +
    ylab("Probability of zero count") + xlab("Treatment time (min)") +
    theme_cowplot() +
    scale_size_manual(values = c(2.5, 1), label = c("chance", bquote("chance +" ~ N[0]))) +
    scale_color_discrete(label = c("chance", bquote("chance +" ~ N[0])))

p1 
beta <- 2

p2 <- tibble(N0 = 10^(1:6)) %>%
    split(.$N0) %>%
    map(.,
        ~ tibble(N0 = .$N0,
                 t = seq(0, 20, length = 200))
    ) %>%
    map(.,
        ~ mutate(., 
                 p = 10^(-(t/D)^beta),
                 lambda = p*N0)
    ) %>%
    map_dfr(.,
            ~ mutate(., 
                     chance = dbinom(0, size = N0, prob = p),
                     `chance + N0` = dpois(0, lambda = lambda))
    ) %>%
    select(N0, t, chance, `chance + N0`) %>%
    pivot_longer(-c(N0, t), names_to = "Source of variation", values_to = "p") %>%
    mutate(`Initial count (log CFU/ml)` = factor(log10(N0))) %>%
    ggplot() +
    geom_line(aes(x = t, y = p, 
                  linetype = `Initial count (log CFU/ml)`,
                  size = `Source of variation`,
                  colour = `Source of variation`)) +
    # geom_vline(xintercept = 6*D, linetype = 2) +
    ylab("Probability of zero count") + xlab("Treatment time (min)") +
    theme_cowplot() +
    scale_size_manual(values = c(2.5, 1), label = c("chance", bquote("chance +" ~ N[0]))) +
    scale_color_discrete(label = c("chance", bquote("chance +" ~ N[0])))


beta <- .6

p3 <- tibble(N0 = 10^(1:6)) %>%
    split(.$N0) %>%
    map(.,
        ~ tibble(N0 = .$N0,
                 t = seq(0, 150, length = 200))
    ) %>%
    map(.,
        ~ mutate(., 
                 p = 10^(-(t/D)^beta),
                 lambda = p*N0)
    ) %>%
    map_dfr(.,
            ~ mutate(., 
                     chance = dbinom(0, size = N0, prob = p),
                     `chance + N0` = dpois(0, lambda = lambda))
    ) %>%
    select(N0, t, chance, `chance + N0`) %>%
    pivot_longer(-c(N0, t), names_to = "Source of variation", values_to = "p") %>%
    mutate(`Initial count (log CFU/ml)` = factor(log10(N0))) %>%
    ggplot() +
    geom_line(aes(x = t, y = p, 
                  linetype = `Initial count (log CFU/ml)`,
                  size = `Source of variation`,
                  colour = `Source of variation`)) +
    # geom_vline(xintercept = 6*D, linetype = 2) +
    ylab("Probability of zero count") + xlab("Treatment time (min)") +
    theme_cowplot() +
    scale_size_manual(values = c(2.5, 1), label = c("chance", bquote("chance +" ~ N[0]))) +
    scale_color_discrete(label = c("chance", bquote("chance +" ~ N[0])))


plot_grid(p1, p2, p3, labels = "AUTO", ncol = 1)

## Probability distribution (Figure 2)

# N0 <- 1e6
# beta <- 1
# 
# tibble(t = c(5, 10, 20, 25, 30, 35)) %>%
#     mutate(p = 10^(-(t/D)^beta),
#            lambda = N0*p) %>%
#     split(.$t) %>%
#     map(.,
#         ~ tibble(N = round(.$lambda-1000):round(.$lambda + 1000),
#                  chance = dbinom(N, size = N0, prob = .$p),
#                  `chance + N0` = dpois(N, lambda = .$lambda))
#     ) %>%
#     imap_dfr(.,
#              ~ mutate(.x, t = as.numeric(.y))
#              ) %>%
#     pivot_longer(-c(N, t), names_to = "Source of variation", values_to = "p") %>%
#     filter(p > 1e-6) %>%
#     mutate(t = paste0("t = ", t, " min")) %>%
#     mutate(t = factor(t, levels = c("t = 5 min", "t = 10 min", "t = 20 min",
#                                     "t = 25 min", "t = 30 min", "t = 35 min"))) %>%
#     ggplot() +
#         # geom_line(aes(x = N, y = p,
#         #               colour = `Source of variation`,
#         #               size = `Source of variation`)) +
#         geom_point(aes(x = N, y = p, colour = `Source of variation`,
#                        shape = `Source of variation`,
#                        size = `Source of variation`)) +
#         facet_wrap(~t, scales = "free") +
#         theme_bw() +
#         xlab("Microbial count (CFU)") + ylab("Probability mass function") +
#         theme(legend.position = "top",
#               axis.title = element_text(size = 16),
#               axis.text = element_text(size = 14),
#               strip.text = element_text(size = 14),
#               # legend.title = element_text(size = 16),
#               legend.title = element_blank(),
#               legend.text = element_text(size = 14)
#               ) +
#     scale_size_manual(values = c(3, 1)) +
#     scale_shape_manual(values = c(1, 20))

N0 <- 1e6
beta <- 1

aa <- tibble(t = c(5, 10)) %>%
    mutate(p = 10^(-(t/D)^beta),
           lambda = N0*p) %>%
    split(.$t) %>%
    map(.,
        ~ tibble(N = round(.$lambda-1000):round(.$lambda + 1000),
                 chance = dbinom(N, size = N0, prob = .$p),
                 `chance + N0` = dpois(N, lambda = .$lambda))
    ) %>%
    imap_dfr(.,
             ~ mutate(.x, t = as.numeric(.y))
             ) %>%
    pivot_longer(-c(N, t), names_to = "Source of variation", values_to = "p_line") %>%
    filter(p_line > 1e-6) %>%
    mutate(t = paste0("t = ", t, " min")) %>%
    mutate(t = factor(t, levels = c("t = 5 min", "t = 10 min", "t = 20 min",
                                    "t = 25 min", "t = 30 min", "t = 35 min")))

# tibble(t = c(20, 25, 30, 35)) %>%
#     mutate(p = 10^(-(t/D)^beta),
#            lambda = N0*p) %>%
#     split(.$t) %>%
#     map(.,
#         ~ tibble(N = round(.$lambda-1000):round(.$lambda + 1000),
#                  chance = dbinom(N, size = N0, prob = .$p),
#                  `chance + N0` = dpois(N, lambda = .$lambda))
#     ) %>%
#     imap_dfr(.,
#              ~ mutate(.x, t = as.numeric(.y))
#              ) %>%
#     pivot_longer(-c(N, t), names_to = "Source of variation", values_to = "p") %>%
#     filter(p > 1e-6) %>%
#     mutate(t = paste0("t = ", t, " min")) %>%
#     mutate(t = factor(t, levels = c("t = 5 min", "t = 10 min", "t = 20 min",
#                                     "t = 25 min", "t = 30 min", "t = 35 min"))) %>%
#     bind_rows(., aa) %>%
#     ggplot() +
#     geom_point(aes(x = N, y = p, colour = `Source of variation`,
#                    shape = `Source of variation`,
#                    size = `Source of variation`)) +
#     geom_line(aes(x = N, y = p_line, colour = `Source of variation`,
#                   size = `Source of variation`)) +
#     facet_wrap(~t, scales = "free", nrow = 3) +
#     theme_bw() +
#     xlab("Microbial count (CFU)") +
#     ylab("Probability mass function") +
#     theme(legend.position = "top",
#           axis.title = element_text(size = 16),
#           axis.text = element_text(size = 14),
#           strip.text = element_text(size = 14),
#           # legend.title = element_text(size = 16),
#           legend.title = element_blank(),
#           legend.text = element_text(size = 14)
#     ) +
#     scale_size_manual(values = c(3, 1)) +
#     scale_shape_manual(values = c(1, 20))

fancy_scientific <- function(l) {
    # turn in to character string in scientific notation
    l <- format(l, scientific = TRUE)
    # quote the part before the exponent to keep all the digits
    l <- gsub("^(.*)e", "'\\1'e", l)
    # turn the 'e+' into plotmath format
    l <- gsub("e", "%*%10^", l)
    # return this as an expression
    parse(text=l)
}

tibble(t = c(20, 25, 30, 35)) %>%
    mutate(p = 10^(-(t/D)^beta),
           lambda = N0*p) %>%
    split(.$t) %>%
    map(.,
        ~ tibble(N = round(.$lambda-1000):round(.$lambda + 1000),
                 chance = dbinom(N, size = N0, prob = .$p),
                 `chance + N0` = dpois(N, lambda = .$lambda))
    ) %>%
    imap_dfr(.,
             ~ mutate(.x, t = as.numeric(.y))
    ) %>%
    pivot_longer(-c(N, t), names_to = "Source of variation", values_to = "p") %>%
    filter(p > 1e-6) %>%
    mutate(t = paste0("t = ", t, " min")) %>%
    mutate(t = factor(t, levels = c("t = 5 min", "t = 10 min", "t = 20 min",
                                    "t = 25 min", "t = 30 min", "t = 35 min"))) %>%
    bind_rows(., aa) %>%
    split(.$t) %>%
    map(.,
        ~ ggplot(.) +
            geom_point(aes(x = N, y = p, colour = `Source of variation`,
                           shape = `Source of variation`,
                           size = `Source of variation`)) +
            geom_line(aes(x = N, y = p_line, colour = `Source of variation`,
                          size = `Source of variation`)) +
            facet_wrap(~t, scales = "free", nrow = 3) +
            theme_bw() +
            # xlab("Microbial count (CFU)") +
            ylab("Probability mass") +
            theme(legend.position = "none",
                  axis.title = element_text(size = 16),
                  axis.text = element_text(size = 14),
                  strip.text = element_text(size = 14),
                  # legend.title = element_text(size = 16),
                  legend.title = element_blank(),
                  legend.text = element_text(size = 14)
            ) +
            scale_size_manual(values = c(3, 1)) +
            scale_shape_manual(values = c(1, 20))
        ) %>%
    map2(., list(
        waiver(),
        waiver(),
        waiver(),
        waiver(),
        c(0, 2, 4, 6, 8),
        waiver()
        ),
        ~ .x + scale_x_continuous(name = "Microbial count (CFU)",
                                  breaks = .y)
        ) %>%
    plot_grid(plotlist = ., ncol = 2)
    
## Plot of survivors (Figure 4)

fancy_scientific <- function(l) {
    # turn in to character string in scientific notation
    l <- format(l, scientific = TRUE)
    # quote the part before the exponent to keep all the digits
    l <- gsub("^(.*)e", "'\\1'e", l)
    # turn the 'e+' into plotmath format
    l <- gsub("e", "%*%10^", l)
    # return this as an expression
    parse(text=l)
}

N0 <- 1e6
beta <- 1

set.seed(14212)

p1 <- tibble(t = seq(0, 40, length = 20)) %>%
    mutate(p = 10^(-(t/D)^beta),
           lambda = N0*p) %>%
    split(.$t) %>%
    map_dfr(.,
            ~ tibble(N = rbinom(1000, size = N0, prob = .$p),
                     t = .$t)
    ) %>%
    mutate(logN_teor = log10(N0) - (t/D)^beta) %>%
    ggplot(aes(x = t, y = N)) +
    geom_point(shape = 1) +
    # geom_smooth(se = FALSE) +
    geom_line(aes(x = t, y = 10^logN_teor)) +
    # scale_y_log10() +
    scale_y_log10(breaks = c(1e0, 1e2, 1e4, 1e6),
                  labels = fancy_scientific(c(1e0, 1e2, 1e4, 1e6))) +
    coord_cartesian(ylim = c(1e0, 1e6)) +
    xlab("Treatment time (min)") + ylab("Microbial count (CFU)") +
    theme_cowplot()

p1
beta <- 2

set.seed(14212)

p2 <- tibble(t = seq(0, 14, length = 20)) %>%
    mutate(p = 10^(-(t/D)^beta),
           lambda = N0*p) %>%
    split(.$t) %>%
    map_dfr(.,
            ~ tibble(N = rbinom(1000, size = N0, prob = .$p),
                     t = .$t)
    ) %>%
    mutate(logN_teor = log10(N0) - (t/D)^beta) %>%
    ggplot(aes(x = t, y = N)) +
    geom_point(shape = 1) +
    # geom_smooth(se = FALSE) +
    geom_line(aes(x = t, y = 10^logN_teor)) +
    # scale_y_log10() +
    scale_y_log10(breaks = c(1e0, 1e2, 1e4, 1e6),
                  labels = fancy_scientific(c(1e0, 1e2, 1e4, 1e6))) +
    coord_cartesian(ylim = c(1e0, 1e6)) +
    coord_cartesian(ylim = c(1e0, 1e6))  +
    xlab("Treatment time (min)") + ylab("Microbial count (CFU)") +
    theme_cowplot()

beta <- .6

set.seed(14212)

p3 <- tibble(t = seq(0, 120, length = 20)) %>%
    mutate(p = 10^(-(t/D)^beta),
           lambda = N0*p) %>%
    split(.$t) %>%
    map_dfr(.,
            ~ tibble(N = rbinom(1000, size = N0, prob = .$p),
                     t = .$t)
    ) %>%
    mutate(logN_teor = log10(N0) - (t/D)^beta) %>%
    ggplot(aes(x = t, y = N)) +
    geom_point(shape = 1) +
    geom_line(aes(x = t, y = 10^logN_teor)) +
    # geom_smooth(se = FALSE) +
    # scale_y_log10() +
    scale_y_log10(breaks = c(1e0, 1e2, 1e4, 1e6),
                  labels = fancy_scientific(c(1e0, 1e2, 1e4, 1e6))) +
    coord_cartesian(ylim = c(1e0, 1e6)) +
    coord_cartesian(ylim = c(1e0, 1e6))  +
    xlab("Treatment time (min)") + ylab("Microbial count (CFU)") +
    theme_cowplot()

plot_grid(p1, p2, p3, labels = "AUTO", nrow = 1)

## Chance + count + dilutions (Figure 5)

N0 <- 1e6
beta <- 1

set.seed(14212)

p1 <- tibble(t = seq(0, 40, length = 20)) %>%
    mutate(p = 10^(-(t/D)^beta),
           lambda = N0*p) %>%
    split(.$t) %>%
    map_dfr(.,
            ~ tibble(plate_d0 = rpois(1000, lambda = .$lambda),
                     plate_d1 = rpois(1000, lambda = .$lambda*1e-1),
                     plate_d2 = rpois(1000, lambda = .$lambda*1e-2),
                     plate_d3 = rpois(1000, lambda = .$lambda*1e-3),
                     plate_d4 = rpois(1000, lambda = .$lambda*1e-4),
                     t = .$t)
    ) %>%
    gather(dil, count, -t) %>%
    separate(dil, into = c("foo", "dil"), sep = "_d") %>%
    mutate(dil = as.numeric(dil),
           N = count/.1^dil) %>%
    select(-foo) %>%
    filter(count < 300) %>%
    filter(count > 30|dil==0) %>%
    mutate(logN_teor = log10(N0) - (t/D)^beta) %>%
    # mutate(dil = paste0("-", dil)) %>%
    mutate(`Dilutions` = factor(dil)) %>%
    ggplot() +
        geom_point(aes(x = t, y = N, colour = `Dilutions`), shape = 1) +
        geom_line(aes(x = t, y = 10^logN_teor)) +
    scale_y_log10(breaks = c(1e0, 1e2, 1e4, 1e6),
                  labels = fancy_scientific(c(1e0, 1e2, 1e4, 1e6))) +
        coord_cartesian(ylim = c(1e0, 1e6))  +
        xlab("Treatment time (min)") + ylab("Microbial count (CFU)") +
        theme_cowplot() +
        theme(legend.position = "top")

N0 <- 1e6
beta <- 2

set.seed(14212)

p2 <- tibble(t = seq(0, 14, length = 20)) %>%
    mutate(p = 10^(-(t/D)^beta),
           lambda = N0*p) %>%
    split(.$t) %>%
    map_dfr(.,
            ~ tibble(plate_d0 = rpois(1000, lambda = .$lambda),
                     plate_d1 = rpois(1000, lambda = .$lambda*1e-1),
                     plate_d2 = rpois(1000, lambda = .$lambda*1e-2),
                     plate_d3 = rpois(1000, lambda = .$lambda*1e-3),
                     plate_d4 = rpois(1000, lambda = .$lambda*1e-4),
                     t = .$t)
    ) %>%
    gather(dil, count, -t) %>%
    separate(dil, into = c("foo", "dil"), sep = "_d") %>%
    mutate(dil = as.numeric(dil),
           N = count/.1^dil) %>%
    select(-foo) %>%
    filter(count < 300) %>%
    filter(count > 30|dil==0) %>%
    mutate(logN_teor = log10(N0) - (t/D)^beta) %>%
    # mutate(dil = paste0("-", dil)) %>%
    mutate(`Dilutions` = factor(dil)) %>%
    ggplot() +
    geom_point(aes(x = t, y = N, colour = `Dilutions`), shape = 1) +
    geom_line(aes(x = t, y = 10^logN_teor)) +
    scale_y_log10(breaks = c(1e0, 1e2, 1e4, 1e6),
                  labels = fancy_scientific(c(1e0, 1e2, 1e4, 1e6))) +
    coord_cartesian(ylim = c(1e0, 1e6))  +
    xlab("Treatment time (min)") + ylab("Microbial count (CFU)") +
    theme_cowplot() +
    theme(legend.position = "top")

N0 <- 1e6
beta <- .6

set.seed(14212)

p3 <- tibble(t = seq(0, 120, length = 20)) %>%
    mutate(p = 10^(-(t/D)^beta),
           lambda = N0*p) %>%
    split(.$t) %>%
    map_dfr(.,
            ~ tibble(plate_d0 = rpois(1000, lambda = .$lambda),
                     plate_d1 = rpois(1000, lambda = .$lambda*1e-1),
                     plate_d2 = rpois(1000, lambda = .$lambda*1e-2),
                     plate_d3 = rpois(1000, lambda = .$lambda*1e-3),
                     plate_d4 = rpois(1000, lambda = .$lambda*1e-4),
                     t = .$t)
    ) %>%
    gather(dil, count, -t) %>%
    separate(dil, into = c("foo", "dil"), sep = "_d") %>%
    mutate(dil = as.numeric(dil),
           N = count/.1^dil) %>%
    select(-foo) %>%
    filter(count < 300) %>%
    filter(count > 30|dil==0) %>%
    mutate(logN_teor = log10(N0) - (t/D)^beta) %>%
    # mutate(dil = paste0("-", dil)) %>%
    mutate(`Dilutions` = factor(dil)) %>%
    ggplot() +
    geom_point(aes(x = t, y = N, colour = `Dilutions`), shape = 1) +
    geom_line(aes(x = t, y = 10^logN_teor)) +
    scale_y_log10(breaks = c(1e0, 1e2, 1e4, 1e6),
                  labels = fancy_scientific(c(1e0, 1e2, 1e4, 1e6))) +
    coord_cartesian(ylim = c(1e0, 1e6))  +
    xlab("Treatment time (min)") + ylab("Microbial count (CFU)") +
    theme_cowplot() +
    theme(legend.position = "top")

plot_grid(p1, p2, p3, labels = "AUTO", nrow = 1)
    
## Table 1 

N0 <- 1e6
beta <- 1

#- chance / chance + N0

tibble(t = c(0, 5, 10, 15, 20, 25, 30, 35, 40)) %>%
    mutate(p = 10^(-(t/D)^beta),
           lambda = N0*p) %>%
    mutate(sigma_chance = sqrt(N0*p*(1-p)),
           log_sigma_chance = log10(sigma_chance),
           # log_lambda = log10(lambda),
           sqrt(lambda),
           log10(sqrt(lambda)),
           p0_chance = dbinom(0, size = N0, prob = p),
           p0_chance_pois = dpois(0, lambda = lambda)) %>%
    select(-p)

#- chance + N0 + dilution

set.seed(14212)

tibble(t = c(0, 5, 10, 15, 20, 25, 30, 35, 40)) %>%
    mutate(p = 10^(-(t/D)^beta),
           lambda = N0*p) %>%
    split(.$t) %>%
    map_dfr(.,
            ~ tibble(plate_d0 = rpois(80000, lambda = .$lambda),
                     plate_d1 = rpois(80000, lambda = .$lambda*1e-1),
                     plate_d2 = rpois(80000, lambda = .$lambda*1e-2),
                     plate_d3 = rpois(80000, lambda = .$lambda*1e-3),
                     plate_d4 = rpois(80000, lambda = .$lambda*1e-4),
                     t = .$t)
    ) %>%
    gather(dil, count, -t) %>%
    separate(dil, into = c("foo", "dil"), sep = "_d") %>%
    mutate(dil = as.numeric(dil),
           N = count/.1^dil) %>%
    select(-foo) %>%
    filter(count < 300) %>%
    filter(count > 30|dil==0) %>%
    mutate(logN_teor = log10(N0) - (t/D)^beta) %>%
    group_by(t) %>%
    summarize(sigma_N = sd(N),
              lambda = mean(N),
              p0 = mean(N == 0)
              ) %>%
    mutate(log10(lambda),
           log10(sigma_N))
    
    
## Error of the log-count

N0 <- 1e6
beta <- 1

tibble(t = c(0, 5, 10, 15, 20, 25, 30, 35, 40)) %>%
    mutate(p = 10^(-(t/D)^beta),
           lambda = N0*p) %>%
    split(.$t) %>%
    map_dfr(.,
            ~ tibble(plate_d0 = rpois(100, lambda = .$lambda),
                     plate_d1 = rpois(100, lambda = .$lambda*1e-1),
                     plate_d2 = rpois(100, lambda = .$lambda*1e-2),
                     plate_d3 = rpois(100, lambda = .$lambda*1e-3),
                     plate_d4 = rpois(100, lambda = .$lambda*1e-4),
                     t = .$t)
    ) %>%
    gather(dil, count, -t) %>%
    separate(dil, into = c("foo", "dil"), sep = "_d") %>%
    mutate(dil = as.numeric(dil),
           N = count/.1^dil) %>%
    select(-foo) %>%
    filter(count < 300) %>%
    filter(count > 30|dil==0) %>%
    mutate(logN_teor = log10(N0) - (t/D)^beta) %>%
    mutate(logN = log10(N)) %>%
    group_by(t) %>%
    summarize(mean(logN), sd(logN))




## sup Table 1 

N0 <- 1e6
beta <- 2

#- chance / chance + N0

tibble(t = c(2, 4, 6, 8, 10, 12, 14)) %>%
    mutate(p = 10^(-(t/D)^beta),
           lambda = N0*p) %>%
    mutate(sigma_chance = sqrt(N0*p*(1-p)),
           log_sigma_chance = log10(sigma_chance),
           # log_lambda = log10(lambda),
           # sqrt(lambda),
           # log10(sqrt(lambda)),
           p0_chance = dbinom(0, size = N0, prob = p),
           p0_chance_pois = dpois(0, lambda = lambda)) %>%
    select(-p)

#- chance + N0 + dilution

set.seed(14212)

tibble(t = c(2, 4, 6, 8, 10, 12, 14)) %>%
    mutate(p = 10^(-(t/D)^beta),
           lambda = N0*p) %>%
    split(.$t) %>%
    map_dfr(.,
            ~ tibble(plate_d0 = rpois(1000, lambda = .$lambda),
                     plate_d1 = rpois(1000, lambda = .$lambda*1e-1),
                     plate_d2 = rpois(1000, lambda = .$lambda*1e-2),
                     plate_d3 = rpois(1000, lambda = .$lambda*1e-3),
                     plate_d4 = rpois(1000, lambda = .$lambda*1e-4),
                     t = .$t)
    ) %>%
    gather(dil, count, -t) %>%
    separate(dil, into = c("foo", "dil"), sep = "_d") %>%
    mutate(dil = as.numeric(dil),
           N = count/.1^dil) %>%
    select(-foo) %>%
    filter(count < 300) %>%
    filter(count > 30|dil==0) %>%
    mutate(logN_teor = log10(N0) - (t/D)^beta) %>%
    group_by(t) %>%
    summarize(sigma_N = sd(N),
              lambda = mean(N),
              p0 = mean(N == 0)
    ) %>%
    mutate(log10(lambda),
           log10(sigma_N))


##

N0 <- 1e6
beta <- 2

tibble(t = c(2, 4, 6, 8, 10, 12, 14)) %>%
    mutate(p = 10^(-(t/D)^beta),
           lambda = N0*p) %>%
    split(.$t) %>%
    map(.,
        ~ tibble(N = round(.$lambda-1000):round(.$lambda + 1000),
                 chance = dbinom(N, size = N0, prob = .$p),
                 `chance + N0` = dpois(N, lambda = .$lambda))
    ) %>%
    imap_dfr(.,
             ~ mutate(.x, t = as.numeric(.y))
    ) %>%
    pivot_longer(-c(N, t), names_to = "Source of variation", values_to = "p") %>%
    filter(p > 1e-6) %>%
    # mutate(t = paste0("t = ", t, " min")) %>%
    ggplot() +
    geom_point(aes(x = N, y = p, colour = `Source of variation`,
                   shape = `Source of variation`), size = 2) +
    facet_wrap(~t, scales = "free") +
    theme_bw() +
    xlab("Microbial count (CFU)") + ylab("Probability mass") +
    theme(legend.position = "top",
          axis.title = element_text(size = 16),
          axis.text = element_text(size = 14),
          strip.text = element_text(size = 14),
          # legend.title = element_text(size = 16),
          legend.title = element_blank(),
          legend.text = element_text(size = 14)
    )


## sup Table 2

N0 <- 1e6
beta <- .6

#- chance / chance + N0

tibble(t = c(5, 30, 50, 75, 100, 130)) %>%
    mutate(p = 10^(-(t/D)^beta),
           lambda = N0*p) %>%
    mutate(sigma_chance = sqrt(N0*p*(1-p)),
           log_sigma_chance = log10(sigma_chance),
           # log_lambda = log10(lambda),
           sqrt(lambda),
           log10(sqrt(lambda)),
           p0_chance = dbinom(0, size = N0, prob = p),
           p0_chance_pois = dpois(0, lambda = lambda)) %>%
    select(-p)

#- chance + N0 + dilution

set.seed(14212)

tibble(t = c(5, 30, 50, 75, 100, 130)) %>%
    mutate(p = 10^(-(t/D)^beta),
           lambda = N0*p) %>%
    split(.$t) %>%
    map_dfr(.,
            ~ tibble(plate_d0 = rpois(1000, lambda = .$lambda),
                     plate_d1 = rpois(1000, lambda = .$lambda*1e-1),
                     plate_d2 = rpois(1000, lambda = .$lambda*1e-2),
                     plate_d3 = rpois(1000, lambda = .$lambda*1e-3),
                     plate_d4 = rpois(1000, lambda = .$lambda*1e-4),
                     t = .$t)
    ) %>%
    gather(dil, count, -t) %>%
    separate(dil, into = c("foo", "dil"), sep = "_d") %>%
    mutate(dil = as.numeric(dil),
           N = count/.1^dil) %>%
    select(-foo) %>%
    filter(count < 300) %>%
    filter(count > 30|dil==0) %>%
    mutate(logN_teor = log10(N0) - (t/D)^beta) %>%
    group_by(t) %>%
    summarize(sigma_N = sd(N),
              lambda = mean(N),
              p0 = mean(N == 0)
    ) %>%
    mutate(log10(lambda),
           log10(sigma_N))


tibble(t = c(5, 30, 50, 75, 100, 130)) %>%
    mutate(p = 10^(-(t/D)^beta),
           lambda = N0*p) %>%
    split(.$t) %>%
    map(.,
        ~ tibble(N = round(.$lambda-1000):round(.$lambda + 1000),
                 chance = dbinom(N, size = N0, prob = .$p),
                 `chance + N0` = dpois(N, lambda = .$lambda))
    ) %>%
    imap_dfr(.,
             ~ mutate(.x, t = as.numeric(.y))
    ) %>%
    pivot_longer(-c(N, t), names_to = "Source of variation", values_to = "p") %>%
    filter(p > 1e-6) %>%
    # mutate(t = paste0("t = ", t, " min")) %>%
    ggplot() +
    geom_point(aes(x = N, y = p, colour = `Source of variation`,
                   shape = `Source of variation`), size = 2) +
    facet_wrap(~t, scales = "free") +
    theme_bw() +
    xlab("Microbial count (CFU)") + ylab("Probability mass") +
    theme(legend.position = "top",
          axis.title = element_text(size = 16),
          axis.text = element_text(size = 14),
          strip.text = element_text(size = 14),
          # legend.title = element_text(size = 16),
          legend.title = element_blank(),
          legend.text = element_text(size = 14)
    )


















