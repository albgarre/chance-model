
library(tidyverse)

## Probability of survival according to Beta distribution

#' It is assumed at N_i = 1
get_pop_lambda <- function(n_exps, n_cells, alpha, beta) {
    
    c(1:n_exps) %>%
        map(., ~ tibble(p = rbeta(n_cells, alpha, beta))) %>%
        map_dfr(., ~ summarize(., lambda = sum(p))) %>%
        mutate(exp = row_number())
    
}

get_alpha_beta <- function(mu, sigma) {
    
    if(sigma^2 >= mu*(1-mu)) stop("Too much variance")
    
    alpha <- mu^2 * ((1 - mu) / sigma^2 - 1 / mu)
    
    list(alpha = alpha,
         beta = alpha * (1 / mu - 1))
}

# get_alpha_beta(10^-1, .2)

##

N0 <- 1e4
D_val <- 1

tibble(t = seq(2, 5),
       mu = 10^(-t/D_val),
       max_var = mu*(1-mu)) %>%
    mutate(alpha = get_alpha_beta(mu, max_var*.9)$alpha,
           beta = get_alpha_beta(mu, max_var*.9)$beta) %>%
    split(.$t) %>%
    map(., ~ get_pop_lambda(10, N0, .$alpha, .$beta)) %>%
    imap_dfr(., ~ mutate(.x, time = as.numeric(.y))) %>%
    ggplot() +
        geom_point(aes(x = time, y = lambda)) + scale_y_log10() +
        xlab("Treatment time") + ylab("Expected microbial count (CFU)")

tibble(t = seq(2, 5),
       mu = 10^(-t/D_val),
       max_var = mu*(1-mu)) %>%
    split(.$t) %>%
    map(., ~ rpois(10, .$mu*N0)) %>%
    imap_dfr(., ~ tibble(time = as.numeric(.y),
                         N = .x)) %>%
    ggplot() +
    geom_point(aes(x = time, y = N)) + scale_y_log10() +
    xlab("Treatment time") + ylab("Microbial count (CFU)")

tibble(t = seq(2, 5),
       mu = 10^(-t/D_val),
       max_var = mu*(1-mu)) %>%
    split(.$t) %>%
    map(., ~ rpois(10, .$mu*N0)) %>%
    imap_dfr(., ~ tibble(time = as.numeric(.y),
                         N = .x)) %>%
    mutate(logN = log10(N)) %>%
    ggplot() +
    geom_point(aes(x = time, y = logN)) +
    xlab("Treatment time") + ylab("Microbial count (CFU)")







































































