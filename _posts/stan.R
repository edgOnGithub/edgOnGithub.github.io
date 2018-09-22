library(readr)
library(rstanarm)
library(dplyr)
library(purrr)
library(broom)


##

collect.results <- function(models, names){
  
  results <- map2_dfr(.x = models,
                      .y = names,
                      .f = function(x, y){tidy(x) %>% mutate(subset = y)}) %>% 
    as_tibble()
  return(results)
}

calculate.alpha <- function(estimate){
  alpha <- estimate / (1 + estimate)
  return(alpha)
}
##

MRW <- read_csv('assets/Transformed Data/MRW_clean_new_dummies.csv') %>% 
  mutate(restricted_regressor = log(s) - log(n_g_d))


## Frequentist Models
classic_parameters_new <- c("oil",
                            "developing_no_oil",
                            "intermediate_no_oil",
                            "rich") %>%  
  map(~filter(MRW, club == .)) %>% 
  map(~lm(log(`1985`) ~ restricted_regressor,
          data = .)) %>% 
  collect.results(., names = c("Oil Restricted Classic",
                               "Developing Restricted Classic",
                               "Intermediate Restricted Classic",
                               "Rich Restricted Classic")) %>% 
  mutate(alpha = ifelse((term == 'restricted_regressor'), calculate.alpha(estimate), NA))
classic_parameters_new

## Stan Version

stan_parameters <- stan_glmer(formula = log(`1985`) ~ restricted_regressor + (1 | club),
                            data = MRW)
  
  