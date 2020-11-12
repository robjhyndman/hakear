library(dplyr)
library(gravitas)
library(parallel)
library(tidyverse)
source("R/shuffle_x_for_each_facet.R")

sm <- smart_meter10 %>%
  filter(customer_id %in% c("10017936"))

harmonies <- sm %>%
  harmony(
    ugran = "month",
    filter_in = "wknd_wday",
    filter_out = c("hhour", "fortnight")
  )

.data <- sm
response <- "general_supply_kwh"
harmony_tbl <- harmonies
# smart_harmony <- .data %>% rank_harmony(
#   harmony_tbl = harmonies,
#   response = "general_supply_kwh", dist_ordered = TRUE
# )

harmonies_split = harmonies %>% group_by(facet_variable,
                                         x_variable) %>%
  group_split()


data_split <- lapply(harmonies_split,
                     function(x){
                       format_data <- sm %>%
                         create_gran(x$facet_variable) %>%
                         create_gran(x$x_variable) %>%
                         as_tibble() %>%
                         select(id_facet = x$facet_variable,
                                id_x = x$x_variable,
                                sim_data = response) %>%
                         mutate(nfacet = length(unique(id_facet)),
                                nx = length(unique(id_x))) %>%
                         mutate(id_facet =
                                  factor(id_facet,
                                         labels = seq_len(nfacet)),
                                #levels =  unique(id_facet)),
                                id_x =
                                  factor(id_x,
                                         labels = seq_len(nx))) %>%
                         #levels =  unique(id_x)))

                         select(nfacet, nx, everything()) %>%
                         group_by(nfacet, nx, id_facet, id_x) %>%
                         nest() %>%
                         compute_mmpd_panel(quantile_prob = seq(0.01, 0.99, 0.01),
                                            dist_ordered = TRUE,
                                            nperm = 2
                         )
                     })

data_split %>% unlist() %>% tibble(.name_repair = "universal") %>% mutate(n = row_number()) -> h

bind_cols(harmonies, h) %>% arrange(desc(value))


mmpd_null_dist =
  suppressMessages(lapply(
    sim_null_split,
    function(x){
      replicate(100,
                {compute_mmpd_panel(shuffle_x_for_each_facet(x),
                                    quantile_prob = quantile_prob,
                                    dist_ordered = dist_ordered,
                                    nperm
                )
                })
    }))

