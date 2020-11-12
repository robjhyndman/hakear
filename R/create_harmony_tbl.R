
  mutate(
    nfacet = length(unique(id_facet)),
    nx = length(unique(id_x))
  ) %>%
  mutate(
    id_facet =
      factor(id_facet,
             labels = seq_len(nfacet)
      ),
    # levels =  unique(id_facet)),
    id_x =
      factor(id_x,
             labels = seq_len(nx)
      )
  ) %>%
  # levels =  unique(id_x)))

  select(nfacet, nx, everything()) %>%
  group_by(nfacet, nx, id_facet, id_x) %>%
  nest()
}
