df <- tibble(astronaut = c("a", "a", "b", "c"),
             flights = c("flight1", "flight2", "flight2", "flight3" )
             )

df %>%  inner_join(., select(., flights, astronaut), by = "flights") %>%
        rename(astronaut1 = astronaut.x, astronaut2 = astronaut.y) %>%
        dplyr::filter(astronaut1 != astronaut2) %>%
        unique %>%
        arrange(flights)
