library(magrittr)
con <- "./data/so_question.txt"
df <- readLines(con = con) %>%
        str_split(pattern = "\\s+") %>%
        unlist() %>%
        matrix(ncol = 6, byrow = T) %>%
        as_tibble()
colnames(df) <- df[1, ]
df <- df[-1, ]

library(dplyr)
df %>% select(session, weight, sequence, INDIVIDUAL) %>%
        inner_join(., select(., session, INDIVIDUAL), by = "session") %>%
        rename(INDIVIDUAL1 = INDIVIDUAL.x, INDIVIDUAL2 = INDIVIDUAL.y) %>%
        dplyr::filter(INDIVIDUAL1 != INDIVIDUAL2) %>%
        unique %>%
        arrange(session, sequence)

