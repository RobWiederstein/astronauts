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

input <- "./data/astronauts.csv"
df <- data.table::fread(input = input)
df.1 <-
        df %>%  
        select(mission_title, name) %>%
        inner_join(., select(., mission_title, name), by = "mission_title") %>%
        rename(name1 = name.x, name2 = name.y) %>%
        dplyr::filter(name1 != name2) %>%
        unique %>%
        arrange(mission_title)
library(igraph)
df.2 <- df.1[, 2:3]
colnames(df.2) <- c("from", "to")
#df.2 <- as.matrix(df.2)
file <- "./data/astronaut_edge_list.csv"
write.csv(df.2, file = file, row.names = F)
#nodes
nodes <- tibble(name = unique(c(df.1$name1, df.1$name2)))
nodes.1 <- merge(nodes, df, by = "name")
#need to only keep variables that unique to astronaut
# NOT to the missions
my.cols <- c("name", "sex", "year_of_birth")
grep(my.cols, colnames(nodes.1))
x <- unlist(sapply(colnames(nodes.1), grep, my.cols))
#add gender color attribute
nodes.1$sex_color <- "blue"
nodes.1$sex_color[which(nodes.1$sex == "female")] <- "pink"
file <- "./data/astronaut_node_list.csv"
write.csv(nodes.1, file = file, row.names = F)
#import
input <- "./data/astronaut_edge_list.csv"
d <- data.table::fread(input = input, header = T)
input <- "./data/astronaut_node_list.csv"
vertices <- data.table::fread(input = input, header = T)
g <- graph_from_data_frame(d = d,
                           directed = F,
                           vertices = vertices
                           )
par(mar = c(0, 0, 0, 0))
plot(g,
     layout = layout_with_fr,
     vertex.label = NA,
     edge.label = NA,
     vertex.size = 2,
     rescale = T)
par(mar = c(0, 0, 0, 0))
g <- simplify(g)
plot(g,
     layout = layout_with_fr,
     vertex.label = NA,
     edge.label = NA,
     vertex.size = 2,
     rescale = T)
#sex
V(g)$color
plot(g,
        layout = layout_with_fr,
        vertex.color = 
        vertex.label = NA,
        edge.label = NA,
        vertex.size = 2,
        rescale = T)
)
