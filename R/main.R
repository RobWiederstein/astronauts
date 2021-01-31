# kaggle may have a cleaner dataset version at 
#https://www.kaggle.com/jessemostipak/astronaut-database
# https://aerospace.csis.org/data/international-astronaut-database/
#answer to convert to edgelist 
#https://stackoverflow.com/questions/48013081/creating-a-edgelist-from-a-dataframe
library(igraph)
library(dplyr)
library(igraph)
input <- "./data/astronauts.csv"
df <- data.table::fread(input = input)

# df.1 <- df %>% 
#         select(number, mission_number) %>%
#         inner_join(., select(., number, mission_number), by = "mission_number") %>%
#         rename(number1 = number.x, number2 = number.y) %>%
#         dplyr::filter(number1 != number2) %>%
#         arrange(mission_number) %>%
#         unique()
#it looks like there are 2 astronauts that have the same
#name, but different numbers --ignored for now
df.1 <-
        df %>%  
        select(mission_title, name) %>%
        inner_join(., select(., mission_title, name), by = "mission_title") %>%
        rename(name1 = name.x, name2 = name.y) %>%
        dplyr::filter(name1 != name2) %>%
        arrange(mission_title) %>% 
        unique
#save edge list
df.2 <- dplyr::select(df.1, name1, name2,  mission_title)
df.3 <- as.matrix(df.2[, 1:2])
g <- graph_from_edgelist(df.3, directed = FALSE)
g1 <- simplify(g)
plot(g,
     edge.label = NA,
     vertex.label = NA,
     vertex.size = 2
     )
file <- "./data/astronaut_edge_list.csv"
write.csv(df.2, file = file, row.names = F)
#nodes
nodes <- tibble(name = unique(c(df.2$from, df.2$to)))
#no merge by name, but by number
nodes.1 <- merge(nodes, df, by = "name")
#need to only keep variables that unique to astronaut
# NOT to the missions
my.cols <- c("name", "sex", "year_of_birth", "nationality",
            "military_civilian", "total_number_of_missions",
            "total_hrs_sum")
nodes.2 <- nodes.1 %>% 
        dplyr::select(all_of(my.cols)) %>%
        distinct()
my.names <- c("Aleksandrov, Aleksandr", "Williams, Jeffrey N.")
grep()
dplyr::filter(nodes.2, name %in% my.names)
#add gender color attribute
nodes.2$sex_color <- "blue"
nodes.2$sex_color[which(nodes.2$sex == "female")] <- "pink"
#add nationality attribute
nodes.2$nat
file <- "./data/astronaut_node_list.csv"
write.csv(nodes.2, file = file, row.names = F)
#clear the environment
rm(list = ls())
#import
input <- "./data/astronaut_edge_list.csv"
d <- data.table::fread(input = input, header = T)
input <- "./data/astronaut_node_list.csv"
vertices <- data.table::fread(input = input, header = T)
vertices <- distinct(vertices, name, .keep_all = T)
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
     main = "First Plot")
par(mar = c(0, 0, 0, 0))
g <- simplify(g)
plot(g,
     layout = layout_with_fr,
     vertex.label = NA,
     edge.label = NA,
     vertex.size = 2,
     rescale = T,
     main = "Simplified")
#sex
V(g)$color <- vertices$sex_color
plot(g,
     layout = layout_with_fr,
     vertex.color = vertex.attributes(g)$colour,
     vertex.label = NA,
     edge.label = NA,
     vertex.size = V(g)$size,
     rescale = T,
     main = "Gender",
     asp = 0
     )
#nationality
head(vertices)
rev(sort(table(vertices$nationality)))
vertices$nat_color <- "yellow"
vertices$nat_color[which(vertices$nationality == "U.S.")] <- "blue"
vertices$nat_color[which(vertices$nationality == "U.S.S.R/Russia")] <- "red"
V(g)$color <- vertices$nat_color
plot(g,
     layout = layout_with_fr,
     vertex.color = vertex.attributes(g)$colour,
     vertex.label = NA,
     edge.label = NA,
     vertex.size = 2,
     rescale = T,
     main = "Nationality"
)
#military/civilian
head(vertices)
rev(sort(table(vertices$military_civilian)))
vertices$mil_color <- "red"
vertices$mil_color[which(vertices$military_civilian == "civilian")] <- "blue"
V(g)$color <- vertices$mil_color
plot(g,
     layout = layout_with_fr,
     vertex.color = vertex.attributes(g)$colour,
     vertex.label = NA,
     edge.label = NA,
     vertex.size = 2,
     rescale = T,
     asp = 0,
     main = "Military/Civilian"
)
#military/civilian
head(vertices)
rev(sort(table(vertices$military_civilian)))
vertices$mil_color <- "red"
vertices$mil_color[which(vertices$military_civilian == "civilian")] <- "blue"
V(g)$color <- vertices$mil_color
plot(g,
     layout = layout_with_fr,
     vertex.color = vertex.attributes(g)$colour,
     vertex.label = NA,
     edge.label = NA,
     vertex.size = 2,
     rescale = T,
     asp = 0,
     main = "Military/Civilian"
)
#military/civilian + vertex.size == trips
V(g)$size <- vertices$total_number_of_missions
V(g)$color <- vertices$mil_color
par(mar = c(1, 1, 1, 1))
plot(g,
     layout = layout_with_fr,
     vertex.color = vertex.attributes(g)$colour,
     vertex.label = NA,
     edge.label = NA,
     vertex.size = vertex.attributes(g)$size ^.75,
     rescale = T,
     asp = 0,
     main = "Military/Civilian"
)
# Add a legend
coul <- c("red", "blue")
x <- c("military", "civilian")
legend("bottomleft", legend=levels(as.factor(V(g)$color))  , col = coul, bty = "n", pch=20 , pt.cex = 3, cex = 1.5, text.col=coul , horiz = FALSE, inset = c(0.1, 0.1))



