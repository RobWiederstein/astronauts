# kaggle may have a cleaner dataset version at 
#https://www.kaggle.com/jessemostipak/astronaut-database
# https://aerospace.csis.org/data/international-astronaut-database/
#answer to convert to edgelist 
#https://stackoverflow.com/questions/48013081/creating-a-edgelist-from-a-dataframe
file <- ".//International Astronaut Database.csv"
df <- read.csv(file = file, stringsAsFactors = F)
colnames(df) <- tolower(colnames(df))
#split flight column by separate flights
df.00 <- data.frame()
for(i in 1:nrow(df)){
flights <- unlist(str_split(df[i, 4], pattern = ", "))
df.new <- merge(df[i, 1], flights)
df.00 <- dplyr::bind_rows(df.00, df.new)
}
names(df.00) <- c("name", "flights")
df.00$name <- as.character(df.00$name)
df.00$flights <- as.character(df.00$flights)
#merge back
df.01 <- merge(df.00, df[, c(1:3, 5, 6)], drop = F)
#create edge list
