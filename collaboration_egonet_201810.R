library(igraph)
# library(igraphtools)
library(tidyverse)
# library(rvacca)
library(networkD3)

# Adjacency data
data <- read_csv("collaboration_egonet_adj_matrix_201810.csv") %>%
  select(-1) %>%
  as.matrix
mode(data) <- "numeric"

# Node attributes
attr <- read_csv("collaboration_egonet_nodes_201810.csv") %>%
  select(name, Organization, Institute, Group)

# Convert adj matrix to edge list
data[is.na(data)] <- 0
data <- graph_from_adjacency_matrix(data, mode= "upper") %>% 
  igraph::as_data_frame(what= "edges") %>%
  dplyr::as_data_frame() %>%
  rename(nodeID1 = from, nodeID2 = to)

# Names of organizations NOT institutes in the data 
orgs <- attr$name[attr$Organization==1 & attr$Institute!=1]

# Remove organizations from node-attribute data frame
attr <- subset(attr, !(name %in% orgs))

# Remove organizations from edge list data frame
data <- filter(data, !((nodeID1 %in% orgs) | (nodeID2 %in% orgs)))

# Convert to graph
gr <- graph_from_data_frame(data, directed=FALSE, vertices= attr)

# Create edge list with numeric IDs 
# --------------------------------------------------------------

# All names of people in edge list
IDs <- sort(unique(c(data$nodeID1, data$nodeID2)))

# Verify that all vertices in edge list are in vertex data frame
if (!all(IDs %in% attr$name)) {
  ind <- !(IDs %in% attr$name)
  stop(paste(IDs[ind], "not in attr data frame"))
}

# Edge list created above does not include isolates.

# Isolates: Names who are in the attribute data but not in edge list
(isolates <- attr$name[!(attr$name %in% IDs)])

# These need to be addedd as self-loops to the edge list
isolates <- data_frame(nodeID1= isolates, nodeID2= isolates)
data <- rbind(data, isolates)

# Recreate vector of all names in edge list (after including isolates in edge
# list)
IDs <- c(data$nodeID1, data$nodeID2) %>% unique %>% sort

# IDs need to start from 0 for networkD3
IDs <- data_frame(ID= seq_along(IDs)-1, name= IDs)

# Replace nodeID1 with IDs
dataIDs <- left_join(data, IDs, by = c("nodeID1"="name"))
dataIDs <- dplyr::select(dataIDs, nodeID1=ID, nodeID2)

# Replace nodeID2 with IDs
dataIDs <- left_join(dataIDs, IDs, by = c("nodeID2"="name"))
dataIDs <- dplyr::select(dataIDs, nodeID1, nodeID2=ID)

# Reorder
data <- arrange(data, nodeID1, nodeID2)
dataIDs <- arrange(dataIDs, nodeID1, nodeID2)

# Bring IDs into attr
attr <- left_join(attr, IDs, by="name")

# Order rows
attr <- attr[order(attr$ID),]

# Node size variable
# attr$Nodesize <- NA
# attr$Nodesize[attr$Organization==1] <- 100
# attr$Nodesize[attr$Organization==0] <- 1


# Create D3 network
# --------------------------------------------------------------

# Legend, zoom
network <- forceNetwork(Links = dataIDs, Nodes = attr, Source = "nodeID1", 
                        Target = "nodeID2", NodeID = "name", Group= "Group", opacity = 1, zoom = TRUE, bounded = TRUE, legend= TRUE, opacityNoHover= 0)

saveNetwork(network, "collaboration_egonet.html", selfcontained = TRUE)

# No zoom
network <- forceNetwork(Links = dataIDs, Nodes = attr, Source = "nodeID1", Target = "nodeID2", NodeID = "name", Group= "Group", opacity = 1, zoom = FALSE, bounded = TRUE, legend= TRUE, opacityNoHover= 0)

saveNetwork(network, "collaboration_egonet_nozoom.html", selfcontained = TRUE)

# No zoom, no legend
network <- forceNetwork(Links = dataIDs, Nodes = attr, Source = "nodeID1", Target = "nodeID2", NodeID = "name", Group= "Group", opacity = 1, zoom = FALSE, bounded = TRUE, legend= FALSE, opacityNoHover= 0)

saveNetwork(network, "collaboration_egonet_nozoom_noleg.html", selfcontained = TRUE)














