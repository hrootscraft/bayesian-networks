# Load igraph
library(igraph)

# Set seed for reproducibility
set.seed(42)

# 1. Generate a preferential attachment graph (Barabási–Albert model)
g <- sample_pa(n = 30, power = 1.2, m = 2, directed = FALSE)
V(g)$name <- paste("U:", 1:vcount(g))

# Plot the main graph
plot(g, vertex.label = V(g)$name, vertex.size = 10,
     vertex.label.cex = 0.5, vertex.label.color = "black",
     main = "Facebook-like Friendship Network")
     
# 2. Compute and print graph density
density_val <- edge_density(g)
cat("Graph Density:", density_val, "\n")

# 3. Compute shortest path between two random nodes
shortest_path <- shortest_paths(g, from = 3, to = 14)$vpath[[1]]
cat("Shortest path from node 3 to 14:", shortest_path, "\n")

# 4. Diameter of the graph
graph_diameter <- diameter(g)
cat("Graph Diameter:", graph_diameter, "\n")

# 5. Cliques
cliques_list <- cliques(g, min = 3)
cat("Number of cliques of size ≥ 3:", length(cliques_list), "\n")

# Extract the largest clique
largest_clique <- largest_cliques(g)[[1]]

# Create subgraph of the largest clique
clique_subgraph <- induced_subgraph(g, largest_clique)

# Plot the largest clique
plot(clique_subgraph, 
     vertex.label = NA, 
     vertex.size = 10, 
     edge.color = "red",
     main = "Largest Clique Subgraph")

# 6. Closeness centrality
closeness_vals <- closeness(g)
print("Closeness Centrality:")
print(closeness_vals)

# 7. Betweenness centrality
betweenness_vals <- betweenness(g)
print("Betweenness Centrality:")
print(betweenness_vals)

# 8. Link prediction using Jaccard similarity
lp <- similarity.jaccard(g)
cat("Jaccard similarity matrix (link prediction):\n")
print(round(lp[1:5, 1:5], 2))  # Show a subset for clarity

# 9. Community detection using edge betweenness
communities <- cluster_edge_betweenness(g)
cat("Detected communities (edge betweenness):\n")
print(membership(communities))

# Plot with communities highlighted
plot(communities, g, main = "Community Detection")