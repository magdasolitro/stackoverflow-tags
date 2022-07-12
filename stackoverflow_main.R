library(tidyverse)
library(tidygraph)
library(ggraph)
library(readr)
library(igraph)

edges_path = paste("dataset/stack_network_links.csv", sep="")
nodes_path = paste("dataset/stack_network_nodes.csv", sep="")

edges = read_csv(edges_path)
nodes = read_csv(nodes_path)

graph = tbl_graph(nodes = nodes, edges = edges, directed = TRUE)

# ------------------------------------------------------------------------------
# CENTRALITY 

# in and out degree
in_deg = degree(graph, v = V(graph), mode = "in", loops = FALSE, 
				normalized = FALSE)

out_deg = degree(graph, v = V(graph), mode = "out", loops = FALSE, 
				 normalized = FALSE)

# betweenness
b = betweenness(graph)

# katz centrality
A = as_adjacency_matrix(graph)
eig = eigen(A)$values
r = max(abs(eig))
alpha = 0.85 / r
x =  alpha_centrality(graph, alpha = alpha, exo = 1)

# plot graph
ggraph(graph, layout = 'kk') +
	geom_edge_link(aes(end_cap = circle(1.5, "mm"), colour = edges$value),
				   arrow = arrow(angle = 30, length = unit(2, "mm"),
				   			  ends = "last")) +
	geom_node_point(aes(size = in_deg)) +
	geom_node_label(aes(label = nodes$name), label.size = 0.25, repel = TRUE) +
	theme_void()


# ------------------------------------------------------------------------------
# COMPARE WITH EXPECTED FLOWS

# flow = degree times weight
out_flow = edges %>%
	group_by(source) %>%
	summarise(out_flow = sum(value))

in_flow = edges %>%
	group_by(target) %>%
	summarise(in_flow = sum(value))

tot_flow = edges %>%
	summarise(tot_flow = sum(value))

tag = scan("dataset/tags_names.txt", what = character(0), sep="\n")
flows = tibble(tag = tag, in_flow = in_flow, out_flow = out_flow) %>%
	mutate(id = row_number()) %>%
	select(id, everything())

OUT = diag(as.numeric(unlist(out_flow[2])))
IN = diag(as.numeric(unlist(in_flow[2])))
TOT = diag(as.numeric(unlist(tot_flow[1])))

n = sum(diag(IN))

n_nodes = vcount(graph)
ONES = matrix(1, nrow = n_nodes, ncol = n_nodes)

# expected flow
E = (OUT %*% ONES %*% IN) / n

F = matrix(scan("dataset/flows.txt", what = numeric(0), sep=","),
		   nrow = n_nodes, ncol = n_nodes)

# normalized flows (X-test)
C = (F - E) / sqrt(E)

# normalized flows (G-test)
D = F * log(F / E)


# ------------------------------------------------------------------------------
# MOST INDISCIPLINARY TAGS
	
# similarity = function(g, type = "cosine", mode = "col" ) {
# 	A = as_adjacency_matrix(g, attr = "weight", sparse = FALSE)
# 	if (mode == "row") {A = t(A)}
# 	if (type == "cosine") {
# 		euclidean = function(x) {sqrt(x %*% x)}
# 		d = apply(A, 2, euclidean)
# 		D = diag(1/d)
# 		S = D %*% t(A) %*% A %*% D
# 	}
# 	if (type == "pearson") {
# 		S = cor(A)
# 	}
# 	return(S)
# }
# 
# shannon = function(p) {
# 	x = p * log2(p)
# 	x = replace(x, is.nan(x), 0)
# 	return(-sum(x))
# }
# 
# simpson = function(p) {
# 	x = 1 - sum(p * p)
# 	return(x)
# }
# 
# rao = function(p, D) {
# 	x = diag(p) %*% D %*% diag(p)
# 	return(sum(c(x)))
# }
# 
# 
# heterogeneity = function(g, D, mode = "col") {
# 	A = as_adjacency_matrix(g, attr = "weight", sparse = FALSE)
# 	if (mode == "col") {
# 		A = A %*% diag(1/colSums(A))
# 		dim = 2 
# 	} else {
# 		A = diag(1/rowSums(A)) %*% A
# 		dim = 1 
# 	}
# 	return(list(shannon = apply(A, dim, shannon), 
# 				simpson = apply(A, dim, simpson), 
# 				rao = apply(A, dim, rao, D)))
# }
# 
# f = graph_from_adjacency_matrix(F, mode = "directed", weighted = TRUE)
# V(f)$name = 1:vcount(f)
# 
# S = similarity(f)
# D = 1 - S
# het = heterogeneity(f, D)
# 
# flows = mutate(flows, shannon_id = het$shannon, simpson_id = het$simpson, rao_id = het$rao)
# 
# flows_id = select(flows, contains("_id"))
# corrplot.mixed(cor(flows_id), upper = "ellipse")

