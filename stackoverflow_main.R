library(tidyverse)
library(tidygraph)
library(ggraph)
library(readr)
library(igraph)

edges_path = "/Users/magdalenasolitro/Desktop/AI&CS MSc. UniUD/Advanced Data Science/stackoverflow_project/dataset/new_stack_network_links.csv"
nodes_path = "/Users/magdalenasolitro/Desktop/AI&CS MSc. UniUD/Advanced Data Science/stackoverflow_project/dataset/new_stack_network_nodes.csv"

edges = read_csv(edges_path)
nodes = read_csv(nodes_path)

graph = tbl_graph(nodes = nodes, edges = edges, directed = TRUE)

# ------------------------------------------------------------------------------
# COMPUTE IN AND OUT DEGREE

in_deg = degree(
	graph,
	v = V(graph),
	mode = "in",
	loops = FALSE,
	normalized = FALSE
)

out_deg = degree(
	graph,
	v = V(graph),
	mode = "out",
	loops = FALSE, 
	normalized = FALSE
)


# ------------------------------------------------------------------------------
# VISUALISE GRAPH

# ggraph(graph, layout = 'linear', circular = TRUE) +
# 	geom_edge_link(aes(end_cap = circle(1.5, "mm"), colour = edges$value),
# 				   arrow = arrow(angle = 30, length = unit(2, "mm"),
# 				   			  ends = "last")) +
# 	geom_node_point(aes(size = in_deg)) +
# 	geom_node_label(aes(label = nodes$name), label.size = 0.25, repel = TRUE) +
# 	theme_void()
# 

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

OUT = diag(as.numeric(unlist(out_flow[2])))
IN = diag(as.numeric(unlist(in_flow[2])))
TOT = diag(as.numeric(unlist(tot_flow[2])))

n = sum(diag(C))

ONES = matrix(1, nrow = vcount(graph), ncol = vcount(graph))

# expected flow
E = (OUT %*% ONES %*% IN) / n

# normalized flows (X-test)
# C = (F - E) / sqrt(E) 

# normalized flows (G-test)
# D = F * log(F / E)


# ------------------------------------------------------------------------------
# MOST INDISCIPLINARY TAGS
	
similarity = function(g, type = "cosine", mode = "col" ) {
	A = as_adjacency_matrix(g, attr = "weight", sparse = FALSE)
	if (mode == "row") {A = t(A)}
	if (type == "cosine") {
		euclidean = function(x) {sqrt(x %*% x)}
		d = apply(A, 2, euclidean)
		D = diag(1/d)
		S = D %*% t(A) %*% A %*% D
	}
	if (type == "pearson") {
		S = cor(A)
	}
	return(S)
}

shannon = function(p) {
	x = p * log2(p)
	x = replace(x, is.nan(x), 0)
	return(-sum(x))
}

simpson = function(p) {
	x = 1 - sum(p * p)
	return(x)
}

rao = function(p, D) {
	x = diag(p) %*% D %*% diag(p)
	return(sum(c(x)))
}


heterogeneity = function(g, D, mode = "col") {
	A = as_adjacency_matrix(g, attr = "weight", sparse = FALSE)
	if (mode == "col") {
		A = A %*% diag(1/colSums(A))
		dim = 2 
	} else {
		A = diag(1/rowSums(A)) %*% A
		dim = 1 
	}
	return(list(shannon = apply(A, dim, shannon), 
				simpson = apply(A, dim, simpson), 
				rao = apply(A, dim, rao, D)))
}

f = graph_from_adjacency_matrix(F, mode = "directed", weighted = TRUE)
V(f)$name = 1:vcount(f)

S = similarity(f)
D = 1 - S
het = heterogeneity(f, D)

flows = mutate(flows, shannon_id = het$shannon, simpson_id = het$simpson, rao_id = het$rao)

flows_id = select(flows, contains("_id"))
corrplot.mixed(cor(flows_id), upper = "ellipse")
