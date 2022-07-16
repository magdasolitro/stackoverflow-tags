library(tidyverse)
library(tidygraph)
library(ggraph)
library(ggplot2)
library(readr)
library(igraph)
library(corrplot)

edges_path = paste("dataset/stack_network_links.csv", sep="")
nodes_path = paste("dataset/stack_network_nodes.csv", sep="")

edges = read_csv(edges_path)
nodes = read_csv(nodes_path)[-2]

graph = tbl_graph(nodes = nodes, edges = edges, directed = TRUE)

# ------------------------------------------------------------------------------
# CENTRALITY 

# in and out degree
in_deg = degree(graph, v = V(graph), mode = "in", loops = FALSE, 
				normalized = FALSE)

out_deg = degree(graph, v = V(graph), mode = "out", loops = FALSE, 
				 normalized = FALSE)

# betweenness
betweenness = betweenness(graph)

# katz centrality
A = as_adjacency_matrix(graph)
eig = eigen(A)$values
r = max(abs(eig))
alpha = 0.85 / r
katz = alpha_centrality(graph, alpha = alpha, exo = 1)

# plot graph
ggraph(graph, layout = 'kk') +
	geom_edge_link(aes(colour = value)) +
	geom_node_point(aes(size = in_deg, colour = b)) +
	geom_node_label(aes(label = nodes$name), label.size = 0.25, repel = TRUE) +
	theme_void()


# ------------------------------------------------------------------------------
# COMPARE WITH EXPECTED FLOWS

n_nodes = vcount(graph)

F = matrix(scan("dataset/flows.txt", what = numeric(0), sep=","),
		   nrow = n_nodes, ncol = n_nodes)

tag = scan("dataset/tags_names.txt", what = character(0), sep="\n")

out_flow = rowSums(F)
names(out_flow) = tag

in_flow = colSums(F)
names(in_flow) = tag

flows = tibble(tag = tag, in_flow = in_flow, out_flow = out_flow) %>%
	mutate(id = row_number()) %>%
	select(id, everything())

# normalize by expected flows 
R = diag(rowSums(F))
C = diag(colSums(F))
n = sum(diag(C))
O = matrix(1, nrow = n_nodes, ncol = n_nodes)

# expected flow
E = (R %*% O %*% C) / n
# normalized flows (X-test)
C = (F - E) / sqrt(E) 
# normalized flows (G-test)
D = F * log(F / E)

g = graph_from_adjacency_matrix(C, mode = "plus", weighted = TRUE)
V(g)$name = 1:vcount(g)

chi = as_tibble(as_data_frame(graph)) %>%
	mutate(from = as.integer(from), to = as.integer(to)) %>%
	left_join(flows, by = c("from" = "id")) %>%
	left_join(flows, by = c("to" = "id")) %>%
	select(from, to, discipline.from = discipline.x, discipline.to = discipline.y,
		   size.from = size.x, size.to = size.y, weight)

# top-10 different discipline pairs
filter(chi, from != to) %>%
	arrange(desc(weight)) %>%
	head(10)

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

flows = mutate(flows, shannon_id = het$shannon, simpson_id = het$simpson, 
			   rao_id = het$rao)

flows_id = select(flows, contains("_id"))
corrplot.mixed(cor(flows_id), upper = "ellipse")


# ------------------------------------------------------------------------------
# COMMUNITY DETECTION
groups = cluster_walktrap(graph)
mod = modularity(groups)
l = length(groups)

# add group column to nodes tibble
nodes = nodes %>% add_column(group = as.integer(membership(groups)))

# plot graph highlighting the group division
ggraph(layout = family_couples_coords) +
	geom_edge_arc(aes(alpha = betweenness, colour = as.factor(couple)), strength = 0.05) +
	geom_node_point(aes(size = pagerank, colour = as.factor(community))) +
	geom_node_text(aes(label = name), repel = T)  +
	scale_color_brewer(palette = "Paired")

ggraph(graph, layout = 'kk') +
	geom_edge_link(aes(end_cap = circle(1.5, "mm")),
				   arrow = arrow(angle = 30, length = unit(2, "mm"),
				   			  ends = "last")) +
	geom_node_point(aes(size = 2, colour = nodes$group)) +
	geom_node_label(aes(label = nodes$name), label.size = 0.25, repel = TRUE) +
	theme_void()

# group histogram
group_sizes <-
	nodes %>%
	group_by(group) %>%
	count()

ggplot(group_sizes, aes(x=group, y=n, fill=n)) + 
	geom_col()

# use hierarchical clustering to group tags into macroareas
# distance object
d = as.dist(D)

# average-linkage clustering method
hc = hclust(d, method = "average")

# plot dendrogram
plot(hc)