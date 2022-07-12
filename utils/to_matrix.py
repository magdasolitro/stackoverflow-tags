# Convert csv file containing information about the network flows to a matrix
import csv
import numpy as np

if __name__ == "__main__":
    nodes_path = "/Users/magdalenasolitro/Desktop/AI&CS MSc. UniUD/Advanced Data " \
                "Science/stackoverflow_project/dataset/stack_network_nodes.csv"

    edges_path = "/Users/magdalenasolitro/Desktop/AI&CS MSc. UniUD/Advanced " \
                "Data Science/stackoverflow_project/dataset/stack_network_links.csv"

    flows_path = "/Users/magdalenasolitro/Desktop/AI&CS MSc. UniUD/Advanced " \
                "Data Science/stackoverflow_project/dataset/flows.txt"

    nodes_dict = dict()
    node_names = list()

    # fill nodes_dict with node names and numerical ids
    with open(nodes_path, 'r') as fp:
        node_reader = csv.reader(fp)
        idx = 0
        for row in node_reader:
            current_node =  str(row[0])
            if current_node != 'name':
                nodes_dict[current_node] = idx
                node_names.append(current_node)
                idx += 1
    # write flows matrix
    flows_matrix = np.zeros((len(nodes_dict), len(nodes_dict)))

    with open(flows_path, 'w') as fp1, open(edges_path, 'r') as fp2:
        reader = csv.reader(fp2)
        for row in reader:
            r = row[0]
            c = row[1]
            if row[0] == 'source':
                continue
            else:
                flows_matrix[nodes_dict[r], nodes_dict[c]] = row[2]

        for row in flows_matrix:
            for i in range(len(node_names)-1):
                fp1.write(str(row[i]) + ', ')
            fp1.write('\n')

    print(flows_matrix)
