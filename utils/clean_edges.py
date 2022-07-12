import csv

if __name__ == "__main__":
    # discard the last 25 nodes
    edges_path = "/Users/magdalenasolitro/Desktop/AI&CS MSc. UniUD/Advanced Data " \
                "Science/stackoverflow_project/dataset_original/stack_network_links.csv"
    new_edges_path = "/Users/magdalenasolitro/Desktop/AI&CS MSc. UniUD/Advanced " \
                "Data Science/stackoverflow_project/dataset/new_stack_network_links.csv"

    nodes_path = "/Users/magdalenasolitro/Desktop/AI&CS MSc. UniUD/Advanced Data " \
                "Science/stackoverflow_project/dataset_original/stack_network_nodes.csv"
    new_nodes_path = "/Users/magdalenasolitro/Desktop/AI&CS MSc. UniUD/Advanced Data " \
                "Science/stackoverflow_project/dataset/new_stack_network_nodes.csv"

    discarded_nodes = ["photoshop", "vb.net", "unity3d", "devops", "drupal",
                        "ionic-framework", "elasticsearch", "vue", "qt",
                        "redis", "jenkins", "rest", "testing", "tdd", "redux",
                        "linq", "wcf", "wpf", "nginx", "laravel", "less", "sass",
                        "hadoop", "redux", "php", "express", "excel-vba", "vba",
                        "excel", "amazon-web-services", "embedded", "mvc",
                        "powershell", "azure", "xamarin", "bootstrap", "agile"
                        "visual-studio", "selenium", "web-services", "codeigniter",
                        "jsp", "angular", "wordpress", "asp.net-web-api", "xcode",
                        "hibernate", "spring", "spring-mvc", "entity-framework",
                        "twitter-bootstrap", "twitter-bootstrap-3"]

    i = 0
    with open(nodes_path, 'r') as fp1, open(new_nodes_path, 'w') as fp2:
        writer = csv.writer(fp2)
        for row in csv.reader(fp1):
            if row[0] not in discarded_nodes:
                writer.writerow(row)
            else:
                i += 1
        print(str(i) + " nodes discarded.")

    i = 0
    with open(edges_path, 'r') as fp1, open(new_edges_path, 'w') as fp2:
        writer = csv.writer(fp2)
        for row in csv.reader(fp1):
            if row[0] not in discarded_nodes and row[1] not in discarded_nodes:
                writer.writerow(row)
            else:
                i += 1
        print(str(i) + " edges discarded.")

    new_nodes_path2 = "/Users/magdalenasolitro/Desktop/AI&CS MSc. UniUD/Advanced Data " \
                "Science/stackoverflow_project/dataset/new_stack_network_nodes2.csv"
