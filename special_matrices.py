import numpy as np
from scipy.sparse import csr_matrix
from scipy.linalg import eigvalsh
import networkx as nx


def draw_graph_from_adjacency_matrix(A):
    """
    Draw a graph from a given adjacency matrix.
    """
    # Create a new graph
    G = nx.Graph()

    # Add edges to the graph based on the adjacency matrix
    for i in range(A.shape[0]):
        for j in range(i+1, A.shape[1]):  # Only need to consider upper triangular part of matrix
            if A[i, j] != 0:
                G.add_edge(i, j)

    # Draw the graph
    nx.draw(G, with_labels=True, node_color='lightblue', node_size=1200)


def generate_adjacency_matrix(graph):
    """
    Generate the adjacency matrix for a given graph.
    The graph parameter should be a dictionary where keys are nodes and values are lists of nodes that the key node is connected to.
    """
    nodes = list(graph.keys())
    A = np.zeros((len(nodes), len(nodes)))
    for node in graph:
        for conn_node in graph[node]:
            A[node, conn_node] = 1
    return A


def generate_degree_matrix(graph):
    """
    Generate the degree matrix for a given graph.
    The graph parameter should be a dictionary where keys are nodes and values are lists of nodes that the key node is connected to.
    """
    nodes = list(graph.keys())
    degrees = [len(graph[node]) for node in nodes]
    D = np.diag(degrees)
    return D


def generate_kirchhoff_from_graph(graph):
    """
    Generate the Kirchhoff (Laplacian) matrix for a given graph.
    The graph parameter should be a dictionary where keys are nodes and values are lists of nodes that the key node is connected to.
    """
    A = generate_adjacency_matrix(graph)
    D = generate_degree_matrix(graph)
    L = D - A
    return L


def network_analysis(graph):
    """
    Perform a network structure analysis using the Laplacian matrix.
    """
    # Generate the Laplacian matrix
    L = generate_kirchhoff_from_graph(graph)

    # Calculate the degree of each node
    degrees = np.diag(L)

    # Calculate the number of connected components (number of zero eigenvalues)
    eigenvalues = eigvalsh(L)
    num_connected_components = np.sum(eigenvalues < 1e-10)  # Small threshold to account for numerical errors

    # Calculate the number of spanning trees (for a connected graph)
    if num_connected_components == 1:
        num_spanning_trees = 1/len(graph) * np.prod(eigenvalues[1:])  # Skip the first eigenvalue (which should be zero)
    else:
        num_spanning_trees = None  # This formula only applies for connected graphs

    return {
        "degrees": degrees,
        "num_connected_components": num_connected_components,
        "num_spanning_trees": num_spanning_trees,
    }


def generate_circulant(n):
    """
    Generate an nxn Kirchhoff matrix for a cyclic graph.
    """
    # Create a cyclic graph with n nodes
    graph = {i: [(i-1)%n, (i+1)%n] for i in range(n)}

    # Generate the Kirchhoff matrix from the graph
    K_old = generate_kirchhoff_from_graph(graph)

    return K_old


def generate_kirchhoff_linear(n):
    """
    Generate an nxn Kirchhoff matrix from a linear graph where the first and
    last nodes are connected to only one other node each..
    """
    # Create a linear graph with n nodes
    graph = {i: [i-1, i+1] for i in range(n)}
    graph[0] = [1]  # First node is only connected to the second node
    graph[n-1] = [n-2]  # Last node is only connected to the second last node

    # Generate the Kirchhoff matrix from the graph
    K = generate_kirchhoff_from_graph(graph)

    return K


def generate_kirchhoff(n):
    """
    Generate an nxn Kirchhoff matrix
    """
    return 2*np.eye(n) - np.eye(n, k=1) - np.eye(n, k=-1)


def generate_upper_triangular(n):
    """
    Generate an nxn upper triangular matrix with ones on the diagonal and above.
    """
    U = np.eye(n) - np.eye(n, k=1)
    return U


def generate_bipartite(graph):
    """
    Generate a bipartite matrix from a given graph.
    The graph parameter should be a dictionary where keys are nodes and values are lists of nodes that the key node is connected to.
    """
    nodes = list(graph.keys())
    edges = [(node, conn_node) for node in graph for conn_node in graph[node]]
    data = np.ones(len(edges))
    row = [nodes.index(edge[0]) for edge in edges]
    col = [nodes.index(edge[1]) for edge in edges]
    B = csr_matrix((data, (row, col)), shape=(len(nodes), len(nodes)))
    return B.toarray()


# Let's test the functions with some inputs
K = generate_kirchhoff(5)
C = generate_circulant(5)
B = generate_kirchhoff_linear(5)
U = generate_upper_triangular(5)
graph = {0: [1, 2], 1: [0, 3], 2: [0, 3, 4], 3: [1, 2], 4: [2]}
BP = generate_bipartite(graph)

print(K)
print(C)
print(B)
print(U)

# Let's generate a Kirchhoff matrix with n = 5 using the provided function
K = generate_kirchhoff(5)

# Now we'll convert this matrix into an adjacency matrix by replacing 2's with 0's and -1's with 1's
A = np.copy(K)
A[A == 2] = 0
A[A == -1] = 1

# Draw the graph corresponding to this adjacency matrix
draw_graph_from_adjacency_matrix(A)


graph = {0: [1, 2], 1: [0, 3], 2: [0, 3, 4], 3: [1, 2], 4: [2]}
network_analysis(graph)
