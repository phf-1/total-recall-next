# :ID: 4ec52b7d-39e5-4179-aeb3-1ad5aac8c1c0

# Context

import igraph as ig
import networkx as nx
from ok import Ok
from degraded import Degraded


def _ndag_to_dag(ndag):
    return ("dag", "edges")


def _digraph_to_dag(digraph):
    try:
        cycle = nx.find_cycle(digraph, orientation="original")
        cycle = [edge[0] for edge in cycle]
    except nx.NetworkXNoCycle:
        return (digraph, [], [])

    ig_graph = ig.Graph.from_networkx(digraph, vertex_attr_hashable="name")
    removed_eids = ig_graph.feedback_arc_set(method="ip")
    edges = [
        (
            ig_graph.vs[ig_graph.es[eid].source]["name"],
            ig_graph.vs[ig_graph.es[eid].target]["name"],
        )
        for eid in removed_eids
    ]

    dag = digraph.copy()
    for u, v in edges:
        dag.remove_edge(u, v)

    return (dag, cycle, edges)


def _sort(nodes, edges):
    edges = [(start, end) for start, end in edges if start in nodes and end in nodes]
    g = nx.DiGraph()
    g.add_nodes_from(nodes)
    g.add_edges_from(edges)
    components = list(nx.weakly_connected_components(g))
    gs = [g.subgraph(component).copy() for component in components]
    gs.sort(key=lambda subgraph: subgraph.number_of_nodes())
    ordered_nodes = []
    rest = []
    for result in [_digraph_to_dag(g) for g in gs]:
        nodes = nx.topological_sort(result[0].reverse())
        ordered_nodes.extend(nodes)
        match result:
            case [_, [], []]:
                pass

            case [_, cycle, edges]:
                rest.append([cycle, edges])

    match rest:
        case []:
            return (Ok.mk(ordered_nodes), None)

        case _:
            return (Degraded.mk((ordered_nodes, rest)), None)


class GraphServer:
    # Interface

    @classmethod
    def mk(cls):
        return cls()

    def server_start(self, _data):
        return self

    def server_rcv(self, msg):
        match msg:
            case ["sort", nodes, edges]:
                return _sort(nodes, edges)

            case _:
                raise AssertionError(f"Unexpected message. message = {msg}")

    def server_state(self):
        return None

    def server_stop(self, _data):
        return None
