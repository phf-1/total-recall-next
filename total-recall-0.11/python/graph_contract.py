# :ID: fb305ce8-0516-4f48-825b-105b2704d6e9
# :REF: d44c84b2-2c30-4463-bb88-3a3ab1cf5ab2

# Context

from contract import Contract
from utils import is_str, list_of
from ok import Ok
from degraded import Degraded


def _is_node(x):
    return is_str(x)


def _is_nodes(x):
    return list_of(x, _is_node)


def _is_cycle(x):
    return _is_nodes(x)


def _is_edge(x):
    match x:
        case [start, end]:
            return _is_node(start) and _is_node(end)
        case _:
            return False


def _is_edges(x):
    return list_of(x, _is_edge)


def _is_sort(x):
    match x:
        case ["sort", nodes, edges]:
            return _is_nodes(nodes) and _is_edges(edges)

        case _:
            return False


def _is_rest(x):
    match x:
        case (cycle, edges):
            return _is_cycle(cycle) and _is_edges(edges)

        case _:
            return False


def _is_rests(x):
    return list_of(x, _is_rest)


def _is_ok(x):
    return isinstance(x, Ok)


def _is_degraded(x):
    return isinstance(x, Degraded)


def _is_ok_nodes(x):
    return _is_ok(x) and _is_nodes(x.value)


def _is_degraded_nodes(x):
    if _is_degraded(x):
        match x.value:
            case (nodes, rests):
                return _is_nodes(nodes) and _is_rests(rests)

    return False


def _contract(state):
    match state:
        case None:

            def _check_request(request):
                match request:
                    case sort if _is_sort(sort):

                        def _check_response(response):
                            match response:
                                case (reply, next_state) if (
                                    _is_ok_nodes(reply) and next_state is None
                                ):
                                    return Contract.mk(_contract)

                                case (reply, next_state) if (
                                    _is_degraded_nodes(reply) and next_state is None
                                ):
                                    return Contract.mk(_contract)

                        return _check_response

            return _check_request


# Interface

graph_contract = Contract.mk(_contract)
