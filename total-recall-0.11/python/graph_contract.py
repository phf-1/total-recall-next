# :ID: fb305ce8-0516-4f48-825b-105b2704d6e9

# Context

from contract import Contract
from utils import is_str, list_of
from ok import Ok
from error import Error
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


def _client(state, request):
    match (state, request):
        case (None, sort) if _is_sort(sort):
            return Ok.mk(sort)

        case _:
            return Error.mk(f"Unexpected request. request = {request}")


def _server(state, request, reply, next_state):
    match (state, request):
        case (None, sort) if _is_sort(sort):
            match (reply, next_state):
                case (Ok(nodes), None) if _is_nodes(nodes):
                    return Ok.mk((reply, next_state))

                case (Degraded((nodes, rests)), None) if _is_nodes(nodes) and _is_rests(
                    rests
                ):
                    return Ok.mk((reply, next_state))

                case _:
                    msg = "Unexpected reply and/or next_state."
                    msg += f" reply = {reply}."
                    msg += f" next_state = {next_state}"
                    return Error.mk(msg)

        case _:
            msg = "The server cannot be in this state and receive this request."
            msg += f" state = {state}."
            msg += f" request = {request}."
            raise AssertionError(msg)


# Interface

graph_contract = Contract.mk(_client, _server)
