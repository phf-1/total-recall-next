# :ID: f0ac51ae-05cb-4748-a469-1802e651b58b

# Context

from contract import Contract
from utils import is_str, list_of, is_float
from ok import Ok
from error import Error
from utctime import Time


def _is_threshold(x):
    return is_float(x) and (0.0 <= x) and (x <= 1.0)


def _is_time(x):
    return Time.is_time_str(x)


def _is_db_path(x):
    return is_str(x)


def _is_ids(x):
    return list_of(x, is_str)


def _is_select(x):
    match x:
        case ["select", db_path, threshold, time, ids]:
            return (
                _is_db_path(db_path)
                and _is_threshold(threshold)
                and _is_time(time)
                and _is_ids(ids)
            )

        case _:
            return False


def _client(state, request):
    match (state, request):
        case (None, select) if _is_select(select):
            return Ok.mk(select)

        case _:
            return Error.mk(f"Unexpected request. request = {request}")


def _server(state, request, reply, next_state):
    match (state, request):
        case (None, select) if _is_select(select):
            match (reply, next_state):
                case (Ok(ids), None) if _is_ids(ids):
                    return Ok.mk((reply, next_state))

                case (Error(string), None) if is_str(string):
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

selector_contract = Contract.mk(_client, _server)
