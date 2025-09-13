# :ID: f0ac51ae-05cb-4748-a469-1802e651b58b

# Context

from contract import Contract
from utils import is_str, list_of, is_float
from ok import Ok
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


def _is_ok(x):
    return isinstance(x, Ok)


def _is_ok_ids(x):
    return _is_ok(x) and _is_ids(x.value)


def _contract(state):
    match state:
        case None:

            def _check_request(request):
                if _is_select(request):

                    def _check_response(response):
                        match response:
                            case (reply, next_state) if (
                                _is_ok_ids(reply) and next_state is None
                            ):
                                return Contract.mk(_contract)

                    return _check_response

            return _check_request


# Interface

selector_contract = Contract.mk(_contract)
