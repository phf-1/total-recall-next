# :ID: 3a3ba30f-1cca-4c66-ac0d-c00c03ec02ce

# Context

from loop import Loop
from error import Error
from server_protocol import ServerProtocol as SP


def _init_mk(server):
    def init(data):
        SP.start(server, data)
        return SP.state(server)

    return init


def _tx_mk(contract, server):
    contract_closure = contract

    def tx(state, request):
        nonlocal contract_closure

        msg = f"Server#state() = {state}. "
        if (check := contract_closure.value(state)) is None:
            msg += "Server error. Unexpected state."
            return (Error(msg), state)

        msg += f"request = {request}. "
        if (check := check(request)) is None:
            msg += "Request error. Unexpected request."
            return (Error(msg), state)

        response = SP.rcv(server, request)
        msg += f"response = {response}. "
        if (contract_closure := check(response)) is None:
            msg += "Server error. Unexpected response."
            return (Error(msg), state)

        return response

    return tx


class ContractLoop(Loop):
    # Interface

    @classmethod
    def mk(cls, contract, server):
        return cls(_init_mk(server), _tx_mk(contract, server))
