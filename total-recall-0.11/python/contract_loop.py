# :ID: 3a3ba30f-1cca-4c66-ac0d-c00c03ec02ce

# Context

from loop import Loop
from ok import Ok
from error import Error
from server_protocol import ServerProtocol as SP


class ContractLoop:
    def _init(self, data):
        server = self._server
        SP.start(server, data)
        return SP.state(server)

    def _tx(self, state, request):
        contract = self._contract
        server = self._server

        match contract.client(state, request):
            case Error(string):
                return (Error(f"Client error. error = {string}"), state)

            case Ok(request):
                try:
                    reply, next_state = SP.rcv(server, request)
                    match contract.server(state, request, reply, next_state):
                        case Error(string):
                            # TODO: stderr?
                            return (Error.mk(f"Server error. error = {string}"), state)

                        case Ok(response):
                            return response

                except Exception as e:
                    return (Error.mk(f"Server error. error = {e}"), state)

    def __init__(self, contract, server):
        self._contract = contract
        self._server = server
        init = lambda data: self._init(data)
        tx = lambda state, request: self._tx(state, request)
        self._loop = Loop.mk(init, tx)

    # Interface

    @classmethod
    def mk(cls, contract, server):
        return cls(contract, server)

    def start(self, data):
        self._loop.start(data)
