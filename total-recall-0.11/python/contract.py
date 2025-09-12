# :ID: 81169118-7f98-4509-a9ca-2fbf997247d1

# Context

from dataclasses import dataclass
from error import Error
from ok import Ok
from typing import Any, Callable

State = Any
Request = Any
Reply = Any


@dataclass
class Contract:
    client: Callable[[State, Request], Ok | Error]
    server: Callable[[State, Request, Reply, State], Ok | Error]

    # Interface

    @classmethod
    def mk(cls, client, server):
        return cls(client, server)
