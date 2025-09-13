# :ID: 27952cb2-ad3a-40ad-841b-a8020c606b84
# :REF: 796fd86e-b841-406e-8c17-1163e31fcbd1

# Context

from dataclasses import dataclass
from nothing import Nothing
from just import Just


@dataclass
class Maybe:
    value: Nothing | Just

    # Interface

    @classmethod
    def mk(cls, x):
        return cls(x)
