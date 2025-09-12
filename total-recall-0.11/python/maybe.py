# :ID: 27952cb2-ad3a-40ad-841b-a8020c606b84

# Context

from dataclasses import dataclass
from nothing import Nothing
from just import Just


@dataclass
class Maybe:
    x: Nothing | Just

    # Interface

    @classmethod
    def mk(cls, x):
        return cls(x)
