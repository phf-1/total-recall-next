# :ID: bfff70b0-755a-4d3c-907f-7d7de4e7a76f
# :REF: ba431f41-cef6-4074-97cc-d4e136850c22

# Context

from dataclasses import dataclass
from typing import Any


@dataclass
class Just:
    x: Any

    # Interface

    @classmethod
    def mk(cls, x):
        return cls(x)
