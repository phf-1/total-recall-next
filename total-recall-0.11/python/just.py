# :ID: bfff70b0-755a-4d3c-907f-7d7de4e7a76f

# Context

from dataclasses import dataclass
from typing import Any


@dataclass
class Just:
    x: Object

    # Interface

    @classmethod
    def mk(cls, x):
        return cls(x)
