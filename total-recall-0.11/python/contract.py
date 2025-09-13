# :ID: 81169118-7f98-4509-a9ca-2fbf997247d1

# Context

from dataclasses import dataclass
from typing import Callable


@dataclass
class Contract:
    value: Callable

    # Interface

    @classmethod
    def mk(cls, value):
        return cls(value)
