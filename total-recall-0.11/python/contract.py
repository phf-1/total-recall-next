# :ID: 81169118-7f98-4509-a9ca-2fbf997247d1
# :REF: dabad7f3-e4b5-4070-9cb9-c224b7482974

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
