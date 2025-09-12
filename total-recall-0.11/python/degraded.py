# :ID: 66c46343-bc91-497b-8e4a-f605cf4b4423

# Context

from json_protocol import JsonProtocol
from dataclasses import dataclass
from typing import Any


@dataclass
class Degraded:
    value: Any

    # Interface

    @classmethod
    def mk(cls, x):
        return cls(x)

    def json_string(self):
        string = JsonProtocol.string(self.value)
        return f'[":degraded", {string}]'
