# :ID: 69708334-a624-4bc8-8c90-9f3ad659f158
# :REF: 2a4529d8-85b7-4b74-928f-fc1506800855

# Context

from json_protocol import JsonProtocol
from dataclasses import dataclass
from typing import Any


@dataclass
class Ok:
    value: Any

    # Interface

    @classmethod
    def mk(cls, x):
        return cls(x)

    def json_string(self):
        string = JsonProtocol.string(self.value)
        return f'[":ok", {string}]'
