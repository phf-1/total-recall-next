# :ID: 2c0468fa-8fbd-48f1-83c5-d9321cb6f1ad

# Context

from dataclasses import dataclass


@dataclass
class Error:
    string: str

    # Interface

    @classmethod
    def mk(cls, x):
        return cls(x)

    def json_string(self):
        return f'[":error", {self.string}]'
