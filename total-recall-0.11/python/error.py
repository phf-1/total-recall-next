# :ID: 2c0468fa-8fbd-48f1-83c5-d9321cb6f1ad
# :REF: d1ff1c40-a6e6-4054-afbe-71fb2b77eba2

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
