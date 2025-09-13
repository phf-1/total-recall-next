# :ID: 2ff19f9b-1895-4fd6-b76d-8bc1085e748f
# :REF: 37efa605-0395-405a-b116-bc793f8f90cc

# Context

from dataclasses import dataclass


@dataclass
class Nothing:
    _instance = None

    def __new__(cls):
        if cls._instance is None:
            cls._instance = super().__new__(cls)
        return cls._instance

    # Interface

    @classmethod
    def mk(cls):
        return cls()

    def __eq__(self, x):
        return self is x
