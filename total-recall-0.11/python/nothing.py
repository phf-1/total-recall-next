# :ID: 2ff19f9b-1895-4fd6-b76d-8bc1085e748f

# Context

from dataclasses import dataclass


@dataclass
class Nothing:
    _instance = None

    def __new__(cls):
        if cls._instance is None:
            cls._instance = super().__new__(cls)
            cls._instance.value = 42  # Initialize attributes here or in __init__
        return cls._instance

    # Interface

    @classmethod
    def mk(cls):
        return cls()

    def __eq__(self, x):
        return self is x
