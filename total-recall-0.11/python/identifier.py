# :ID: 3bc7bf47-0c5f-4a15-9c8d-0bfbbea66ee2

# Context

from uuid import UUID


class Identifier:
    def __init__(self, s):
        if not isinstance(s, str):
            raise AssertionError("Identifier.mk expects a string")

        self._uuid = UUID(s)

    def __repr__(self) -> str:
        return f"Identifier({self._original})"

    # Interface

    @staticmethod
    def mk(s: str):
        return Identifier(s)

    @staticmethod
    def eq(id1, id2):
        return id1._uuid == id2._uuid

    def string(self):
        return self._uuid.hex

    def __eq__(self, id2):
        return self.eq(self, id2)
