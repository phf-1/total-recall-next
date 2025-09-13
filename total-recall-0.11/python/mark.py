# :ID: 9c974443-0cfb-47e7-8b54-b29b16b85f53
# :REF: c561f394-58e8-48a9-a2fe-0ce8bbbc73ad

# Context

from enum import Enum


class Mark(Enum):
    SUCCESS = ":success"
    FAILURE = ":failure"
    SKIP = ":skip"

    # Interface

    @staticmethod
    def success():
        return Mark.SUCCESS

    @staticmethod
    def failure():
        return Mark.FAILURE

    @staticmethod
    def skip():
        return Mark.SKIP

    @staticmethod
    def from_string(s: str):
        for m in Mark:
            if s == m.string():
                return m
        raise AssertionError(f"s is unexpected. s = {s}")

    def string(self) -> str:
        return self.value
