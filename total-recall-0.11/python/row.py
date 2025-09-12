# :ID: 79a21c29-0a23-456f-89a1-e26f31dff74e

# Context

from utctime import Time
from dataclasses import dataclass
from mark import Mark


@dataclass(frozen=True)
class Row:
    _mark: Mark
    _id: str
    _time: Time

    # Interface

    @staticmethod
    def mk(mark, identifier, time):
        if not isinstance(mark, Mark):
            raise AssertionError(
                f"Row.mk: first argument must be Mark, got {type(mark)}"
            )

        if not isinstance(identifier, str):
            raise AssertionError(
                f"Row.mk: second argument must be str, got {type(identifier)}"
            )

        if not isinstance(time, Time):
            raise AssertionError(
                f"Row.mk: third argument must be Time, got {type(time)}"
            )

        return Row(mark, identifier, time)

    def mark(self) -> Mark:
        return self._mark

    def id(self) -> str:
        return self._id

    def time(self) -> Time:
        return self._time

    def __repr__(self) -> str:
        return f"Row(mark={self._mark}, id={self._id}, time={self._time})"
