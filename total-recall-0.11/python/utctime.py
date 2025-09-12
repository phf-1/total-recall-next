# :ID: 707bd668-327f-463a-82fb-eff4592fd962

# Context

from datetime import datetime, timezone


class Time(datetime):
    def __new__(cls, dt: datetime):
        if dt.tzinfo != timezone.utc:
            raise AssertionError("Time must be stored in UTC")
        return super().__new__(
            cls,
            dt.year,
            dt.month,
            dt.day,
            dt.hour,
            dt.minute,
            dt.second,
            dt.microsecond,
            dt.tzinfo,
        )

    # Interface

    @classmethod
    def mk(cls, s: str) -> "Time":
        try:
            if s.endswith("Z"):
                s = s[:-1] + "+00:00"
            dt = datetime.fromisoformat(s)

            if dt.tzinfo is None or dt.utcoffset() != timezone.utc.utcoffset(dt):
                raise AssertionError(f"Not UTC: {s}")

            return cls(dt.astimezone(timezone.utc))
        except Exception:
            raise AssertionError(f"Invalid ISO8601 UTC string: {s}")

    @classmethod
    def is_time_str(cls, x):
        try:
            cls.mk(x)
            return True
        except Exception:
            return False

    def string(self) -> str:
        return self.isoformat().replace("+00:00", "Z")

    def __repr__(self):
        return f"Time({self.string()})"
