# :ID: 570c7719-f2e3-4961-bdb5-8ba1db753c39
# :REF: e794f0cb-fa14-4447-9897-cb85683f97f1

# Context

from row import Row
import sqlite3


class Db:
    TABLE = "exercise_log"

    def __init__(self, rows):
        self._rows = rows

    # Interface

    @classmethod
    def mk(cls, path):
        if not path.is_file():
            raise AssertionError(f"path is not a regular file. {path}")

        disk_conn = sqlite3.connect(path)
        disk_cursor = disk_conn.cursor()
        tuples = disk_cursor.execute(f"SELECT * FROM {cls.TABLE}").fetchall()
        disk_conn.close()
        rows = [Row(*t) for t in tuples]
        return cls(rows)

    def rows(self, identifier=None):
        if identifier is None:
            return self._rows

        return [row for row in self._rows if row.id() == identifier]
