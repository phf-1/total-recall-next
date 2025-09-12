# :ID: 7e2cce5b-9c3d-41c0-9e62-c43334f0d120

# Context

from db import Db
from ok import Ok
from error import Error
from mark import Mark
from datetime import timedelta
from utctime import Time
from pathlib import Path


def _id_selected(rows, threshold, time):
    sorted_rows = sorted(rows, key=lambda row: row.time(), reverse=True)

    successes = []
    for row in sorted_rows:
        if row.mark() == Mark.SUCCESS:
            successes.append(row)
        else:
            break

    if not successes:
        return True

    else:
        last_time = successes[0].time()
        delta = timedelta(days=2 ** (len(successes) - 1))
        return time >= last_time + delta


def _select_ids(db, threshold, time, ids):
    return [id for id in ids if _id_selected(db.rows(identifier=id), threshold, time)]


class SelectorServer:
    # Interface

    @classmethod
    def mk(cls):
        return cls()

    def server_start(self, _data):
        return self

    def server_rcv(self, msg):
        match msg:
            case ["select", path_str, threshold, time_str, ids]:
                try:
                    db = Db.mk(Path(path_str))
                except Exception as e:
                    msg = "database cannot be built from path_str."
                    msg += f" path_str = {path_str}"
                    msg += f" exception = {e}"
                    return Error(msg)

                try:
                    time = Time.mk(time_str)
                except Exception as e:
                    msg = "time cannot be built from time_str."
                    msg += f" time_str = {time_str}"
                    msg += f" exception = {e}"
                    return Error(msg)

                selected_ids = _select_ids(db, threshold, time, ids)
                return (Ok.mk(selected_ids), None)

            case _:
                raise AssertionError(f"Unexpected message. message = {msg}")

    def server_state(self):
        return None

    def server_stop(self, _data):
        return None
