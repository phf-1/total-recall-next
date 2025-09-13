# :ID: 27de3cd6-e4b0-4621-8ab9-f85bb86f17b6
# :REF: 389a2c5d-6738-4bf5-9f4b-de9b111faa9b

# Context

from error import Error
from json_protocol import JsonProtocol
import json
import sys
import traceback


def _send_reply(reply):
    sys.stdout.write(JsonProtocol.string(reply) + "\n")
    sys.stdout.flush()


class Loop:
    EOF = ""

    def __init__(self, init, tx):
        self._init = init
        self._tx = tx

    # Interface

    @classmethod
    def mk(cls, init, tx):
        return cls(init, tx)

    def start(self, data):
        state = self._init(data)
        tx = self._tx

        while True:
            try:
                string = sys.stdin.readline().strip()
                if string == self.EOF:
                    break

                try:
                    message = json.loads(string)
                except json.JSONDecodeError:
                    reply = Error.mk(
                        f"message is not a JSON string. message = {string}"
                    )
                    _send_reply(reply)
                    continue

                try:
                    reply, state = tx(state, message)
                except Exception as e:
                    exc_type = type(e).__name__
                    exc_message = str(e)
                    exc_traceback = traceback.format_exc().strip().split("\n")
                    error_info = {
                        "type": exc_type,
                        "message": exc_message,
                        "traceback": exc_traceback,
                    }
                    msg = f"Server error. error = {error_info}"
                    reply = Error.mk(msg)

            except KeyboardInterrupt:
                break

            _send_reply(reply)
