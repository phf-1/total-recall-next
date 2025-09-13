# :ID: 1dfa3036-145f-4c7f-9de1-5e74bccfc8ef
# :REF: 44eafef4-4db1-4fff-be14-b346a2f1b01b

# Context

from operator import methodcaller


class ServerProtocol:

    # Interface

    START = "server_start"
    RCV = "server_rcv"
    STATE = "server_state"
    STOP = "server_stop"

    @classmethod
    def start(cls, server, data):
        call = methodcaller(cls.START, data)
        call(server)

    @classmethod
    def rcv(cls, server, msg):
        call = methodcaller(cls.RCV, msg)
        return call(server)

    @classmethod
    def state(cls, server):
        call = methodcaller(cls.STATE)
        return call(server)

    @classmethod
    def stop(cls, server):
        call = methodcaller(cls.STOP)
        call(server)
