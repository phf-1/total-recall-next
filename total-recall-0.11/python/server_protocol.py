# :ID: 1dfa3036-145f-4c7f-9de1-5e74bccfc8ef

# Context

from operator import methodcaller


class ServerProtocol:
    START = "server_start"
    RCV = "server_rcv"
    STATE = methodcaller("server_state")
    STOP = methodcaller("server_stop")

    # Interface

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
        return cls.STATE(server)

    @classmethod
    def stop(cls, server):
        cls.STOP(server)
