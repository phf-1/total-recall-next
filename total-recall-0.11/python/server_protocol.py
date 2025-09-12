# :ID: 1dfa3036-145f-4c7f-9de1-5e74bccfc8ef

# Context

from operator import methodcaller


class ServerProtocol:
    START = "server_start"
    STATE = "server_state"
    RCV = "server_rcv"
    STOP = "server_stop"

    # Interface

    @classmethod
    def start(cls, obj, data):
        call = methodcaller(cls.START, data)
        return call(obj)

    @classmethod
    def state(cls, obj):
        call = methodcaller(cls.STATE)
        return call(obj)

    @classmethod
    def rcv(cls, obj, msg):
        call = methodcaller(cls.RCV, msg)
        return call(obj)

    @classmethod
    def stop(cls, obj):
        call = methodcaller(cls.STOP)
        return call(obj)
