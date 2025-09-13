# :ID: 4deef454-284f-43d7-adfe-9a4bd85ffe67
# :REF: a9530b61-915e-4e0f-8fac-077b12a7399e

# Context

from operator import methodcaller
import json


class JsonProtocol:
    JSON_STRING = methodcaller("json_string")

    # Interface

    @classmethod
    def string(cls, x):
        try:
            return cls.JSON_STRING(x)
        except AttributeError:
            return json.dumps(x)
