# :ID: 4deef454-284f-43d7-adfe-9a4bd85ffe67

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
