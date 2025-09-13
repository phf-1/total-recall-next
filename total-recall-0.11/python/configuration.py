# :ID: 25974f54-a9f9-4429-b199-b43dbebcac1a
# :REF: dd62b36a-0c72-4e49-bef9-f02feec16ac4

# Context

import sys


class Configuration:
    # Interface

    def read(self):
        if sys.flags.utf8_mode != 1:
            raise AssertionError("UTF-8 Mode is not enabled.")
