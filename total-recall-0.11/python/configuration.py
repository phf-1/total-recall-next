# :ID: 25974f54-a9f9-4429-b199-b43dbebcac1a

# Context

import sys


class Configuration:
    # Interface

    def read(self):
        if sys.flags.utf8_mode != 1:
            raise AssertionError("UTF-8 Mode is not enabled.")
