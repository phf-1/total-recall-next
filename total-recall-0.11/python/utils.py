from nothing import Nothing

nothing = Nothing.mk()


def find_in_iterable(it, pred):
    return next((el for el in it if pred(el)), nothing)


def is_list(el):
    return isinstance(el, list)


def is_str(el):
    return isinstance(el, str)


def list_of(x, pred):
    return isinstance(x, list) and all(map(pred, x))


def is_float(el):
    return isinstance(el, float)
