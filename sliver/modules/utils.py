class Result(object):
    def __init__(self, value=None, warn=None, error=None):
        self.value = value
        self.warn = warn
        self.error = error

    def bind(self, other):
        if self.warn:
            other.warn(self.warn)
        if self.error:
            other.error(self.error)
        return self.value


_safe = Result(warn='no map generated due to program trivially verified safe')
_empty = Result(error='the given map is empty')


def findpropositionalvarsize(key, lines):
    ''' Scan the (comments in the) DIMACS encoding to
        extract the identifiers of the propositional variables for the
        given local variable of the given function.
    '''
    firstvar = lastvar = 0

    if len(lines) == 0:
        return _empty
    elif len(lines) == 1:
        return _safe

    for line in lines:
        if line.startswith(key):
            line = line[len(key):]
            firstvar = int(line[:line.find(' ')])   # least significant digit
            lastvar = int(line[line.rfind(' '):])   # most significant digit

    return Result(int(lastvar) - int(firstvar) + 1)


def findpropositionalvar(key, lines, offset=0):
    ''' Scan the (comments in the) DIMACS encoding to
        extract the identifiers of the propositional variables for the
        given local variable of the given function.
    '''
    firstvar = lastvar = 0

    if len(lines) == 0:
        return _empty
    elif len(lines) == 1:
        return _safe

    for line in lines:
        # print line
        if line.startswith(key):
            line = line[len(key):]
            firstvar = int(line[:line.find(' ')])   # least significant digit
            lastvar = int(line[line.rfind(' '):])   # most significant digit

    if int(firstvar) + int(offset) <= int(lastvar):
        return Result(int(firstvar) + int(offset))
    else:
        return Result(error="Incorrect offset %s[%i]" % (key, offset))


def get_bin(x, n):
    if x >= 0:
        return format(x, 'b').zfill(n)
    else:
        return get_bin((2 ** n) + x, n)
