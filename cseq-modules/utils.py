class Result(object):
    def __init__(self, value=None, warn_msg=None, error_msg=None):
        self.value = value
        self.warn_msg = warn_msg
        self.error_msg = error_msg

    def bind(self, other):
        """Logs errors and warnings in self using other's methods

        Args:
            other (core.module.BasicModule): a CSeq module

        Returns:
            The value of this result
        """
        if self.warn_msg:
            other.warn(self.warn_msg)
        if self.error_msg:
            other.error(self.error_msg)
        return self.value


_safe = Result(warn_msg='no map generated due to program trivially verified safe')
_empty = Result(error_msg='the given map is empty')


def findpropositionalvarsize(key, lines):
    '''Get size of the DIMACS encoding for a variable

    Scan the (comments in the) DIMACS encoding to
    extract the identifiers of the propositional variables for the
    given local variable of the given function.

    Args:
        key (str): Description
        lines (:obj:`list` of :obj:`str`:): the DIMACS encoding

    Returns:
        int: The number of variables mapping to the given key.
    '''
    firstvar = lastvar = 0

    if len(lines) == 0:
        return _empty
    elif len(lines) == 1:
        return _safe

    for line in lines:
        if line.startswith(key):
            line = line[len(key):]
            vars = line.split()
            firstvar = int(vars[0])
            lastvar = -1
            for v in vars:
                try:
                    intv = int(v)
                    if intv > lastvar:
                        lastvar = intv
                except ValueError:
                    break
            if lastvar == -1:
                raise ValueError

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
            vars = line.split()
            firstvar = int(vars[0])
            lastvar = -1
            for v in vars:
                try:
                    intv = int(v)
                    if intv > lastvar:
                        lastvar = intv
                except ValueError:
                    break
            if lastvar == -1:
                raise ValueError

    if int(firstvar) + int(offset) <= int(lastvar):
        return Result(int(firstvar) + int(offset))
    else:
        return Result(error_msg="Incorrect offset %s[%i]" % (key, offset))


def get_bin(x, n):
    if x >= 0:
        return format(x, 'b').zfill(n)
    else:
        return get_bin((2 ** n) + x, n)
