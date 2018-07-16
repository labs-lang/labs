import re


ATTR = re.compile(r"I\[([0-9]+)l?\]\[([0-9]+)l?\]")
LSTIG = re.compile(r"Lvalue\[([0-9]+)l?\]\[([0-9]+)l?\]")
LTSTAMP = re.compile(r"Ltstamp\[([0-9]+)l?\]\[([0-9]+)l?\]")
ENV = re.compile(r"E\[([0-9]+)l?\]")
STEP = re.compile(r"__LABS_step")


def translateCPROVER(cex, c_program, info, backend):
    translatedcex = ''
    lines = cex.split('\n')
    k = 0  # cex[:cex.find('Trace for')].count('\n') + 1 + 1
    separator = "----------------------------------------------------"

    for k, ln in enumerate(lines):
        # case 1: another transition to fetch
        if ln.startswith('State ') and lines[k + 1] == separator:
            A, B, C = ln, lines[k + 1], lines[k + 2]

            # the part below the separator might be
            # more than one line long..
            j = 1
            while (k + 2 + j < len(lines) and
                    not lines[k + 2 + j].startswith('State ') and
                    not lines[k + 2 + j].startswith('Violated property')):
                C += lines[k + 2 + j]
                j += 1

            translatedcex += _mapCPROVERstate(A, B, C, info)

        # case 2: final transation with property violation
        elif ln.startswith('Violated property') and backend == "cbmc":
            # Y, Z, W = lines[k + 1], lines[k + 2], lines[k + 3]
            Y = keys_of(lines[k + 1])
            _, prop = c_program.split("\n")[int(Y["line"]) - 1].split("//")
            translatedcex += """Violated property: {}\n""".format(prop)
            break  # Stop converting after the 1st property has been violated
        elif ln.startswith('Violated property'):
            Y = keys_of(lines[k + 1])
            _, prop = c_program.split("\n")[int(Y["line"]) + 5].split("//")
            translatedcex += """Violated property: {}\n""".format(prop)

    if len(translatedcex) > 0:
        translatedcex = "Counterexample:\n\n{}\n".format(translatedcex)

    return translatedcex


def keys_of(ln):
    tokens = ln.split()
    return {key: value for key, value in zip(tokens[0::2], tokens[1::2])}


last_return = ""
last_step = -1


def _mapCPROVERstate(A, B, C, info):
    global last_return, last_step
    '''
    'Violated property:'
    '  file _cs_lazy_unsafe.c line 318 function thread3_0'
    '  assertion 0 != 0'
    '  0 != 0'
    '''
    # Fetch values.
    try:
        # 1st line
        keys = keys_of(A)
        keys["lvalue"], rvalue = C.strip().split("=")
        keys["rvalue"] = rvalue.split(" ")[0]

        is_attr = ATTR.match(keys["lvalue"])
        if is_attr and keys["rvalue"] != "2147483647":
            tid, k = is_attr.group(1), is_attr.group(2)
            last_return = "attr"
            return "{} {}:\t{} <- {}\n".format(
                info["Comp"][int(tid)],
                tid, info["I"][int(k)], keys["rvalue"])

        is_lstig = LSTIG.match(keys["lvalue"])
        if is_lstig and keys["rvalue"] != "2147483647":
            tid, k = is_lstig.group(1), is_lstig.group(2)
            last_return = "lstig"
            return "{} {}:\t{} <~ {}".format(
                info["Comp"][int(tid)],
                tid, info["L"][int(k)], keys["rvalue"])

        is_ltstamp = LTSTAMP.match(keys["lvalue"])
        if is_ltstamp and last_return == "lstig":
            last_return = "ltstamp"
            return " ({})\n".format(keys["rvalue"])

        if (keys["lvalue"].startswith("__LABS_step") and
                keys["rvalue"] != last_step):
            last_return = "step"
            last_step = keys["rvalue"]
            return "--step {}--\n".format(last_step)

        is_env = ENV.match(keys["lvalue"])
        if is_env and keys["rvalue"] != "2147483647":
            k = is_env.group(1)
            last_return = "env"
            return "\t{} <-- {}\n".format(info["E"][int(k)], keys["rvalue"])

        return ""

    except Exception as e:
            print('unable to parse state %s' % keys['State'])
            return ""  # (A,B,C + '\n')
