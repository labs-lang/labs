import re
from modules.info import Info


ATTR = re.compile(r"I\[([0-9]+)l?\]\[([0-9]+)l?\]")
LSTIG = re.compile(r"Lvalue\[([0-9]+)l?\]\[([0-9]+)l?\]")
LTSTAMP = re.compile(r"Ltstamp\[([0-9]+)l?\]\[([0-9]+)l?\]")
ENV = re.compile(r"E\[([0-9]+)l?\]")
STEP = re.compile(r"__LABS_step")

PROPAGATE = re.compile(r"propagate_or_confirm=TRUE")
CONFIRM = re.compile(r"propagate_or_confirm=FALSE")



UNDEF = "16960"
OFFSET = 17


def translateCPROVER(cex, fname, info, offset=-1):
    info = Info.parse(info)

    with open(fname) as f:
        c_program = f.readlines()
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
        elif ln.startswith('Violated property'):
            Y = keys_of(lines[k + 1])
            _, prop = c_program[int(Y["line"]) + offset].split("//")
            translatedcex += """Violated property: {}\n""".format(prop)
            break  # Stop converting after the 1st property has been violated

    if len(translatedcex) > 0:
        translatedcex = "Counterexample:\n\n{}\n".format(translatedcex)

    return translatedcex


def translateCSEQ(cex, fname, info):
    return translateCPROVER(cex, fname, info, 15)


def keys_of(ln):
    tokens = ln.split()
    return {key: value for key, value in zip(tokens[0::2], tokens[1::2])}


last_return = ""
last_step = -1
last_sys = []


def _mapCPROVERstate(A, B, C, info):
    global last_return, last_step, last_sys
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

        if PROPAGATE.match(C.strip()):
            last_sys.append("propagate ")
        elif CONFIRM.match(C.strip()):
            last_sys.append("confirm ")
        elif keys["lvalue"] == "guessedcomp":
            tid = int(keys["rvalue"])
            agent = "from {} {}: ".format(info.spawn[tid], tid)
            last_sys.append(agent)
        elif keys["lvalue"] == "guessedkey":
            last_sys.append(info.lstig[int(keys["rvalue"])].name)
            result = ("".join(last_sys) + "\n")
            last_sys = []
            return result

        is_attr = ATTR.match(keys["lvalue"])
        if is_attr and keys["rvalue"] != UNDEF:
            tid, k = is_attr.group(1), is_attr.group(2)
            last_return = "attr"
            return "{} {}:\t{} <- {}\n".format(
                info.spawn[int(tid)],
                tid, info.i[int(k)].name, keys["rvalue"])

        is_lstig = LSTIG.match(keys["lvalue"])
        if is_lstig and keys["rvalue"] != UNDEF:
            tid, k = is_lstig.group(1), is_lstig.group(2)
            last_return = "lstig"
            return "{} {}:\t{} <~ {}".format(
                info.spawn[int(tid)],
                tid, info.lstig[int(k)].name, keys["rvalue"])

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
        if is_env and keys["rvalue"] != UNDEF:
            k = is_env.group(1)
            last_return = "env"
            return "\t{} <-- {}\n".format(info.e[int(k)].name, keys["rvalue"])

        return ""

    except Exception as e:
            print('unable to parse state %s' % keys['State'])
            return ""  # (A,B,C + '\n')
