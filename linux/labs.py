#!/usr/bin/env python3
import begin
import re
from subprocess import check_output, DEVNULL, CalledProcessError
from enum import Enum
from os import remove
import uuid
from cex import translateCPROVER


class Backends(Enum):
    CBMC = "cbmc"
    ESBMC = "esbmc"


backend_descr = """choose the verification backend.
Options are: {}""".format(", ".join(b.value for b in Backends))

timeout_descr = """configure time limit (seconds).
A value of 0 means no timeout (default)"""

backends = {
    "cbmc": ["cbmc"],
    "esbmc": [
        "esbmc", "--no-bounds-check", "--no-div-by-zero-check",
        "--no-pointer-check", "--no-align-check", "--no-unwinding-assertions",
        "--z3"]
}


def unwind(backend, num):
    return {
        "cbmc":
            ["--unwindset",
                "confirm.0:{0},propagate.0:{0},differentLstig.0:{0}".format(
                    num)],
        "esbmc":
            ["--unwindset", "1:{0},2:{0},4:{0}".format(num)]

    }[backend]


cmd = "./LabsTranslate"
env = {"LD_LIBRARY_PATH": "libunwind"}


class Components:
    def __init__(self, d):
        self._dict = d

    def __getitem__(self, key):
        for (a, b), v in self._dict.items():
            if a <= key <= b:
                return v
        raise KeyError


def split_comps(c):
    result = {}
    for comp in c.split(";"):
        name, rng = comp.split(" ")
        compmin, compmax = rng.split(",")
        result[(int(compmin), int(compmax))] = name
    return Components(result)


def gather_info(call):
    call_info = call + ["--info"]
    info = check_output(call_info, env=env)
    # Deserialize system info
    i_names, l_names, e_names, comps, unwind, *_ = info.decode().split("\n")
    info = {
        "I": i_names.split(","),
        "L": l_names.split(","),
        "E": e_names.split(","),
        "Comp": split_comps(comps),
        "unwind": unwind.split(" ")[1]
    }
    return info


def parse_linux(file, values, bound, fair):
    call = [
        cmd,
        "--file", file,
        "--bound", str(bound)]
    if values:
        call.extend(["--values"] + list(values))
    if fair:
        call.append("--fair")
    try:
        out = check_output(call, env=env)
        fname = str(uuid.uuid4()) + ".c"

        with open(fname, 'wb') as out_file:
            out_file.write(out)
        return out.decode("utf-8"), fname, gather_info(call)
    except CalledProcessError as e:
        return None, None, None


def get_functions(c_program):
    isFunc = re.compile(r"^void (\w+)\(int tid\)")

    lines = c_program.split("\n")
    lines = (
        (isFunc.match(l1).group(1), l2.split("// ")[1])
        for l1, l2 in zip(lines, lines[1:])
        if isFunc.match(l1))
    return dict(lines)


@begin.start(auto_convert=True)
def main(file: "path to LABS file",
         backend: backend_descr = "cbmc",
         steps: "number of system evolutions" = 1,
         fair: "enforce fair interleaving of components" = True,
         timeout: timeout_descr = 0,
         *values: "specify the value of placeholders (key=value)"):
    """ Verification of multi-agent systems in the LABS format.
"""
    print("Encoding...")
    c_program, fname, info = parse_linux(file, values, steps, fair)
    if fname:
        backend_call = backends[backend]
        backend_call.append(fname)
        backend_call.extend(unwind(backend, info["unwind"]))
        if timeout > 0:
            backend_call = ["/usr/bin/timeout", str(timeout)] + backend_call
        try:
            print("Verifying with backend {}...".format(backend))
            out = b''
            out = check_output(backend_call, stderr=DEVNULL)
        except KeyboardInterrupt as err:
            print("Verification stopped (keyboard interrupt)")
        except CalledProcessError as err:
            if err.returncode == 10:
                out = err.check_out
            elif err.returncode == 6:
                print("Backend failed with parsing error.")
            elif err.returncode == 124:
                print("Timeout")
        finally:
            out = out.decode("utf-8")
            remove(fname)

            if ("VERIFICATION SUCCESSFUL" in out):
                print("VERIFICATION SUCCESSFUL")
            else:
                print(translateCPROVER(out, c_program, info))
                # cex_cbmc(out, i_names, l_names)

# PASS/FAIL/TIMEOUT/UNKNOWN