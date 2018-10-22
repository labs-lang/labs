#!/usr/bin/env python3
import begin
import platform
import sys
from subprocess import check_output, DEVNULL, CalledProcessError
from enum import Enum
from os import remove
import uuid
from random import choice

from cex import translateCPROVER, translateCSEQ

SYS = platform.system()


class Backends(Enum):
    CBMC = "cbmc"
    ESBMC = "esbmc"
    CSeq = "cseq"


backend_descr = """choose the verification backend.
Options are: {}""".format(", ".join(b.value for b in Backends))

timeout_descr = """configure time limit (seconds).
A value of 0 means no timeout (default)"""

values_descr = "specify values for parameterised specification (key=value)"

backends = {
    "cbmc": ["cbmc"],
    "esbmc": [
        "esbmc", "--no-bounds-check", "--no-div-by-zero-check",
        "--no-pointer-check", "--no-align-check", "--no-unwinding-assertions",
        "--z3"],
    "cseq": [
        "./cseq.py", "-l", "labs_parallel", "--split", "choice",
        "--cores", "4"]
}

backends_debug = {
    "cbmc": ["cbmc", "--bounds-check" "--signed-overflow-check"],
    "esbmc": [
        "esbmc", "--no-pointer-check", "--no-align-check",
        "--no-unwinding-assertions", "--z3"],
    "cseq": backends["cseq"]
}


def check_cbmc_version():
    cbmc_check = ["cbmc", "--version"]
    CBMC_V, CBMC_SUBV = check_output(cbmc_check).decode().strip().split(".")
    if not (int(CBMC_V) <= 5 and int(CBMC_SUBV) <= 4):
        additional_flags = ["--trace", "--stop-on-fail"]
        backends["cbmc"].extend(additional_flags)
        backends_debug["cbmc"].extend(additional_flags)


cmd = "labs/LabsTranslate"


if "Linux" in SYS:
    env = {"LD_LIBRARY_PATH": "labs/libunwind"}
    TIMEOUT_CMD = "/usr/bin/timeout"
else:
    env = {}
    TIMEOUT_CMD = "/usr/local/bin/gtimeout"


class Spawn:
    def __init__(self, d):
        self._dict = d

    def __getitem__(self, key):
        for (a, b), v in self.items():
            if a <= key < b:
                return v
        raise KeyError

    def values(self):
        return self._dict.values()

    def items(self):
        return self._dict.items()


class Variable:
    def __init__(self, index, name, init):
        self.index = int(index)
        self.name = name
        if init[0] == "[":
            self.values = [int(v) for v in init[1:-1].split(",")]
        elif ".." in init:
            low, up = init.split("..")
            self.values = range(int(low), int(up))
        elif init == "undef":
            self.values = [-32767]  # UNDEF

    def rnd_value(self):
        return choice(self.values)


class Agent:
    def __init__(self, name, iface, lstig):
        self.name = name
        self.iface = {}
        self.lstig = {}

        for txt in iface.split(";"):
            index, *text = txt.split("=")
            self.iface[int(index)] = Variable(int(index), *text)

        for txt in lstig.split(";"):
            index, *text = txt.split("=")
            self.lstig[int(index)] = Variable(int(index), *text)

    def __str__(self):
        return self.name


def split_comps(c):
    result = {}

    for comp, iface, lstig in zip(c[::3], c[1::3], c[2::3]):
        name, rng = comp.split(" ")
        compmin, compmax = rng.split(",")
        result[(int(compmin), int(compmax))] = Agent(name, iface, lstig)

    return Spawn(result)


def gather_info(call):
    call_info = call + ["--info"]
    info = check_output(call_info, env=env)
    # Deserialize system info
    envs, *comps = info.decode().split("\n")
    info = {
        "E":
            [Variable(*v.split("=")) for v in envs.split(";")]
            if envs != ""
            else [],
        "Comp": split_comps(comps)
    }
    info["I"] = {}
    info["L"] = {}
    for c in info["Comp"].values():

        info["I"].update(c.iface)
        info["L"].update(c.lstig)
        print(">>>", instrument_simulation(info), file=sys.stderr)
    return info


def parse_linux(file, values, bound, fair, simulate):
    call = [
        cmd,
        "--file", file,
        "--bound", str(bound)]
    if values:
        call.extend(["--values"] + list(values))
    if fair:
        call.append("--fair")
    if simulate:
        call.append("--simulation")
    try:
        out = check_output(call, env=env)
        fname = str(uuid.uuid4()) + ".c"

        with open(fname, 'wb') as out_file:
            out_file.write(out)
        return out.decode("utf-8"), fname, gather_info(call)
    except CalledProcessError as e:
        print(e, file=sys.stderr)
        return None, None, None


def instrument_simulation(info):
    TYPE = "short"
    e = "\n".join(
        "{} E[{}]={}".format(TYPE, x.index, x.rnd_value())
        for x in info["E"])
    for (low, up), agent in info["Comp"].items():
        for n in range(low, up):
            for v in agent.iface.values():
                e += "\n{} I[{}][{}]={}".format(
                    TYPE, n, v.index, v.rnd_value())
            for v in agent.lstig.values():
                e += "\n{} Lvalue[{}][{}]={}".format(
                    TYPE, n, v.index, v.rnd_value())
    return e


@begin.start(auto_convert=True)
def main(file: "path to LABS file",
         backend: backend_descr = "cbmc",
         steps: "number of system evolutions" = 1,
         fair: "enforce fair interleaving of components" = False,
         simulate: "run in simulation mode" = False,
         show: "print C encoding and exit" = False,
         debug: "enable additional checks in the backend" = False,
         timeout: timeout_descr = 0,
         *values: values_descr):
    """ SLiVER - Symbolyc LAbS VERification. v1.0 (May 2018)
"""
    print("Encoding...", file=sys.stderr)
    c_program, fname, info = parse_linux(file, values, steps, fair, simulate)
    if fname:
        if show:
            print(c_program)
            return
        if backend == "cbmc":
            check_cbmc_version()

        backend_call = backends_debug[backend] if debug else backends[backend]

        if backend == "cseq":
            backend_call.extend(["--steps", str(steps), "-i"])

        backend_call.append(fname)
        if timeout > 0:
            backend_call = [TIMEOUT_CMD, str(timeout)] + backend_call
        try:
            sim_or_verify = "Running simulation" if simulate else "Verifying"
            print(
                "{} with backend {}...".format(sim_or_verify, backend),
                file=sys.stderr)
            out = b''
            out = check_output(backend_call, stderr=DEVNULL)
        except KeyboardInterrupt as err:
            print("Verification stopped (keyboard interrupt)", file=sys.stderr)
        except CalledProcessError as err:
            if err.returncode == 10:
                out = err.output
            elif err.returncode == 1 and backend == "cseq":
                out = err.output
            elif err.returncode == 6:
                print("Backend failed with parsing error.", file=sys.stderr)
            elif err.returncode == 124:
                print(
                    "Timed out after {} seconds"
                    .format(timeout), file=sys.stderr)
            else:
                print(
                    "Unexpected error (return code: {})"
                    .format(err.returncode), file=sys.stderr)
                print(err.output.decode())
        finally:
            out = out.decode("utf-8")

            if ("VERIFICATION SUCCESSFUL" in out):
                print("No properties violated!", end="", file=sys.stderr)
                if simulate:
                    print(" (simulation mode)", file=sys.stderr)
                else:
                    print("", file=sys.stderr)
            else:
                if backend == "cbmc":
                    print(translateCPROVER(out, fname, info))
                elif backend == "cseq":
                    print(translateCSEQ(out, fname, info))
                else:
                    print(out)
            remove(fname)
            if backend == "cseq":
                for suffix in ("", ".map", ".cbmc-assumptions.log"):
                    remove("_cs_" + fname + suffix)
