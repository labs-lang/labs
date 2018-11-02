#!/usr/bin/env python3
import begin
import platform
import sys
from subprocess import check_output, DEVNULL, CalledProcessError
from enum import Enum
from os import remove
import uuid

from cex import translateCPROVER, translateCSEQ
from modules.info import raw_info


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

simulate_descr = """number of simulation traces to analyze.
0 = run in verification mode (default)
"""

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


if "Linux" in SYS:
    env = {"LD_LIBRARY_PATH": "labs/libunwind"}
    TIMEOUT_CMD = "/usr/bin/timeout"
else:
    env = {}
    TIMEOUT_CMD = "/usr/local/bin/gtimeout"


def parse_linux(file, values, bound, fair, simulate):
    call = [
        "labs/LabsTranslate",
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
        return out.decode("utf-8"), fname, raw_info(call)
    except CalledProcessError as e:
        print(e, file=sys.stderr)
        return None, None, None


@begin.start(auto_convert=True)
def main(file: "path to LABS file",
         backend: backend_descr = "cbmc",
         steps: "number of system evolutions" = 1,
         fair: "enforce fair interleaving of components" = False,
         simulate: simulate_descr = 0,
         show: "print C encoding and exit" = False,
         debug: "enable additional checks in the backend" = False,
         timeout: timeout_descr = 0,
         *values: values_descr):
    """ SLiVER - Symbolyc LAbS VERification. v1.0 (May 2018)
"""
    print("Encoding...", file=sys.stderr)
    c_program, fname, info = parse_linux(file, values, steps, fair, simulate)
    info = info.decode().replace("\n", "|")[:-1]
    if fname:
        if show:
            print(c_program)
            return

        if simulate:
            backend = "cseq"

        if backend == "cbmc":
            check_cbmc_version()

        backend_call = backends_debug[backend] if debug else backends[backend]

        if backend == "cseq":
            backend_call.extend(["--steps", str(steps), "-i"])

        backend_call.append(fname)

        if simulate:
            backend_call.extend([
                "--simulate", str(simulate),
                "--info", info])

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
                if backend == "cbmc" and not simulate:
                    print(translateCPROVER(out, fname, info))
                elif backend == "cseq" and not simulate:
                    print(translateCSEQ(out, fname, info))
                else:
                    print(out)
            try:
                remove(fname)
                if backend == "cseq":
                    for suffix in ("", ".map", ".cbmc-assumptions.log"):
                        remove("_cs_" + fname + suffix)
            except FileNotFoundError:
                pass
