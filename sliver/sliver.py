#!/usr/bin/env python3
import click
import platform
import sys
from subprocess import check_output, DEVNULL, CalledProcessError
from enum import Enum
from os import remove
import uuid
from pathlib import Path

from cex import translateCPROVER
from .info import raw_info
from __about__ import __date__, __summary__, __version__

SYS = platform.system()
__DIR = Path(__file__).parent


class Backends(Enum):
    CBMC = "cbmc"
    ESBMC = "esbmc"
    CSeq = "cseq"


class Languages(Enum):
    C = "c"
    LNT = "lnt"


SHORTDESCR = "SLiVER"
LONGDESCR = """
* * * {} - {} v{} ({}) * * *

FILE -- path of LABS file to analyze

VALUES -- assign values for parameterised specification (key=value)
""".format(SHORTDESCR, __summary__, __version__, __date__)

HELPMSG = {
    "backend": "Backend to use in verification mode.",

    "bitvector":
        "Enable bitvector optimization where supported",

    "cores": "Number of CPU cores for parallel analysis",

    "debug": "Enable additional checks in the backend.",

    "lang": "Target language for the code generator.",

    "fair": "Enforce fair interleaving of components.",

    "show": "Print C encoding and exit.",

    "simulate": (
        "Number of simulation traces to analyze. "
        "If 0, run in verification mode."),

    "steps": (
        "Number of system evolutions."
        "If 0, generate an unbounded system."),

    "sync": "Force synchronous stigmergy messages.",

    "timeout": (
        "Configure time limit (seconds)."
        "Set to 0 to disable timeout.")
}

backends = {
    "cbmc": ["/usr/local/bin/cbmc5.4"],
    "esbmc": [
        "esbmc", "--no-bounds-check", "--no-div-by-zero-check",
        "--no-pointer-check", "--no-align-check",
        "--no-unwinding-assertions", "--z3"],
    "cseq": [
        "./cseq.py", "-l", "labs_parallel", "--split", "_I"]
}


backends_debug = {
    "cbmc":
        backends["cbmc"] + ["--bounds-check", "--signed-overflow-check"],
    "esbmc": [
        "esbmc", "--no-pointer-check", "--no-align-check",
        "--no-unwinding-assertions", "--z3"],
    "cseq": backends["cseq"]
}


def check_cbmc_version():
    cbmc_check = backends["cbmc"] + ["--version"]
    CBMC_V, CBMC_SUBV = check_output(
        cbmc_check, cwd=__DIR).decode().strip().split(".")
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


def parse_linux(file, values, bound, fair, simulate, bv, sync, lang):
    call = [
        __DIR / Path("labs/LabsTranslate"),
        "--file", file,
        "--bound", str(bound),
        "--enc", lang]
    flags = [
        (fair, "--fair"), (simulate, "--simulation"),
        (not bv, "--no-bitvector"), (sync, "--sync")
    ]

    if values:
        call.extend(["--values"] + list(values))
    for a, b in flags:
        if a:
            call.append(b)
    try:
        out = check_output(call, env=env)
        fname = make_filename(file, values, bound, fair, sync)
        with open(fname, 'wb') as out_file:
            out_file.write(out)
        return out.decode("utf-8"), fname, raw_info(call)
    except CalledProcessError as e:
        print(e, file=sys.stderr)
        return None, None, None


def make_filename(file, values, bound, fair, sync):
    result = "_".join((
        file[:-5],
        str(bound), ("fair" if fair else "unfair"),
        ("sync" if sync else ""),
        "".join(v.replace("=", "") for v in values),
        str(uuid.uuid4())[:6])) + ".c"
    return result.replace("__", "_")


def cleanup(fname, backend):
    try:
        remove(fname)
        if backend == "cseq":
            for suffix in ("", ".map", ".cbmc-assumptions.log"):
                remove("_cs_" + fname + suffix)
    except FileNotFoundError:
        pass


def DEFAULTS(name):
    return {
        "help": HELPMSG[name],
        "show_default": True
    }


@click.command()
@click.version_option(__version__, prog_name=SHORTDESCR)
@click.argument('file', required=True, type=click.Path(exists=True))
@click.argument('values', nargs=-1)
@click.option(
    '--backend',
    type=click.Choice(b.value for b in Backends),
    default=Backends.CBMC.value, **DEFAULTS("backend"))
@click.option('--debug', default=False, is_flag=True, **DEFAULTS("debug"))
@click.option('--fair/--no-fair', default=False, **DEFAULTS("fair"))
@click.option('--bv/--no-bv', default=True, **DEFAULTS("bitvector"))
@click.option('--simulate', default=0, type=int, **DEFAULTS("simulate"))
@click.option('--show', default=False, is_flag=True, **DEFAULTS("show"))
@click.option('--steps', default=0, type=int, **DEFAULTS("steps"))
@click.option('--sync/--no-sync', default=False, **DEFAULTS("sync"))
@click.option('--timeout', default=0, type=int, **DEFAULTS("timeout"))
@click.option('--cores', default=4, type=int, **DEFAULTS("cores"))
@click.option(
    '--lang',
    type=click.Choice(l.value for l in Languages),
    default=Languages.C.value, **DEFAULTS("lang"))
def main(file, backend, steps, fair, bv, simulate, show, sync, values, timeout,
         debug, cores, lang):
    """
* * *  SLiVER - Symbolic LAbS VERification. v1.3 (July 2019) * * *

FILE -- path of LABS file to analyze

VALUES -- assign values for parameterised specification (key=value)
"""

    print("Encoding...", file=sys.stderr)
    c_program, fname, info = parse_linux(
        file, values, steps, fair, simulate, bv, sync, lang)
    info = info.decode().replace("\n", "|")[:-1]
    if fname:
        if show:
            print(c_program)
            cleanup(fname, backend)
            return

        if simulate:
            backend = "cseq"

        if backend == "cbmc":
            check_cbmc_version()

        backend_call = backends_debug[backend] if debug else backends[backend]

        if backend == "cseq":
            backend_call.extend(["--cores", str(cores)])
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
            out = check_output(backend_call, stderr=DEVNULL, cwd=__DIR)
        except KeyboardInterrupt:
            print("Verification stopped (keyboard interrupt)", file=sys.stderr)
            out = b""
        except CalledProcessError as err:
            out = b""
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
                if backend == "cseq" and not simulate:
                    print(translateCPROVER(out, fname, info, 19))
                elif not simulate:
                    print(translateCPROVER(out, fname, info))
                else:
                    print(out)
            cleanup(fname, backend)


if __name__ == "__main__":
    main()
