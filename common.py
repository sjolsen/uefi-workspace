import ast
from collections.abc import Sequence
import dataclasses
import os.path
import subprocess


@dataclasses.dataclass
class Env:
    environ: dict[str, str]
    workspace: str
    basetools: str

    def run(self, args: Sequence[str]) -> subprocess.CompletedProcess:
        return subprocess.run(args, env=self.environ, check=True)


def activate() -> Env:
    parent = os.path.dirname(os.path.abspath(__file__))
    script = os.path.join(parent, 'activate')

    print_env = 'python3 -c "import os; print(dict(os.environ))"'
    command = f'. {script} >/dev/null; exec {print_env}'
    cp = subprocess.run(
        ['sh', '-c', command],
        cwd=parent,
        capture_output=True,
        text=True,
        check=True)
    environ = ast.literal_eval(cp.stdout)

    return Env(
        environ=environ,
        workspace=environ['WORKSPACE'],
        basetools=environ['EDK_TOOLS_PATH'])
