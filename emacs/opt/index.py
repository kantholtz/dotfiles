#!/usr/bin/env python3

"""
Build an index file based on my roam hierarchy.

- Requires orgparse and ktz.
- Reading timestamps from filenames.

Sometimes, under windows, it saves org files very
unnecessarily as ISO-8859 encoded files...

mkdir -p backup
for orgfile in (file *org | grep ISO-8859 | cut -d ':' -f 1)
    cp $orgfile backup/
    iconv -f ISO-8859-1 -t utf-8 $orgfile -o $orgfile.tmp
    mv $orgfile.tmp $orgfile
end

"""

import orgparse
from ktz.filesystem import path
from ktz.dataclasses import Builder

import sys
import enum
import argparse
import traceback
from pathlib import Path
from datetime import datetime
from collections import Counter
from collections import defaultdict
from dataclasses import field
from dataclasses import dataclass

from typing import TextIO
from typing import Optional


# ---


def _parse_meta(blob: str):
    meta = {}

    lines = map(str.strip, blob.split("\n"))
    for line in filter(lambda s: s.startswith("#+"), lines):
        key, val = map(str.strip, line.split(":", maxsplit=1))
        meta[key.replace("#+", "")] = val

    return meta


class State(enum.Enum):

    NEXT = "NEXT"
    TODO = "TODO"
    TBDI = "TBDI"
    WAIT = "WAIT"
    POST = "POST"

    DONE = "DONE"
    WONT = "WONT"
    ARCH = "ARCH"

    # aggregations
    ALL_TODO = "ALL_TODO"
    ALL_DONE = "ALL_DONE"


TODO = {State.NEXT, State.TODO, State.TBDI, State.WAIT, State.POST}
DONE = {State.DONE, State.WONT, State.ARCH}


AGGREGATORS = {"Current", "Archive", "Meetings"}


@dataclass
class Org:

    id: str
    created: datetime
    breadcrumbs: tuple[str]

    # statistics
    nodes: int
    states: dict[State, int]

    # additional information
    category: Optional[str]

    @classmethod
    def from_file(Self: "Org", fd: TextIO):

        build = Builder(Klass=Self, immutable=True)
        parsed = orgparse.load(fd)

        # read timestamp from filename
        created = Path(fd.name).name.split("-", maxsplit=1)[0]
        created = datetime.strptime(created, "%Y%m%d%H%M%S")

        build.add(created=created)

        # parsing meta information from root node
        root = parsed[0]

        build.add(id=root.get_property("ID"))
        meta = _parse_meta(root.body)

        build.add(
            breadcrumbs=tuple(meta["title"].split(".")),
            category=meta.get("category", None),
        )

        nodes = 0
        states = Counter()
        for node in parsed[1:]:
            nodes += 1

            # skip aggregator nodes
            if node.heading in AGGREGATORS:
                continue

            if node.todo is not None:
                state = State(node.todo)
                states[state] += 1

                if state in TODO:
                    states[State.ALL_TODO] += 1
                elif state in DONE:
                    states[State.ALL_DONE] += 1
                else:
                    assert False, state

        build.add(nodes=nodes)
        build.add(states=dict(states))

        return build()


# ---


def fail(msg):
    print(msg)
    sys.exit(2)


# no forward references
def _nodefac():
    return defaultdict(Node)


@dataclass
class Node:

    children: dict[str, "Node"] = field(default_factory=_nodefac)
    org: Optional[Org] = None


def read_files(roam_path) -> Node:
    root = Node()

    # build hierarchy
    for org_path in roam_path.glob("*.org"):
        if org_path.name.startswith('.#'):
            continue

        try:
            with org_path.open(mode="r", encoding="utf-8") as fd:
                org = Org.from_file(fd=fd)

                # populate recursively
                place = root
                for crumb in org.breadcrumbs:
                    place = place.children[crumb]
                place.org = org

        except Exception:
            traceback.print_exc()
            fail(f"\nGot exception for {org_path}!")

    return root


def _recurse(buf: list[str], node: Node, name: str, depth: int):
    orgstr = ""
    heading = name

    if node.org is not None:
        org = node.org

        done = org.states.get(State.ALL_DONE, 0)
        total = done + org.states.get(State.ALL_TODO, 0)
        timestamp = org.created.strftime("[%Y-%m-%d %a]")

        heading = f"[[id:{org.id}][{name}]]"
        orgstr = f" {timestamp} ({org.nodes})"
        if total > 0:
            orgstr += f" [{done}/{total}]"

    prefix = "*" * (depth + 1)
    buf.append(f"{prefix} {heading}{orgstr}")

    for key, val in sorted(node.children.items(), key=lambda t: t[0]):
        _recurse(buf=buf, node=val, name=key, depth=depth + 1)


def write_index(index_path: Path, tree: Node):
    with index_path.open(mode="r") as fd:

        content, found = [], False
        for line in map(str.strip, fd.readlines()):
            content.append(line)
            if line == "# --- <<":
                found = True
                break

        assert found, "cannot find marker"

    # TODO currently disabled until ramconfig:roam.sh ignores
    # auto-commits for the index file if only the timestamp changed
    # content.append(f"# generated: {datetime.now()}")
    _recurse(buf=content, node=tree, name="root", depth=1)

    with index_path.open(mode="w") as fd:
        fd.write("\n".join(content))


def main(
    index_path: Path,
    roam_path: Path,
):

    tree = read_files(roam_path=roam_path)
    write_index(index_path=index_path, tree=tree)


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser()

    parser.add_argument(
        "--index",
        type=str,
        required=True,
        help="index file to use",
    )

    parser.add_argument(
        "--roam",
        type=str,
        required=True,
        help="roam directory",
    )

    return parser.parse_args()


if __name__ == "__main__":
    args = parse_args()

    index_path = path(args.index, is_file=True)
    roam_path = path(args.roam, is_dir=True)

    print("building index")
    print(f"  reading from {roam_path}")
    print(f"  writing to {index_path}")
    main(index_path=index_path, roam_path=roam_path)
