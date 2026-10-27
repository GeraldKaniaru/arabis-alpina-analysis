"""Publish the working analysis folder to its public GitHub research archive.
No R models are run. Python's standard library, Git and GitHub CLI are required.
"""
from __future__ import annotations
import argparse, hashlib, json, os, re, shutil, subprocess, sys, tempfile, zipfile
from datetime import datetime, timedelta, timezone
from pathlib import Path
from urllib.parse import quote

OWNER_REPO = "GeraldKaniaru/arabis-alpina-analysis"
REPO_URL = "https://github.com/" + OWNER_REPO
GROUPS = ("data", "outputs", "images")
ROOT_FILES = ("arabis analysis.Rproj", "publish_updates.py", "Publish-Updates.cmd", "PUBLISHING.md")
SMALL_LIMIT = 5_000_000
NAIROBI = timezone(timedelta(hours=3))


def sha256(path):
    digest = hashlib.sha256()
    with Path(path).open("rb") as handle:
        for chunk in iter(lambda: handle.read(8 * 1024 * 1024), b""):
            digest.update(chunk)
    return digest.hexdigest()


def read_json(path):
    return json.loads(Path(path).read_text(encoding="utf-8-sig"))


def write_json(path, value):
    path = Path(path)
    path.parent.mkdir(parents=True, exist_ok=True)
    temporary = path.with_name(path.name + ".publishing-tmp")
    temporary.write_text(json.dumps(value, ensure_ascii=False, indent=2), encoding="utf-8")
    temporary.replace(path)


def source_files(source):
    paths = []
    for directory in ("scripts",) + GROUPS:
        folder = source / directory
        if not folder.is_dir():
            raise RuntimeError("Required analysis folder is missing: " + str(folder))
        paths.extend(p for p in folder.rglob("*") if p.is_file() and not p.name.startswith("~$"))
    paths.extend(source / name for name in ROOT_FILES if (source / name).is_file())
    return sorted(paths, key=lambda p: p.relative_to(source).as_posix())


def inspect_source(source, previous):
    old = {record["path"]: record for record in previous["files"]}
    records, changed = [], []
    paths = source_files(source)
    for path in paths:
        relative = path.relative_to(source).as_posix()
        digest = sha256(path)
        size = path.stat().st_size
        group = relative.split("/")[0] if "/" in relative else "project"
        small = size <= SMALL_LIMIT and not relative.startswith(("data/climate/", "data/migration_records/"))
        record = dict(path=relative, name=path.name, category=group, size=size,
                      extension=path.suffix.lower().lstrip("."), sha256=digest,
                      inRepository=small)
        records.append(record)
        if relative not in old or digest != old[relative]["sha256"]:
            changed.append(relative)
            if path.suffix.lower() in (".r", ".py", ".json", ".yaml", ".yml", ".env") and size < 2_000_000:
                text = path.read_text(encoding="utf-8", errors="ignore")
                if re.search(r"gh[pousr]_[A-Za-z0-9]{30,}|AKIA[0-9A-Z]{16}|-----BEGIN (?:RSA |OPENSSH )?PRIVATE KEY-----", text):
                    raise RuntimeError("Possible credential in changed file; publishing stopped: " + relative)
    present = {record["path"] for record in records}
    removed = sorted(set(old) - present)
    groups = sorted({p.split("/")[0] for p in changed + removed if p.split("/")[0] in GROUPS})
    return records, changed, removed, groups


def safe_file(root, relative):
    target = (root / relative).resolve()
    if not target.is_relative_to(root.resolve()) or target == root.resolve():
        raise RuntimeError("File path escapes the configured folder: " + relative)
    return target


def build_bundle(source, records, group, destination):
    selected = [r for r in records if r["category"] == group]
    destination.parent.mkdir(parents=True, exist_ok=True)
    temporary = destination.with_suffix(".building.zip")
    try:
        with zipfile.ZipFile(temporary, "w", zipfile.ZIP_DEFLATED, compresslevel=6, allowZip64=True) as archive:
            for index, record in enumerate(selected, 1):
                path = safe_file(source, record["path"])
                digest = hashlib.sha256()
                with path.open("rb") as handle, archive.open(record["path"], "w", force_zip64=True) as member:
                    for chunk in iter(lambda: handle.read(4 * 1024 * 1024), b""):
                        digest.update(chunk)
                        member.write(chunk)
                if digest.hexdigest() != record["sha256"]:
                    raise RuntimeError("File changed while packaging; save your work and retry: " + record["path"])
                if index % 100 == 0:
                    print(f"  {group}: packaged {index}/{len(selected)} files", flush=True)
        with zipfile.ZipFile(temporary) as archive:
            if archive.testzip() is not None:
                raise RuntimeError("ZIP verification failed: " + str(temporary))
        temporary.replace(destination)
    finally:
        if temporary.exists():
            temporary.unlink()
    return dict(bytes=destination.stat().st_size, sha256=sha256(destination), files=len(selected))


def copy_verified(source, destination, expected):
    if source.resolve() == destination.resolve():
        return
    destination.parent.mkdir(parents=True, exist_ok=True)
    temporary = destination.with_name(destination.name + ".publishing-tmp")
    shutil.copyfile(source, temporary)
    if sha256(temporary) != expected:
        temporary.unlink()
        raise RuntimeError("Source changed during copying: " + str(source))
    temporary.replace(destination)


def build_website_download(repo, archive_path):
    docs = repo / "docs"
    archive_path.parent.mkdir(parents=True, exist_ok=True)
    with tempfile.TemporaryDirectory(prefix="arabis-website-") as folder:
        target = Path(folder) / "Arabis_alpina_website"
        shutil.copytree(docs, target)
        catalog = read_json(target / "catalog.json")
        (target / "catalog.js").write_text("window.ARABIS_CATALOG = " + json.dumps(catalog, ensure_ascii=False) + ";\n", encoding="utf-8")
        index = target / "index.html"
        index.write_text(index.read_text(encoding="utf-8-sig").replace('<script src="app.js" defer></script>', '<script src="catalog.js"></script><script src="app.js" defer></script>'), encoding="utf-8")
        app = target / "app.js"
        app.write_text(app.read_text(encoding="utf-8-sig").replace("fetch('catalog.json').then(r=>{if(!r.ok)throw Error(r.status);return r.json();})", "(window.ARABIS_CATALOG ? Promise.resolve(window.ARABIS_CATALOG) : fetch('catalog.json').then(r=>{if(!r.ok)throw Error(r.status);return r.json();}))"), encoding="utf-8")
        (target / "OPEN_ME.txt").write_text("Extract this ZIP, then open index.html. The website and catalogue work locally. Internet access is required for research-file downloads from GitHub.\n", encoding="utf-8")
        with zipfile.ZipFile(archive_path, "w", zipfile.ZIP_DEFLATED, compresslevel=9) as archive:
            for path in target.rglob("*"):
                if path.is_file():
                    archive.write(path, path.relative_to(Path(folder)).as_posix())


class Publisher:
    def __init__(self, source, repo):
        self.source = source.resolve()
        self.repo = repo.resolve()
        self.export = self.repo.parent
        self.state_file = self.export / "publish-pending.json"

    def run(self, args, capture=True, check=True):
        result = subprocess.run(args, cwd=self.repo, text=True, encoding="utf-8", errors="replace",
                                stdout=subprocess.PIPE if capture else None,
                                stderr=subprocess.PIPE if capture else None)
        if check and result.returncode:
            message = (result.stderr or result.stdout or "Command failed").strip()
            raise RuntimeError(message)
        return result

    def git(self, *args, **kwargs):
        return self.run(["git", "-c", "safe.directory=" + str(self.repo),
                         "-c", "credential.helper=", "-c", "credential.helper=!gh auth git-credential", *args], **kwargs)

    def gh(self, *args, **kwargs):
        return self.run(["gh", *args], **kwargs)

    def old_catalog(self):
        return json.loads(self.git("show", "HEAD:catalog.json").stdout)

    def finish(self, state):
        if state["phase"] == "prepared":
            self.git("add", "--", *state["paths"])
            if self.git("diff", "--cached", "--quiet", check=False).returncode:
                self.git("commit", "-m", state["message"], capture=False)
            state["commit"] = self.git("rev-parse", "HEAD").stdout.strip()
            state["phase"] = "committed"
            write_json(self.state_file, state)
        print("Pushing the updated scripts, files and website catalogue...", flush=True)
        self.git("push", "origin", "HEAD:main", capture=False)
        if state.get("tag"):
            self.gh("release", "edit", state["tag"], "--repo", OWNER_REPO, "--draft=false", "--latest", capture=False)
        self.state_file.unlink()
        print("Published successfully: https://geraldkaniaru.github.io/arabis-alpina-analysis/", flush=True)
        print("GitHub Pages will rebuild automatically. Website ZIP: " + str(self.export / "Arabis_alpina_website.zip"), flush=True)

    def publish(self, message=None, allow_removals=False):
        login = self.gh("api", "user", "--jq", ".login").stdout.strip()
        if login.lower() != "geraldkaniaru":
            raise RuntimeError("GitHub CLI is signed in to a different account. Expected GeraldKaniaru.")
        remote = self.git("remote", "get-url", "origin").stdout.strip()
        if remote.rstrip("/").removesuffix(".git").lower() != REPO_URL.lower():
            raise RuntimeError("Unexpected Git remote; refusing to publish to " + remote)
        pending = read_json(self.state_file) if self.state_file.exists() else None
        if pending and pending["phase"] in ("prepared", "committed"):
            print("Resuming the previously prepared publication. Run again afterwards if you made further source edits.", flush=True)
            self.finish(pending)
            return
        if self.git("status", "--porcelain").stdout.strip():
            raise RuntimeError("The export repository has uncommitted changes. Inspect or commit them before publishing.")
        self.git("fetch", "origin", "main")
        if self.git("rev-parse", "HEAD").stdout != self.git("rev-parse", "origin/main").stdout:
            self.git("merge", "--ff-only", "origin/main", capture=False)
        previous = self.old_catalog()
        print("Checking source files and SHA-256 checksums...", flush=True)
        records, changed, removed, groups = inspect_source(self.source, previous)
        show_plan(changed, removed, groups)
        if removed and not allow_removals:
            raise RuntimeError("Previously published files are missing locally. Restore them, or use --allow-removals for intentional removals.")
        if not changed and not removed:
            print("No analysis changes to publish.")
            return
        now = datetime.now(NAIROBI)
        snapshot = now.strftime("%d %B %Y").lstrip("0")
        fingerprint = hashlib.sha256(json.dumps(records, sort_keys=True).encode()).hexdigest()
        if pending and pending["fingerprint"] != fingerprint:
            raise RuntimeError("Source files changed since the interrupted upload. Inspect publish-pending.json before retrying; its draft release is preserved.")
        state = pending or dict(phase="uploading", fingerprint=fingerprint,
                                tag="analysis-update-" + now.strftime("%Y%m%d-%H%M%S") if groups else None,
                                bundles={})
        write_json(self.state_file, state)
        bundles = dict(previous.get("bundles", {}))
        if groups:
            exists = self.gh("release", "view", state["tag"], "--repo", OWNER_REPO, check=False)
            if exists.returncode:
                self.gh("release", "create", state["tag"], "--repo", OWNER_REPO, "--draft",
                        "--title", "Analysis update - " + snapshot,
                        "--notes", "Complete updated folder bundles. Unchanged folders remain available through the website's current download links. R models were not run by the publisher.", capture=False)
            for group in groups:
                asset = "analysis-" + group + ".zip"
                local = self.export / "releases" / state["tag"] / asset
                cached = state["bundles"].get(group)
                if not cached or not local.exists() or sha256(local) != cached["sha256"]:
                    print("Building complete updated " + group + " bundle...", flush=True)
                    cached = build_bundle(self.source, records, group, local)
                    state["bundles"][group] = cached
                    write_json(self.state_file, state)
                published = json.loads(self.gh("release", "view", state["tag"], "--repo", OWNER_REPO, "--json", "assets").stdout)
                present = next((a for a in published["assets"] if a["name"] == asset and a["state"] == "uploaded" and a["size"] == cached["bytes"]), None)
                if not present:
                    print(f"Uploading {asset}: {cached['bytes']/1e6:.1f} MB. Large bundles can take several minutes.", flush=True)
                    self.gh("release", "upload", state["tag"], str(local), "--repo", OWNER_REPO, "--clobber", capture=False)
                release_id = self.gh("release", "view", state["tag"], "--repo", OWNER_REPO, "--json", "databaseId", "--jq", ".databaseId").stdout.strip()
                release = json.loads(self.gh("api", "repos/" + OWNER_REPO + "/releases/" + release_id).stdout)
                item = next(a for a in release["assets"] if a["name"] == asset)
                if item["size"] != cached["bytes"] or item.get("digest") != "sha256:" + cached["sha256"]:
                    raise RuntimeError("Uploaded archive checksum does not match: " + asset)
                bundles[group] = dict(cached, url=item["browser_download_url"], snapshot=snapshot, tag=state["tag"], asset=asset)
        old = {r["path"]: r for r in previous["files"]}
        managed = set()
        for record in records:
            relative = record["path"]
            group = record["category"]
            record["bundle"] = "analysis-" + group + ".zip" if group in GROUPS else None
            if record["inRepository"]:
                record["download"] = "https://raw.githubusercontent.com/" + OWNER_REPO + "/main/" + quote(relative, safe="/")
                record["view"] = REPO_URL + "/blob/main/" + quote(relative, safe="/")
                if relative in changed or relative not in old or not old[relative]["inRepository"]:
                    copy_verified(safe_file(self.source, relative), safe_file(self.repo, relative), record["sha256"])
                    managed.add(relative)
            else:
                record["download"] = bundles[group]["url"]
                record["view"] = record["download"]
                if old.get(relative, {}).get("inRepository"):
                    target = safe_file(self.repo, relative)
                    if target.exists(): target.unlink()
                    managed.add(relative)
        for relative in removed:
            if old[relative]["inRepository"]:
                target = safe_file(self.repo, relative)
                if target.exists(): target.unlink()
                managed.add(relative)
        catalog = dict(previous, files=records, bundles=bundles, snapshot=snapshot,
                       lastPublishedAt=now.isoformat(), release=REPO_URL + "/releases")
        for relative in ("catalog.json", "docs/catalog.json"):
            write_json(self.repo / relative, catalog)
            managed.add(relative)
        (self.repo / "SHA256SUMS.txt").write_text("".join(r["sha256"] + "  " + r["path"] + "\n" for r in records), encoding="utf-8")
        managed.add("SHA256SUMS.txt")
        readme = self.repo / "README.md"
        text = readme.read_text(encoding="utf-8-sig")
        text = re.sub(r"Snapshot: \d{1,2} \w+ \d{4}\.", "Snapshot: " + snapshot + ".", text)
        marker = "\n## Current complete downloads\n"
        text = re.sub(r"- Download full data, outputs and image bundles:.*", "- Latest complete folder downloads are listed below under Current complete downloads.", text)
        text = text.split(marker)[0]
        text += marker + "\n" + "\n".join("- [" + group.title() + "](" + bundles[group]["url"] + ") - " + bundles[group]["snapshot"] for group in GROUPS)
        text += "\n\nRun `Publish-Updates.cmd` from your working analysis folder to publish saved changes. See [PUBLISHING.md](PUBLISHING.md). The command never runs R models.\n"
        readme.write_text(text, encoding="utf-8")
        managed.add("README.md")
        build_website_download(self.repo, self.export / "Arabis_alpina_website.zip")
        state.update(phase="prepared", paths=sorted(managed), message=message or "Publish saved analysis changes - " + snapshot)
        write_json(self.state_file, state)
        self.finish(state)


def show_plan(changed, removed, groups):
    print(f"Changed/new files: {len(changed)}; missing files: {len(removed)}")
    for path in changed: print("  UPDATE " + path)
    for path in removed: print("  REMOVE " + path)
    print("Complete bundles to refresh: " + (", ".join(groups) or "none (script-only updates are quick)"), flush=True)


def self_test():
    with tempfile.TemporaryDirectory() as directory:
        source = Path(directory) / "source"
        for group in ("scripts",) + GROUPS: (source / group).mkdir(parents=True)
        (source / "scripts/test.R").write_text("x <- 1\n")
        (source / "outputs/test.txt").write_text("result\n")
        records, changed, removed, groups = inspect_source(source, {"files": []})
        assert len(changed) == 2 and groups == ["outputs"] and not removed
        previous = {"files": records}
        assert inspect_source(source, previous)[1:] == ([], [], [])
        (source / "scripts/test.R").write_text("x <- 2\n")
        assert inspect_source(source, previous)[3] == []
        destination = Path(directory) / "bundle.zip"
        record = build_bundle(source, records, "outputs", destination)
        assert record["files"] == 1
        with zipfile.ZipFile(destination) as archive: assert archive.read("outputs/test.txt") == (source / "outputs/test.txt").read_bytes()
        try: safe_file(source, "../escape.txt")
        except RuntimeError: pass
        else: raise AssertionError("Path guard failed")
        (source / "outputs/test.txt").write_text("changed\n")
        try: build_bundle(source, records, "outputs", destination)
        except RuntimeError: pass
        else: raise AssertionError("Mutation guard failed")
    print("Self-test passed: change detection, script-only updates, bundle content, path and mutation guards.")


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--publish", action="store_true", help="Commit, push and publish saved changes")
    parser.add_argument("--dry-run", action="store_true", help="Show changes without modifying files or contacting GitHub")
    parser.add_argument("--allow-removals", action="store_true", help="Publish intentional source-file removals")
    parser.add_argument("--message", help="Optional Git commit message")
    parser.add_argument("--repo-path", type=Path, help="Override the export repository location")
    parser.add_argument("--self-test", action="store_true")
    args = parser.parse_args()
    if args.self_test:
        self_test(); return
    source = Path(__file__).resolve().parent
    repo = args.repo_path or source.parent / "publication_export/github"
    if not (repo / ".git").exists():
        raise RuntimeError("Export repository not found: " + str(repo) + ". Supply --repo-path if it was moved.")
    publisher = Publisher(source, repo)
    if args.publish and not args.dry_run:
        publisher.publish(args.message, args.allow_removals)
    else:
        print("Dry run: no uploads or file changes.")
        records, changed, removed, groups = inspect_source(source, publisher.old_catalog())
        show_plan(changed, removed, groups)


if __name__ == "__main__":
    if hasattr(sys.stdout, "reconfigure"):
        sys.stdout.reconfigure(encoding="utf-8")
        sys.stderr.reconfigure(encoding="utf-8")
    try:
        main()
    except (RuntimeError, OSError, subprocess.SubprocessError, zipfile.BadZipFile) as error:
        print("PUBLISH STOPPED: " + str(error), file=sys.stderr)
        print("Your working analysis files have not been changed. Fix the reported issue and retry.", file=sys.stderr)
        sys.exit(1)
