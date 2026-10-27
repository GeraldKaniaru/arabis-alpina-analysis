# Publishing saved analysis changes

Run this command in a terminal opened in the working `analysis` folder:

```powershell
.\Publish-Updates.cmd
```

From the RStudio console, with `analysis` as the working directory:

```r
system("cmd /c Publish-Updates.cmd")
```

Preview changes without publishing:

```powershell
.\Publish-Updates.cmd --dry-run
```

Save your R script first. To update figures or tables, rerun the relevant R script before publishing. The publisher never runs R code or models. Saving a file alone does not upload it; run the publish command when ready.

The command checks SHA-256 hashes, copies changed current files into the separate public export repository, updates the website catalogue and checksum list, rebuilds the standalone website ZIP, commits and pushes to GitHub. GitHub Pages then rebuilds automatically. The working data and R scripts are read, not modified.

If data, outputs or images changed, only the affected complete folder bundles are rebuilt and uploaded in a new release. Unchanged folder downloads retain their existing version. Large bundles can take several minutes to upload; script-only updates do not upload those bundles. The website's download section identifies each folder's current published version.

The public destination is https://github.com/GeraldKaniaru/arabis-alpina-analysis. Git and GitHub CLI (`gh`) must be installed, and `gh auth login` must use GeraldKaniaru. Existing authentication is reused; credentials are never stored by this publisher. Python 3.9+ is required; the launcher first uses the available bundled Python, then `py -3` or `python`.

RStudio caches, saved workspace sessions/history and Office lock files are excluded. Historical migration records remain in data bundles. The manuscript-figure gallery is tied to the 26 October manuscript and is not regenerated automatically when an R output changes.

If an upload is interrupted, rerun the same command before changing source files. It resumes the prepared publication or reuses verified uploaded assets. If you edit more files while an already prepared publication is pending, finish that publication and run the command again. Pending information is saved beside the export repository in `publication_export/publish-pending.json`.

Missing previously published files stop publication by default. For deliberate removals, inspect the dry run, then use `--allow-removals`. Older published releases remain available.
