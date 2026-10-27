# Arabis alpina: Mount Kenya analysis archive

Data, R scripts and outputs for elevational variation in flowering, maternal seed traits and germination.

## Access the files

- Browse the research website: https://geraldkaniaru.github.io/arabis-alpina-analysis/
- Latest complete folder downloads are listed below under Current complete downloads.
- `catalog.json` lists every published file, its original path, size and SHA-256 checksum.
- Smaller current files are directly browsable in this repository. Climate rasters, historical migration records and files above 5 MB are provided in the release bundles to keep Git operations manageable. Bundles preserve original folder paths.

## Run the analyses

1. Download and extract all three release bundles into the repository root. They restore `data/`, `outputs/` and `images/`.
2. Open `arabis analysis.Rproj` in RStudio. Run all scripts with the project root as the working directory.
3. Install the packages listed in each script. Each script checks its required packages.
4. Run scripts 01–04 for maps and sampling figures; 05a for distributions; 06–10 for models. Run 05b only after 06–10 have saved fitted model objects. Script 05c creates Spearman correlations.
5. Published model outputs are an archived snapshot. Rerunning models, bootstrap simulations or downloaded external products may take time.

## Scope and provenance

Snapshot: 27 October 2026. All files under data/, scripts/, images/ and outputs/ are included. Historical migration records retain older results and are labelled as historical on the website. RStudio caches, workspace session objects, history and Office lock files are excluded. Original data and scientific estimates have not been changed for web publication.

The original private research repository is separate from this public export. This export does not grant a new licence to third-party climate or geographic products; consult the applicable source terms before reuse.

## Current complete downloads

- [Data](https://github.com/GeraldKaniaru/arabis-alpina-analysis/releases/download/analysis-2026-10-26/analysis-data.zip) - 26 October 2026
- [Outputs](https://github.com/GeraldKaniaru/arabis-alpina-analysis/releases/download/untagged-0f5f79a7fb9c23641d49/analysis-outputs.zip) - 27 October 2026
- [Images](https://github.com/GeraldKaniaru/arabis-alpina-analysis/releases/download/analysis-2026-10-26/analysis-images.zip) - 26 October 2026

Run `Publish-Updates.cmd` from your working analysis folder to publish saved changes. See [PUBLISHING.md](PUBLISHING.md). The command never runs R models.
