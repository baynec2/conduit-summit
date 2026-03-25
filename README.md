
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Conduit-Summit

[![Build and Push Docker
Image](https://github.com/baynec2/conduit-summit/actions/workflows/docker.yml/badge.svg)](https://github.com/baynec2/conduit-summit/actions/workflows/docker.yml)
[![Docker
Pulls](https://img.shields.io/docker/pulls/baynec2/conduit-summit)](https://hub.docker.com/r/baynec2/conduit-summit)

A point-and-click interface for exploring and analyzing metaproteomic
data — no coding required. Upload the `.rds` file produced by
[Conduit-Ascent](https://github.com/baynec2/conduit-ascent) and work
through an eight-step analysis workflow entirely in your browser.

## The Conduit Ecosystem

Conduit-Summit is one part of a three-tool metaproteomics platform:

| Tool | Role |
|----|---|
| [Conduit-Ascent](https://github.com/baynec2/conduit-ascent) | Snakemake command-line workflow. Processes raw mass-spec data, identifies peptides, resolves taxonomy, and produces a structured `.rds` output. |
| **Conduit-Summit** (this repo) | Visual interface for exploring Conduit-Ascent results. All analysis is point-and-click. |
| [ConduitR](https://github.com/baynec2/conduitR) | The R package powering both tools. Available for advanced users who want to build on the framework. |

## Quick Start

The easiest way to run Conduit-Summit is with Docker — no R
installation needed:

``` bash
docker pull baynec2/conduit-summit
docker run -p 3838:3838 baynec2/conduit-summit
```

Then open <http://localhost:3838/conduit-summit/> in your browser and
upload your `.rds` file.

## Analysis Workflow

Once a file is uploaded, eight analysis tabs unlock in sequence:

| Step | Tab | What you can do |
|------|-----|-----------------|
| 1 | **File Upload** | Upload your Conduit-Ascent `.rds` file and preview taxonomic coverage |
| 2 | **Database** | Explore detected species, protein groups, and per-sample summaries |
| 3 | **DIA-NN QC** | Review database search quality metrics |
| 4 | **View Metadata** | Inspect sample annotations and experimental design |
| 5 | **Filter Data** | Subset samples and features; choose log transform, imputation, and normalization |
| 6 | **View Assay** | Inspect the processed quantitative matrix |
| 7 | **Analysis** | QC plots, PCA, heatmaps, differential expression (LIMMA), GO/KEGG enrichment, pathway maps, and outcome prediction |
| 8 | **Traverse** | Follow a feature across assay levels (precursor → peptide → protein → taxon) |

## AI Assistant

The AI tab lets you explore your data and run analyses using
plain-language questions — ask for a volcano plot, request differential
expression between two groups, or get an interpretation of enrichment
results. No code required.

Supported providers: **Anthropic (Claude)**, **OpenAI (GPT)**, **Google
Gemini**, and **Ollama** (fully local, no API key needed). Your API key
is used only within your session and is never stored.

To pre-load an API key via Docker:

``` bash
docker run -p 3838:3838 \
  -e ANTHROPIC_API_KEY=your_key_here \
  baynec2/conduit-summit
```

## Running Locally

**Prerequisites:** R \>= 4.4

``` bash
git clone https://github.com/baynec2/conduit-summit.git
cd conduit-summit
```

``` r
install.packages("renv")
renv::restore()
shiny::runApp()
```

## Contributing

Bug reports and feature requests are welcome — please [open an
issue](https://github.com/baynec2/conduit-summit/issues) and include
your R session info where relevant.
