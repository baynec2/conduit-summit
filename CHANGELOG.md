# Changelog

All notable changes to conduit-summit are recorded here. Versions follow
[Semantic Versioning](https://semver.org/).

## [0.1.0] - 2026-10-07

First tagged release. The Docker image is published as `baynec2/conduit-summit:0.1.0`.

- Upload a conduit-ascent `.rds` file (or a DIA-NN/FragPipe `.parquet`) and work through
  eight tabs: file upload, database, DIA-NN QC, metadata, filtering, assay view,
  analysis, and traverse
- Analysis: QC plots, PCA, heatmaps, differential expression (limma), GO/KEGG
  enrichment, pathway maps, and outcome prediction
- Traverse a feature across assay levels (precursor → peptide → protein → taxon)
- AI assistant tab supporting Anthropic, OpenAI, Google Gemini and local Ollama models
- Runs from Docker, locally with renv, or inside conduit-basecamp
- Released under the MIT License

[0.1.0]: https://github.com/baynec2/conduit-summit/releases/tag/v0.1.0
