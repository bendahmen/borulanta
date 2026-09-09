#!/usr/bin/env Rscript

# Regenerate manifest.json, which is how Connect Cloud knows what to install.
#
# Run this whenever renv.lock changes. The two describe the same environment and
# nothing checks that they agree, so a lockfile updated without a manifest to
# match deploys the packages the manifest last remembered rather than the ones
# renv now pins.
#
#   Rscript scripts/write-manifest.R
#
# The file list is written out rather than left to rsconnect's default sweep,
# for one reason that matters: renv's .Rprofile must not ship. On Connect the
# packages come from the manifest, and an .Rprofile that activates renv points
# .libPaths() at a project library that does not exist there, hiding every one
# of them. Locally it is exactly what we want, which is why it stays in the
# repository and out of the bundle.

suppressPackageStartupMessages(library(rsconnect))

# renv.lock earns its place in the bundle by changing how the manifest is
# built, not by being read at run time: with it here rsconnect pins every
# version from the lockfile, and without it falls back to scanning the code and
# resolving against whatever happens to be installed. Same answer today, but
# only the first is a guarantee.
app_files <- c(
  "app.R",
  "renv.lock",
  list.files("R", full.names = TRUE),
  list.files("data", recursive = TRUE, full.names = TRUE),
  list.files("www", full.names = TRUE)
)

writeManifest(appDir = ".", appPrimaryDoc = "app.R", appFiles = app_files)

manifest <- jsonlite::fromJSON("manifest.json")
cat(
  "manifest.json:", length(manifest$files), "files,",
  length(manifest$packages), "packages, R", manifest$platform, "\n"
)
