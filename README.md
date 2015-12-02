# `readdst`: read Distance for Windows data into R

This package allows one to import data from Distance for Windows "projects" and convert them into analyses one can run in the R package `mrds`.

The `golftees.html` vignette shows how to use most of the functionality of the package.

## Access databases

Distance for Windows uses Access databases to store data, models and results. To use an Access database on Unix and Mac one needs to install [mdbtools](https://github.com/brianb/mdbtools).

### On Debian

```
apt-get install mdbtools
```

Note that Filipe Dias reports that there are issues with `mdbtools` on Linux when using Distance projects with spaces in their filenames. I can't reproduce these on Mac though.

### On Mac

If you have [`homebrew`](http://brew.sh/) installed:

```
brew install mdbtools
```

### On Windows

Support is via `RODBC` -- simply installing the R package should enable use.  Version 0.0.3 has added a dependency on the `readr` package.

Subsequent runs of analyses extracted from `.dst` projects creates dependence of `readdst` upon `mrds`
