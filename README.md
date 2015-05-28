# `readdst`: read Distance for Windows data into R

This package allows one to import data from Distance for Windows "projects" and convert them into analyses one can run in the R package `mrds`.

## Access databases

Distance for Windows uses Access databases to store data, models and results. To use an Access database on Unix and Mac one needs to install [mdbtools](https://github.com/brianb/mdbtools).

### On Debian

```
apt-get install mdbtools
```

### On Mac

If you have [`homebrew`](http://brew.sh/) installed:

```
brew install mdbtools
```

### On Windows

Not supported at the moment, but will be in the future via RODBC.
