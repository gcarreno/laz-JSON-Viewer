# Lazarus JSON Viewer

[![Build Status](https://github.com/gcarreno/laz-JSON-Viewer/workflows/build-test/badge.svg?branch=master)](https://github.com/gcarreno/laz-JSON-Viewer/actions)


Simple application to view a JSON file.

## Setup as default viewer

### Linux (Ubuntu)

The quick and dirty way I did it on my system was:

1. Create a file under `$HOME/.local/share/applications` named `jsonviewer.desktop` with the appropriate contents pointing to where you have uncompressed your binary.
2. Create and entry on `$HOME/.local/share/applications/defaults.list` that looks like:

```ini
[Default Applications]
application/json=jsonviewer.desktop
```

### Windows

At the moment I don't have an answer. Need to consult some Windows gurus.
