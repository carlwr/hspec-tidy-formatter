_Capture terminal screenshots and produce a side-by-side montage of them._

Invoke:
```sh
cd ${repoRoot}
make -f scripts/montage/Makefile              # (re)make if needed
make -f scripts/montage/Makefile clean all    # always remake
```

Copy plaintext montage to cliboard, for pasting into README.md comment:
```sh
< .aux/montage.txt pbcopy
```

Callchain:
* `Makefile` -> `capture` -> `runTest`

Requirements:
* zsh
* freeze (by charm.sh)
* imagemagick
* optipng
