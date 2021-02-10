# editor

`/e` is the `EDITOR`.

- [/e/?t=../data/csv/images.csv](http://jeremydrape.com/e/?t=../data/csv/images.csv)
- [/e/?t=../data/hs/opt.hs](http://jeremydrape.com/e/?t=../data/hs/opt.hs)
- [/e/?t=../data/md/menu.md](http://jeremydrape.com/e/?t=../data/md/menu.md)

# selectors

`?s` selects the `SERIES`, `-` selects everything.

- [?s=1](http://jeremydrape.com/?s=1)
- [?s=2](http://jeremydrape.com/?s=2)
- [?s=3](http://jeremydrape.com/?s=3)
- [?s=-](http://jeremydrape.com/?s=-)

`?x` selects the `INDEX-SERIES`.

- [?x=z](http://jeremydrape.com/?x=z)
- [?x=-](http://jeremydrape.com/?x=-)

`?m` selects the display `MODE`.

- [?s=1&m=ix](http://jeremydrape.com/?s=1&m=ix)
- [?x=z&m=ix](http://jeremydrape.com/?x=z&m=ix)

# options

- [/e/?t=../data/hs/opt.hs](http://jeremydrape.com/e/?t=../data/hs/opt.hs)

`ix:image-size` is the image size (height in pixels) for the `m=ix` mode, allowed values are: 150, 250, 350, 500.

# data

- [/e/?t=../data/csv/images.csv](http://jeremydrape.com/e/?t=../data/csv/images.csv)

The columns are:

1. `SERIES-ID`.  A letter (ie. `1`, `2`, `3`, `-`)

2. `INDEX-ID`. A letter (ie. `z`, `-`)

3. `INDEX-SORT`. A number that is used to sort files in the index.  Files in the
series are in the order they are in the `images.csv`.  If all
images are at 0 they will be in the order the sorting algorithm uses.

4. `FILE-NAME`.  The image file name, without the `.jpeg` extension.

5. `TITLE`.  The title for the image.  Double quotes allow the title to have
commas, ie. `Crucifixion, 2014`.

# menu

- [?e=data/md/menu.md](http://jeremydrape.com/?e=data/md/menu.md)

# reload

In `Chrome`  reload the style sheet by typing Ctl-Shift-R (ie. Ctl-R for reload +
shift for reload CSS).

# m=ix

The spacing is:

- above and below are randomly between 0 and 300
- left and right are randomly between 0 and 150

# image selector

The image selector is `?i=` and uses the `FILE-NAME`.  It can either
be an exact match to select one file, or a partial match to select
multiple files.

- [?i=DD_36](http://jeremydrape.com/?i=DD_36)
- [?i=DD](http://jeremydrape.com/?i=DD)

# directory structure

~~~~
css                css files
data               data files (csv,hs,jpeg,md)
error              http error code files
hs/*.hs            haskell code (current)
hs/jrd-f           obsolete (haskell code)
o                  obsolete (previous design)
scm                obsolete (scheme code)
sh                 shell scripts
theindex           obsolete
~~~~

# monitor resolutions

5120×2880 ; iMac 27" Retina
4096×3072
4096×2160
3240×2160 ;
3000×2000 ;
2960×1440 ; S8, S9
2880×1800 ; MacBook Pro 15" Retina
2800×1752 ; S7+
2732×2048 ; iPad Pro 12.9"
2560×1600
2560×1440 ; x1
2436×1125 ; iPhone X
2388×1668 ; iPad Pro 11"
2360×1640 ; iPad Air
2160×1620 ; iPad 10.2
2048×1556
2048×1536
2048×1152
2048×872
1920×1200
1920×1080 ; x1
1680×1050
1600×1200 ; T60
1600×900
1440×960  ; PowerBook G4
1440×900
1400×1050 ; x61
1366×768  ; x270
1280×1024
1280×800
1152×768  ; PowerBook G4
1120×832  ; NeXT
1024×768
800×600
720×480
