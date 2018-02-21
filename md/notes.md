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
