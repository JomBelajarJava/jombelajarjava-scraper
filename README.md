# scraper

A scraper for Jom Belajar Java wordpress blog.

### Usage

Load scraper:

```
$ sbcl --load scraper.lisp
```

Scrape:

```
$ (scrape-all-content)
```

The output will be inside `out` folder.
