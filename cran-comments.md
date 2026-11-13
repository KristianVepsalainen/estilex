## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

The note reports one URL as possibly invalid:

```
URL: https://eur-lex.europa.eu/eli-register/what_is_eli.html
  From: inst/doc/estilex.html
  Status: 202
  Message: Accepted
```

The page exists and the link is correct. EUR-Lex answers automated requests
with 202 Accepted rather than 200, so the check cannot confirm it. Opening
the address in a browser resolves normally.

## Network use

The package is a client for the Riigi Teataja public API, so some care was
taken to keep the check free of network access:

* All examples are wrapped in `if (FALSE)`, so none of them makes a request.
* Tests that need the service call `skip_on_cran()` and `skip_if_offline()`.
  The majority of the suite runs offline against synthetic documents.
* The vignette is pre-computed. `vignettes/estilex.Rmd.orig` holds the live
  calls and is excluded from the build; only the knitted result is shipped.
* The package can download bulk archives on request. Nothing downloads
  unless the user calls `etx_download_corpus()`, no example or test does so,
  archives default to `tempdir()`, and a size guard stops a non-interactive
  session before a large transfer.

This follows the same structure as my earlier packages finlex, swelex and
danlex, which are already on CRAN.
