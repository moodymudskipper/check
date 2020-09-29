
<!-- README.md is generated from README.Rmd. Please edit that file -->

# check

*{check}* is yet another package to deal with assertions. It’s only a
proof of concept.

*{assertive}*, *{assertthat}*, *{ensurer}*, *{assertr}*, *{checkmate}* …
are more mature packages that will do the the same job.

## Installation

``` r
remotes::install_gitub("moodyudskipper/check")
```

## How it works

Your functions will look like this:

``` r
hello <- function(x) {
  check(
    "`x` must be a character vector of length 1."
    # we can add more
    )
  paste("hello", x)
}

hello("world")
#> [1] "hello world"

hello(c("it's", "me"))
#> Error: `x` must be a character vector of length 1.
```

  - The error messages are the code, so it’s very readable.
  - Assertion patterns are defined for the package and are reused in
    different functions, so it is both enforcing consistency and leaving
    complete flexibility to the users.

To do so :

  - Paste `check::setup()` in an a R file of your package. It will
    create local copies of `check::check`, `check::set_check_fun`, and a
    `check_funs` environment

  - Right below define assertions using `set_check_fun(...)`

For instance have a “check.R” file containing :

``` r
check::setup() # unnecessary if you want to test it outside of a package

# don't namespace this one! (no `::`)
set_check_fun(
  "`{var}` must be a {type} vector of length {length}." = {
      val <- get(var, env)
      is.atomic(val) && is(val, type) && length(val) == length
  }
  # we can add more
  )
```

If you forget what patterns to use, then type `check_funs$` and
auto-complete will show you what you’ve defined. If you misspell a
pattern, `check` will suggest the closest fit to you.

## Caveats

  - A bit more setup than packages mentioned above. We could provide a
    `check_funs` environment with some patterns usable out of the box,
    for most common checks.
  - Doesn’t currently tell you more about the problematic arguments
    (e.g. failing example above doesn’t tell you that it fails because
    `x` is of length 2)
