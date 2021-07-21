
<!-- README.md is generated from README.Rmd. Please edit that file -->

# facetious

<!-- badges: start -->

![](https://img.shields.io/badge/cool-useless-green.svg)
<!-- badges: end -->

`facetious` is home to some alternate facetting options for `ggplot2`.

## What’s in the box

-   `facet_wrap_strict()`
    -   respects nrow, ncol and inserts empty grobs to keep the
        resultant plot at the given size
    -   Otherwise works like `ggplot2::facet_wrap()`
-   `facet_grid_blank()`
    -   When a factor level doesn’t exist, this facetting approach will
        insert a totally empty object, instead of just an empty plot
    -   Otherwise works like `ggplot2::facet_grid()`

## Installation

You can install from
[GitHub](https://github.com/coolbutuseless/facetious) with:

``` r
# install.package('remotes')
remotes::install_github('coolbutuseless/facetious')
```

## `facet_wrap_strict`

In `ggplot2::facet_wrap()` (regardless of the `nrow` and `ncol`
specified by the user), rows and columns of empty facets are removed.

This behaviour of `ggplot2` can make it difficult to size and align
plots in multiple plots.

In contrast, `facetious::facet_wrap_strict()` will strictly adhere to
the `nrow`, `ncol` specified by the user, and retain blank rows/cols of
facets.

``` r
library(ggplot2)
library(patchwork)
library(facetious)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Standard ggplot2 facet_wrap()
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ggplot_facet_wrap <- ggplot(mtcars) +
  geom_point(aes(mpg, wt)) + 
  facet_wrap(~cyl, nrow = 3, ncol = 2) + 
  labs(title = "facet_wrap(... nrow=3, ncol=2)")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# facetious::facet_wrap_strict
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
facetious_facet_wrap_strict <- ggplot(mtcars) +
  geom_point(aes(mpg, wt)) + 
  facet_wrap_strict(~cyl, nrow = 3, ncol = 2) + 
  labs(title = "facet_wrap_strict(... nrow=3, ncol=2)")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Use 'patchwork' to stitch plots side-by-side
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ggplot_facet_wrap + facetious_facet_wrap_strict
```

<img src="man/figures/README-unnamed-chunk-2-1.png" width="100%" />

## `facet_grid_blank`

By default when a particular factor level is empty, `ggplot2` includes
an empty plot.

`facetious::facet_grid_blank()` goes a step further and (by default)
makes the empty facet entirely *blank*.

That is, empty factor levels are represented as a `grid::nullGrob()`,
but it is possible to specify another grob, or list of grobs to fill the
empty facets.

v0.1.1 of `facetious` borrows an idea from the really great hack
provided in [ggbillboard](https://github.com/nacnudus/ggbillboard) by
[Duncan Garmonsway](https://twitter.com/nacnudus) - i.e.  don’t just put
empty grobs into empty facets, put some advertising in there!

``` r
library(dplyr)
library(ggplot2)
library(patchwork)
library(facetious)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Make some data with some empty factors
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
plot_df <- mtcars %>%
  mutate(
    cyl = as.factor(cyl),
    am = as.factor(am)
  ) %>%
  filter(!(cyl == 4 & am == 1)) %>%
  filter(!(cyl == 8 & am == 0))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Standard ggplot facet_grid()
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ggplot_facet_grid <- ggplot(plot_df) +
  geom_point(aes(mpg, wt)) +
  facet_grid(rows = vars(cyl), cols = vars(am), drop = FALSE,
             labeller = label_both) +
  labs(title = "ggplot2::facet_grid()")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# facetious::facet_grid_blank()
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
facetious_facet_grid_blank <- ggplot(plot_df) +
  geom_point(aes(mpg, wt)) +
  facet_grid_blank(rows = vars(cyl), cols = vars(am), drop = FALSE, 
                   labeller = label_both) +
  labs(title = "facetious::facet_grid_blank()")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Use 'patchwork' to stitch plots side-by-side
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ggplot_facet_grid + facetious_facet_grid_blank
```

<img src="man/figures/README-unnamed-chunk-3-1.png" width="100%" />

# Using alternate globs for blank facets. Example 1 - single grob

``` r
grob_for_blank <- grid::textGrob("No data available")

 ggplot(plot_df) +
  geom_point(aes(mpg, wt)) +
  facet_grid_blank(rows = vars(cyl), cols = vars(am), drop = FALSE, 
                   labeller = label_both, blank = grob_for_blank) +
  labs(title = "facetious::facet_grid_blank()")
```

<img src="man/figures/README-unnamed-chunk-4-1.png" width="100%" />

# Using alternate globs for blank facets. Example 2 - multiple grobs

``` r
grobs_for_blank <- list(
  grid::rasterGrob(jpeg::readJPEG("man/figures/distracted.jpg")),
  grid::rasterGrob(jpeg::readJPEG("man/figures/run.jpg"))
)

 ggplot(plot_df) +
  geom_point(aes(mpg, wt)) +
  facet_grid_blank(rows = vars(cyl), cols = vars(am), drop = FALSE, 
                   labeller = label_both, blank = grobs_for_blank) +
  labs(title = "facetious::facet_grid_blank()")
```

<img src="man/figures/README-unnamed-chunk-5-1.png" width="100%" />

## Related Software

-   [ggplot2](https://cran.r-project.org/package=ggplot2)
-   [ggforce](https://cran.r-project.org/package=ggforce) a great
    package with alternate facets and tools for ggplot2
-   [ggbillboard](https://github.com/nacnudus/ggbillboard) a package for
    replacing blank facets with other grobs. This was so cool that I’ve
    incorporated the idea into `facetious` as part of the facet process,
    rather than as a post-processing step.

## Acknowledgements

-   R Core for developing and maintaining the language.
-   CRAN maintainers, for patiently shepherding packages onto CRAN and
    maintaining the repository
