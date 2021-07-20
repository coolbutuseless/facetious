


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# What I'm about to do here is highly illegal in the #RStats worlds
#
# I am going to pull in a bunch of non-exported functions from ggplot2,
# and make them available internally in this package.
#
# By doing this, I can basically copy 'FacetWrap' and 'FacetGrid' from
# ggplot2 and adapt to my needs without having to replicate all the supporting
# functions that are internal to ggplot2
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
pkg       <- 'ggplot2'
# pkg_names <- unclass(ls(envir = asNamespace(pkg), all = TRUE))
# pkg_names <- pkg_names[grepl("^[\\w]*$", pkg_names, perl = TRUE)]

pkg_names <- c(
  'weave_tables_row',
  'weave_tables_col',
  'convertInd',
  'is.zero'
)

for(name in pkg_names) {
  assign(name, get(name,asNamespace(pkg)))
}


`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}
