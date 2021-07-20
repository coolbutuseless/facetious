
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Facet Grid with blanks where no information avail
#'
#' @param rows,cols,scales,space,shrink,labeller,as.table,switch,margins,facets
#'        see documentation for \code{ggplot2::facet_grid}
#' @param drop Default: FALSE, all factor levels will be shown, regardless of
#'        whether or not they appear in the data.  This differs from
#'        \code{ggplot2::facet_grid()} which defaults to TRUE
#' @param blank Default TRUE.  Facets which contain no data to be plotted are
#'        shown as completely blank spaces rather than a plot with a panel
#'        but no geometry.  This argument only has an effect if \code{drop=FALSE}
#'
#' @import ggplot2
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
facet_grid_blank <- function(rows = NULL, cols = NULL, scales = "fixed",
                             space = "fixed", shrink = TRUE,
                             labeller = "label_value", as.table = TRUE,
                             switch = NULL, drop = FALSE, margins = FALSE,
                             facets = NULL, blank = TRUE) {
  facet <- ggplot2::facet_grid(
    rows     = rows,
    cols     = cols,
    scales   = scales,
    space    = space,
    shrink   = shrink,
    labeller = labeller,
    as.table = as.table,
    switch   = switch,
    drop     = drop,
    margins  = margins,
    facets   = facets
  )

  ggplot2::ggproto(
    NULL,
    FacetGridBlank,
    shrink = shrink,
    params = c(
      facet$params,
      list(blank = isTRUE(blank))
    )
  )
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' FacetGrid
#' @format NULL
#' @usage NULL
#'
#' @import ggplot2
#' @import gtable
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
FacetGridBlank <- ggplot2::ggproto(
  "FacetGridBlank",
  ggplot2::FacetGrid,
  shrink = TRUE,

  draw_panels = function(panels, layout, x_scales, y_scales, ranges, coord, data, theme, params) {
    if ((params$free$x || params$free$y) && !coord$is_free()) {
      abort(glue("{snake_class(coord)} doesn't support free scales"))
    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Work out which ROW,COL (and hence which panel) are blank
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (isTRUE(params$blank)) {
      empty_idx    <- which(table(data[[1]]$PANEL) == 0) # when is PANEL count zero in the data?
      n_row        <- max(layout$ROW)
      n_col        <- max(layout$COL)
      tmp_df       <- layout[layout$PANEL %in% empty_idx, c('ROW', 'COL')]
      empty_panels <- with(tmp_df, (COL - 1) * n_row + ROW)
    }

    cols <- which(layout$ROW == 1)
    rows <- which(layout$COL == 1)
    axes <- render_axes(ranges[cols], ranges[rows], coord, theme, transpose = TRUE)

    col_vars <- unique(layout[names(params$cols)])
    row_vars <- unique(layout[names(params$rows)])
    # Adding labels metadata, useful for labellers
    attr(col_vars, "type") <- "cols"
    attr(col_vars, "facet") <- "grid"
    attr(row_vars, "type") <- "rows"
    attr(row_vars, "facet") <- "grid"
    strips <- render_strips(col_vars, row_vars, params$labeller, theme)

    aspect_ratio <- theme$aspect.ratio
    if (!is.null(aspect_ratio) && (params$space_free$x || params$space_free$y)) {
      abort("Free scales cannot be mixed with a fixed aspect ratio")
    }
    if (is.null(aspect_ratio) && !params$free$x && !params$free$y) {
      aspect_ratio <- coord$aspect(ranges[[1]])
    }
    if (is.null(aspect_ratio)) {
      aspect_ratio <- 1
      respect <- FALSE
    } else {
      respect <- TRUE
    }
    ncol <- max(layout$COL)
    nrow <- max(layout$ROW)
    panel_table <- matrix(panels, nrow = nrow, ncol = ncol, byrow = TRUE)

    # @kohske
    # Now size of each panel is calculated using PANEL$ranges, which is given by
    # coord_train called by train_range.
    # So here, "scale" need not to be referred.
    #
    # In general, panel has all information for building facet.
    if (params$space_free$x) {
      ps <- layout$PANEL[layout$ROW == 1]
      widths <- vapply(ps, function(i) diff(ranges[[i]]$x.range), numeric(1))
      panel_widths <- unit(widths, "null")
    } else {
      panel_widths <- rep(unit(1, "null"), ncol)
    }
    if (params$space_free$y) {
      ps <- layout$PANEL[layout$COL == 1]
      heights <- vapply(ps, function(i) diff(ranges[[i]]$y.range), numeric(1))
      panel_heights <- unit(heights, "null")
    } else {
      panel_heights <- rep(unit(1 * abs(aspect_ratio), "null"), nrow)
    }

    panel_table <- gtable_matrix("layout", panel_table,
                                 panel_widths, panel_heights, respect = respect, clip = coord$clip, z = matrix(1, ncol = ncol, nrow = nrow))
    panel_table$layout$name <- paste0('panel-', rep(seq_len(nrow), ncol), '-', rep(seq_len(ncol), each = nrow))

    panel_table <- gtable_add_col_space(panel_table,
                                        theme$panel.spacing.x %||% theme$panel.spacing)
    panel_table <- gtable_add_row_space(panel_table,
                                        theme$panel.spacing.y %||% theme$panel.spacing)

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Zero out the empty panels
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (isTRUE(params$blank)) {
      for(idx in empty_panels) {
        panel_table$grobs[[idx]] <- zeroGrob()
      }
    }


    # Add axes
    panel_table <- gtable_add_rows(panel_table, max_height(axes$x$top), 0)
    panel_table <- gtable_add_rows(panel_table, max_height(axes$x$bottom), -1)
    panel_table <- gtable_add_cols(panel_table, max_width(axes$y$left), 0)
    panel_table <- gtable_add_cols(panel_table, max_width(axes$y$right), -1)
    panel_pos_col <- panel_cols(panel_table)
    panel_pos_rows <- panel_rows(panel_table)

    panel_table <- gtable_add_grob(panel_table, axes$x$top, 1, panel_pos_col$l, clip = "off", name = paste0("axis-t-", seq_along(axes$x$top)), z = 3)
    panel_table <- gtable_add_grob(panel_table, axes$x$bottom, -1, panel_pos_col$l, clip = "off", name = paste0("axis-b-", seq_along(axes$x$bottom)), z = 3)
    panel_table <- gtable_add_grob(panel_table, axes$y$left, panel_pos_rows$t, 1, clip = "off", name = paste0("axis-l-", seq_along(axes$y$left)), z = 3)
    panel_table <- gtable_add_grob(panel_table, axes$y$right, panel_pos_rows$t, -1, clip = "off", name = paste0("axis-r-", seq_along(axes$y$right)), z= 3)

    # Add strips
    switch_x <- !is.null(params$switch) && params$switch %in% c("both", "x")
    switch_y <- !is.null(params$switch) && params$switch %in% c("both", "y")
    inside_x <- (theme$strip.placement.x %||% theme$strip.placement %||% "inside") == "inside"
    inside_y <- (theme$strip.placement.y %||% theme$strip.placement %||% "inside") == "inside"
    strip_padding <- convertUnit(theme$strip.switch.pad.grid, "cm")
    panel_pos_col <- panel_cols(panel_table)
    if (switch_x) {
      if (!is.null(strips$x$bottom)) {
        if (inside_x) {
          panel_table <- gtable_add_rows(panel_table, max_height(strips$x$bottom), -2)
          panel_table <- gtable_add_grob(panel_table, strips$x$bottom, -2, panel_pos_col$l, clip = "on", name = paste0("strip-b-", seq_along(strips$x$bottom)), z = 2)
        } else {
          panel_table <- gtable_add_rows(panel_table, strip_padding, -1)
          panel_table <- gtable_add_rows(panel_table, max_height(strips$x$bottom), -1)
          panel_table <- gtable_add_grob(panel_table, strips$x$bottom, -1, panel_pos_col$l, clip = "on", name = paste0("strip-b-", seq_along(strips$x$bottom)), z = 2)
        }
      }
    } else {
      if (!is.null(strips$x$top)) {
        if (inside_x) {
          panel_table <- gtable_add_rows(panel_table, max_height(strips$x$top), 1)
          panel_table <- gtable_add_grob(panel_table, strips$x$top, 2, panel_pos_col$l, clip = "on", name = paste0("strip-t-", seq_along(strips$x$top)), z = 2)
        } else {
          panel_table <- gtable_add_rows(panel_table, strip_padding, 0)
          panel_table <- gtable_add_rows(panel_table, max_height(strips$x$top), 0)
          panel_table <- gtable_add_grob(panel_table, strips$x$top, 1, panel_pos_col$l, clip = "on", name = paste0("strip-t-", seq_along(strips$x$top)), z = 2)
        }
      }
    }
    panel_pos_rows <- panel_rows(panel_table)
    if (switch_y) {
      if (!is.null(strips$y$left)) {
        if (inside_y) {
          panel_table <- gtable_add_cols(panel_table, max_width(strips$y$left), 1)
          panel_table <- gtable_add_grob(panel_table, strips$y$left, panel_pos_rows$t, 2, clip = "on", name = paste0("strip-l-", seq_along(strips$y$left)), z = 2)
        } else {
          panel_table <- gtable_add_cols(panel_table, strip_padding, 0)
          panel_table <- gtable_add_cols(panel_table, max_width(strips$y$left), 0)
          panel_table <- gtable_add_grob(panel_table, strips$y$left, panel_pos_rows$t, 1, clip = "on", name = paste0("strip-l-", seq_along(strips$y$left)), z = 2)
        }
      }
    } else {
      if (!is.null(strips$y$right)) {
        if (inside_y) {
          panel_table <- gtable_add_cols(panel_table, max_width(strips$y$right), -2)
          panel_table <- gtable_add_grob(panel_table, strips$y$right, panel_pos_rows$t, -2, clip = "on", name = paste0("strip-r-", seq_along(strips$y$right)), z = 2)
        } else {
          panel_table <- gtable_add_cols(panel_table, strip_padding, -1)
          panel_table <- gtable_add_cols(panel_table, max_width(strips$y$right), -1)
          panel_table <- gtable_add_grob(panel_table, strips$y$right, panel_pos_rows$t, -1, clip = "on", name = paste0("strip-r-", seq_along(strips$y$right)), z = 2)
        }
      }
    }
    panel_table
  }
)


