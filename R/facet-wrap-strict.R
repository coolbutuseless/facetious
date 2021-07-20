

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Whatever
#'
#' @format NULL
#' @usage NULL
#'
#' @import ggplot2
#' @import gtable
#' @import grid
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
FacetWrapStrict <- ggplot2::ggproto(
  "FacetWrapStrict",
  ggplot2::FacetWrap,
  shrink = TRUE,

  draw_panels = function(panels, layout, x_scales, y_scales, ranges, coord, data, theme, params) {
    if ((params$free$x || params$free$y) && !coord$is_free()) {
      abort(glue("{snake_class(coord)} doesn't support free scales"))
    }

    if (inherits(coord, "CoordFlip")) {
      if (params$free$x) {
        layout$SCALE_X <- seq_len(nrow(layout))
      } else {
        layout$SCALE_X <- 1L
      }
      if (params$free$y) {
        layout$SCALE_Y <- seq_len(nrow(layout))
      } else {
        layout$SCALE_Y <- 1L
      }
    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    exact_facet_count <- TRUE

    if (exact_facet_count) {
      ncol2 <- max(layout$COL)
      nrow2 <- max(layout$ROW)
      ncol <- params$ncol
      nrow <- params$nrow
    } else {
      ncol <- max(layout$COL)
      nrow <- max(layout$ROW)
    }
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



    n <- nrow(layout)

    panel_order <- order(layout$ROW, layout$COL)
    layout <- layout[panel_order, ]
    panels <- panels[panel_order]
    panel_pos <- convertInd(layout$ROW, layout$COL, nrow)

    axes <- render_axes(ranges, ranges, coord, theme, transpose = TRUE)

    if (length(params$facets) == 0) {
      # Add a dummy label
      labels_df <- new_data_frame(list("(all)" = "(all)"), n = 1)
    } else {
      labels_df <- layout[names(params$facets)]
    }
    attr(labels_df, "facet") <- "wrap"
    strips <- render_strips(
      structure(labels_df, type = "rows"),
      structure(labels_df, type = "cols"),
      params$labeller, theme)

    # If user hasn't set aspect ratio, and we have fixed scales, then
    # ask the coordinate system if it wants to specify one
    aspect_ratio <- theme$aspect.ratio
    if (is.null(aspect_ratio) && !params$free$x && !params$free$y) {
      aspect_ratio <- coord$aspect(ranges[[1]])
    }

    if (is.null(aspect_ratio)) {
      aspect_ratio <- 1
      respect <- FALSE
    } else {
      respect <- TRUE
    }

    empty_table <- matrix(list(zeroGrob()), nrow = nrow, ncol = ncol)
    panel_table <- empty_table
    panel_table[panel_pos] <- panels
    empties <- apply(panel_table, c(1,2), function(x) is.zero(x[[1]]))
    panel_table <- gtable_matrix("layout", panel_table,
                                 widths = unit(rep(1, ncol), "null"),
                                 heights = unit(rep(abs(aspect_ratio), nrow), "null"), respect = respect, clip = coord$clip, z = matrix(1, ncol = ncol, nrow = nrow))
    panel_table$layout$name <- paste0('panel-', rep(seq_len(ncol), nrow), '-', rep(seq_len(nrow), each = ncol))

    panel_table <- gtable_add_col_space(panel_table,
                                        theme$panel.spacing.x %||% theme$panel.spacing)
    panel_table <- gtable_add_row_space(panel_table,
                                        theme$panel.spacing.y %||% theme$panel.spacing)

    # Add axes
    axis_mat_x_top <- empty_table
    axis_mat_x_top[panel_pos] <- axes$x$top[layout$SCALE_X]
    axis_mat_x_bottom <- empty_table
    axis_mat_x_bottom[panel_pos] <- axes$x$bottom[layout$SCALE_X]
    axis_mat_y_left <- empty_table
    axis_mat_y_left[panel_pos] <- axes$y$left[layout$SCALE_Y]
    axis_mat_y_right <- empty_table
    axis_mat_y_right[panel_pos] <- axes$y$right[layout$SCALE_Y]
    if (!params$free$x) {
      axis_mat_x_top[-1,]<- list(zeroGrob())
      axis_mat_x_bottom[-nrow,]<- list(zeroGrob())
    }
    if (!params$free$y) {
      axis_mat_y_left[, -1] <- list(zeroGrob())
      axis_mat_y_right[, -ncol] <- list(zeroGrob())
    }
    axis_height_top <- unit(
      apply(axis_mat_x_top, 1, max_height, value_only = TRUE),
      "cm"
    )
    axis_height_bottom <- unit(
      apply(axis_mat_x_bottom, 1, max_height, value_only = TRUE),
      "cm"
    )
    axis_width_left <- unit(
      apply(axis_mat_y_left, 2, max_width, value_only = TRUE),
      "cm"
    )
    axis_width_right <- unit(
      apply(axis_mat_y_right, 2, max_width, value_only = TRUE),
      "cm"
    )
    # Add back missing axes

    if (any(empties)) {

      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Which panels are last in their column?  They get a bottom axis.
      # Which panels are last in their row? They (may) get a right axis
      #
      # Use new varialbe 'idx' as the row index into the 'layout' data.frame
      # as I'm not certain that layout$PANEL is always strictly sequentially
      # increasing from 1.
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      lay     <- layout[,c('ROW', 'COL')]
      lay$idx <- seq(nrow(lay))

      row_panels <- aggregate(lay, by=list(m = lay$COL), max)
      row_panels <- row_panels$idx
      row_pos    <- convertInd(layout$ROW[row_panels], layout$COL[row_panels], nrow)
      row_axes   <- axes$x$bottom[layout$SCALE_X[row_panels]]

      col_panels <- aggregate(lay, by=list(m = lay$ROW), max)
      col_panels <- col_panels$idx
      col_pos    <- convertInd(layout$ROW[col_panels], layout$COL[col_panels], nrow)
      col_axes   <- axes$y$right[layout$SCALE_Y[col_panels]]
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # End change
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



      inside     <- (theme$strip.placement %||% "inside") == "inside"
      if (params$strip.position == "bottom" &&
          !inside &&
          any(!vapply(row_axes, is.zero, logical(1))) &&
          !params$free$x) {
        warn("Suppressing axis rendering when strip.position = 'bottom' and strip.placement == 'outside'")
      } else {
        axis_mat_x_bottom[row_pos] <- row_axes
      }
      if (params$strip.position == "right" &&
          !inside &&
          any(!vapply(col_axes, is.zero, logical(1))) &&
          !params$free$y) {
        warn("Suppressing axis rendering when strip.position = 'right' and strip.placement == 'outside'")
      } else {
        axis_mat_y_right[col_pos] <- col_axes
      }
    }

    panel_table <- weave_tables_row(panel_table, axis_mat_x_top   , -1, axis_height_top   , "axis-t", 3)
    panel_table <- weave_tables_row(panel_table, axis_mat_x_bottom,  0, axis_height_bottom, "axis-b", 3)
    panel_table <- weave_tables_col(panel_table, axis_mat_y_left  , -1, axis_width_left   , "axis-l", 3)
    panel_table <- weave_tables_col(panel_table, axis_mat_y_right ,  0, axis_width_right  , "axis-r", 3)

    strip_padding <- convertUnit(theme$strip.switch.pad.wrap, "cm")
    strip_name <- paste0("strip-", substr(params$strip.position, 1, 1))
    strip_mat <- empty_table
    strip_mat[panel_pos] <- unlist(unname(strips), recursive = FALSE)[[params$strip.position]]
    if (params$strip.position %in% c("top", "bottom")) {
      inside_x <- (theme$strip.placement.x %||% theme$strip.placement %||% "inside") == "inside"
      if (params$strip.position == "top") {
        placement <- if (inside_x) -1 else -2
        strip_pad <- axis_height_top
      } else {
        placement <- if (inside_x) 0 else 1
        strip_pad <- axis_height_bottom
      }
      strip_height <- unit(apply(strip_mat, 1, max_height, value_only = TRUE), "cm")
      panel_table <- weave_tables_row(panel_table, strip_mat, placement, strip_height, strip_name, 2, coord$clip)
      if (!inside_x) {
        strip_pad[as.numeric(strip_pad) != 0] <- strip_padding
        panel_table <- weave_tables_row(panel_table, row_shift = placement, row_height = strip_pad)
      }
    } else {
      inside_y <- (theme$strip.placement.y %||% theme$strip.placement %||% "inside") == "inside"
      if (params$strip.position == "left") {
        placement <- if (inside_y) -1 else -2
        strip_pad <- axis_width_left
      } else {
        placement <- if (inside_y) 0 else 1
        strip_pad <- axis_width_right
      }
      strip_pad[as.numeric(strip_pad) != 0] <- strip_padding
      strip_width <- unit(apply(strip_mat, 2, max_width, value_only = TRUE), "cm")
      panel_table <- weave_tables_col(panel_table, strip_mat, placement, strip_width, strip_name, 2, coord$clip)
      if (!inside_y) {
        strip_pad[as.numeric(strip_pad) != 0] <- strip_padding
        panel_table <- weave_tables_col(panel_table, col_shift = placement, col_width = strip_pad)
      }
    }
    panel_table
  },
  vars = function(self) {
    names(self$params$facets)
  }
)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Facet wrap strict
#'
#' @param facets,nrow,ncol,scales,shrink,labeller,as.table,switch,drop,dir,strip.position
#'        see ggplot2 docs
#'
#' @return FacetWrap oject
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
facet_wrap_strict <- function(facets, nrow = NULL, ncol = NULL, scales = "fixed",
                              shrink = TRUE, labeller = "label_value", as.table = TRUE,
                              switch = NULL, drop = TRUE, dir = "h", strip.position = 'top') {

  facet <- facet_wrap(
    facets,
    nrow           = nrow,
    ncol           = ncol,
    scales         = scales,
    shrink         = shrink,
    labeller       = labeller,
    as.table       = as.table,
    switch         = switch,
    drop           = drop,
    dir            = dir,
    strip.position = strip.position
  )

  if (is.null(nrow) || is.null(ncol)) {
    facet
  } else {
    ggproto(
      NULL,
      FacetWrapStrict,
      shrink = shrink,
      params = facet$params
    )
  }

}









