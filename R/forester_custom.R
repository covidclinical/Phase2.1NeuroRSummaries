

forester <- function (left_side_data, estimate, ci_low, ci_high, right_side_data = NULL,
          estimate_precision = 1, ggplot_width = 30, null_line_at = 0,
          file_path = here::here("forester_plot.png"), dpi = 600,
          display = TRUE, blank_na = TRUE, font_family = "mono", estimate_col_name = "Estimate",
          stripe_colour = "#eff3f2", x_scale_linear = TRUE, xlim = NULL,
          xbreaks = NULL, nudge_y = 0, nudge_x = 1, arrows = FALSE,
          arrow_labels = c("Lower", "Higher"), add_plot = NULL, add_plot_width = 1,
          add_plot_gap = FALSE, point_sizes = 3, point_shapes = 16,
          center_ggplot = NULL)
{
  theme <- gridExtra::ttheme_minimal(core = list(fg_params = list(hjust = 0,
                                                                  x = 0.05, fontfamily = font_family), bg_params = list(fill = c(rep(c(stripe_colour,
                                                                                                                                       "white"), length.out = nrow(left_side_data)), "white",
                                                                                                                                 "white", "white"))), colhead = list(fg_params = list(hjust = 0,
                                                                                                                                                                                      x = 0.05, fontfamily = font_family), bg_params = list(fill = "white")))
  gdata <- data.frame(estimate = estimate, ci_low = ci_low,
                      ci_high = ci_high)
  if (is.null(right_side_data)) {
    tdata <- gdata
    tdata <- dplyr::mutate_all(tdata, ~sprintf(., fmt = paste0("%#.",
                                                               estimate_precision, "f")))
    tdata[tdata == "NA"] <- " "
    right_side_data <- data.frame(Estimate = ifelse(tdata$estimate ==
                                                      " ", " ", paste0(tdata$estimate, " (", tdata$ci_low,
                                                                       " to ", tdata$ci_high, ")")))
    colnames(right_side_data) <- estimate_col_name
  }
  find_width_mono <- function(data) {
    num_of_rows <- nrow(data)
    num_of_cols <- ncol(data)
    print_data <- dplyr::mutate_all(data, as.character)
    num_char_across <- 0
    width <- 0
    for (i in 1:num_of_cols) {
      for (j in 1:num_of_rows) {
        num_char_across[j] <- nchar(print_data[j, i])
      }
      width[i] <- max(max(num_char_across, na.rm = TRUE),
                      nchar(colnames(print_data)[i]), na.rm = TRUE)
    }
    return(sum(width, na.rm = TRUE))
  }
  find_width <- function(data) {
    num_of_rows <- nrow(data)
    num_of_cols <- ncol(data)
    print_data <- dplyr::mutate_all(data, as.character)
    width <- 0
    names <- colnames(print_data)
    for (i in 1:num_of_cols) {
      temp <- systemfonts::shape_string(print_data[[names[i]]],
                                        family = font_family)
      temp_col <- systemfonts::shape_string(names[i],
                                            family = font_family)
      width[i] <- max(max(temp$metrics$width, na.rm = TRUE),
                      temp_col$metrics$width, na.rm = TRUE)
    }
    return(sum(width, na.rm = TRUE)/7.2)
  }
  if (font_family == "mono") {
    left_width <- find_width_mono(left_side_data)
    right_width <- find_width_mono(right_side_data)
  }
  else {
    left_width <- find_width(left_side_data)
    right_width <- find_width(right_side_data)
  }
  total_width <- left_width + right_width + ggplot_width
  tdata_print <- left_side_data
  tdata_print$` ` <- paste(rep(" ", times = round(ggplot_width,
                                                  0)), collapse = "")
  tdata_print <- cbind(tdata_print, right_side_data)
  tdata_print <- tibble::add_row(tdata_print)
  tdata_print <- tibble::add_row(tdata_print)
  tdata_print <- tibble::add_row(tdata_print)
  if (blank_na == TRUE) {
    tdata_print <- dplyr::mutate_all(tdata_print, as.character)
    tdata_print[is.na(tdata_print)] <- " "
  }
  mono_column <- function(table, col) {
    col_indexes <- function(table, col, name = "core-fg") {
      l <- table$layout
      which(l$l == col & l$name == name)
    }
    ind <- col_indexes(table, col, "core-fg")
    for (i in ind) {
      table$grobs[i][[1]][["gp"]] <- grid::gpar(fontfamily = "mono")
    }
    return(table)
  }
  white_column <- function(table, col) {
    col_indexes <- function(table, col, name = "core-bg") {
      l <- table$layout
      which(l$l == col & l$name == name)
    }
    ind <- col_indexes(table, col, "core-bg")
    ind_fg <- col_indexes(table, col, "core-fg")
    for (i in ind) {
      table$grobs[i][[1]][["gp"]] <- grid::gpar(fill = "white",
                                                col = "white")
    }
    for (i in ind_fg) {
      table$grobs[i][[1]][["gp"]] <- grid::gpar(fontfamily = "mono")
    }
    return(table)
  }
  gdata$row_num <- (nrow(gdata) - 1):0
  h_adj <- dplyr::case_when(font_family == "mono" ~ 0.2, font_family ==
                              "serif" ~ 0.43, font_family == "sans" ~ 0.37, TRUE ~
                              0)
  h_adj <- nudge_y + h_adj
  slope_adj <- dplyr::case_when(font_family == "mono" ~ -0.175,
                                font_family == "serif" ~ -0.19, font_family == "sans" ~
                                  -0.16, TRUE ~ 0)
  font_adj <- 0.3 + h_adj + log(nrow(gdata)) * slope_adj
  y_low <- -0.5 + font_adj + -0.1381 * log(nrow(gdata))
  y_high <- 1.017 * nrow(gdata) - 0.6
  gdata$shape <- point_shapes
  gdata$sizes <- point_sizes
  g_oob <- tibble::tibble()
  if (!is.null(xlim)) {
    oob_arrows <- gdata
    oob_arrows$x_low <- xlim[1]
    oob_arrows$x_high <- xlim[2]
    ra <- sum(oob_arrows$ci_high > oob_arrows$x_high, na.rm = T) >
      0
    la <- sum(oob_arrows$ci_low < oob_arrows$x_low, na.rm = T) >
      0
    if (ra) {
      right_arrows <- dplyr::select(dplyr::filter(oob_arrows,
                                                  ci_high > x_high), start = estimate, end = x_high,
                                    y = row_num)
    }
    if (la) {
      left_arrows <- dplyr::select(dplyr::filter(oob_arrows,
                                                 ci_low < x_low), start = estimate, end = x_low,
                                   y = row_num)
    }
    if (ra && !la) {
      g_oob <- right_arrows
    }
    else if (!ra && la) {
      g_oob <- left_arrows
    }
    else if (ra && la) {
      g_oob <- rbind.data.frame(right_arrows, left_arrows)
    }
  }
  center <- ggplot2::ggplot() + ggplot2::geom_point(data = gdata,
                                                    ggplot2::aes(y = row_num, x = estimate, size = sizes,
                                                                 shape = shape, colour = as.factor(shape)), na.rm = TRUE) + ggplot2::geom_errorbarh(data = gdata,
                                                                                                                         ggplot2::aes(y = row_num, xmin = ci_low, xmax = ci_high),
                                                                                                                         height = 0.25, na.rm = TRUE) + ggplot2::theme_classic() +
    scale_colour_manual(values=c("black", "red")) +
    ggplot2::theme(axis.title.y = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_blank(), axis.ticks.y = ggplot2::element_blank(),
                   axis.line.y = ggplot2::element_blank(), axis.ticks.length.x = grid::unit(0.07,
                                                                                            "in"), text = ggplot2::element_text(family = font_family,
                                                                                                                                size = 12), panel.background = ggplot2::element_rect(fill = "transparent"),
                   plot.background = ggplot2::element_rect(fill = "transparent",
                                                           color = NA), panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(), legend.background = ggplot2::element_rect(fill = "transparent"),
                   legend.position = "none") +
                   #legend.box.background = ggplot2::element_rect(fill = "transparent")) +
    ggplot2::geom_vline(xintercept = null_line_at, linetype = "dashed") +
    ggplot2::scale_y_continuous(expand = c(0, 0)) + ggplot2::scale_shape_identity() +
    ggplot2::scale_size_identity() + ggplot2::xlab("")
  if (nrow(g_oob) > 0) {
    center <- center + ggplot2::geom_segment(data = g_oob,
                                             ggplot2::aes(x = start, xend = end, y = y, yend = y),
                                             arrow = ggplot2::arrow(angle = 15, type = "closed",
                                                                    length = grid::unit(0.1, "in")))
  }
  if (is.null(xlim)) {
    center <- center + ggplot2::coord_cartesian(ylim = c(y_low,
                                                         y_high))
  }
  else {
    center <- center + ggplot2::coord_cartesian(ylim = c(y_low,
                                                         y_high), xlim = xlim)
  }
  if (x_scale_linear) {
    if (is.null(xbreaks)) {
      center <- center + ggplot2::scale_x_continuous(labels = scales::number_format(accuracy = 0.1),
                                                     expand = c(0, 0))
    }
    else {
      center <- center + ggplot2::scale_x_continuous(labels = scales::number_format(accuracy = 0.1),
                                                     breaks = xbreaks, expand = c(0, 0))
    }
  }
  else {
    if (is.null(xbreaks)) {
      center <- center + ggplot2::scale_x_log10(labels = scales::number_format(accuracy = 0.1),
                                                expand = c(0, 0))
    }
    else {
      center <- center + ggplot2::scale_x_log10(labels = scales::number_format(accuracy = 0.1),
                                                breaks = xbreaks, expand = c(0, 0))
    }
  }
  if (!is.null(center_ggplot)) {
    center <- center_ggplot
  }
  if (arrows == TRUE) {
    xlab_df <- data.frame(text = arrow_labels, x = xlim,
                          y = c(0, 0), hjust = c(0, 1))
    a_small_amount <- abs(xlim[1] - xlim[2])/35
    if (x_scale_linear == TRUE) {
      arrow_df <- data.frame(id = c(1, 2), xstart = c(null_line_at -
                                                        a_small_amount, null_line_at + a_small_amount),
                             xend = c(xlim[1] + a_small_amount, xlim[2] -
                                        a_small_amount), y = c(1, 1))
    }
    else {
      arrow_df <- data.frame(id = c(1, 2), xstart = c(null_line_at -
                                                        a_small_amount, null_line_at + a_small_amount),
                             xend = c(xlim[1], xlim[2]), y = c(1, 1))
    }
    arrows_plot <- ggplot2::ggplot() + ggplot2::geom_segment(data = arrow_df,
                                                             ggplot2::aes(x = .data$xstart, xend = .data$xend,
                                                                          y = .data$y, yend = .data$y), arrow = ggplot2::arrow(angle = 15,
                                                                                                                               type = "closed", length = grid::unit(0.1, "in"))) +
      ggplot2::geom_text(data = xlab_df, ggplot2::aes(x = .data$x,
                                                      y = .data$y, label = .data$text, hjust = .data$hjust),
                         family = font_family, size = 3) + ggplot2::scale_y_continuous(expand = c(0,
                                                                                                  0), limits = c(-0.5, 1.75)) + ggplot2::scale_x_continuous(expand = c(0,
                                                                                                                                                                       0), limits = xlim) + ggplot2::theme(panel.background = ggplot2::element_rect(fill = "transparent"),
                                                                                                                                                                                                           plot.background = ggplot2::element_rect(fill = "transparent",
                                                                                                                                                                                                                                                   color = NA), panel.grid.major = ggplot2::element_blank(),
                                                                                                                                                                                                           panel.grid.minor = ggplot2::element_blank(), legend.background = ggplot2::element_rect(fill = "transparent"),
                                                                                                                                                                                                           legend.position = "none",
                                                                                                                                                                                                           #legend.box.background = ggplot2::element_rect(fill = "transparent"),
                                                                                                                                                                                                           panel.border = ggplot2::element_blank(), axis.title.y = ggplot2::element_blank(),
                                                                                                                                                                                                           axis.text.y = ggplot2::element_blank(), axis.ticks.y = ggplot2::element_blank(),
                                                                                                                                                                                                           axis.line.y = ggplot2::element_blank(), axis.title.x = ggplot2::element_blank(),
                                                                                                                                                                                                           axis.text.x = ggplot2::element_blank(), axis.ticks.x = ggplot2::element_blank(),
                                                                                                                                                                                                           axis.line.x = ggplot2::element_blank())
    if (x_scale_linear == FALSE) {
      arrows_plot <- arrows_plot + ggplot2::scale_x_log10(expand = c(0,
                                                                     0), limits = xlim)
    }
  }
  png_width <- total_width/10 + nudge_x
  png_height <- (nrow(gdata) + 3)/3.8
  if (is.null(add_plot)) {
    table_final <- mono_column(gridExtra::tableGrob(tdata_print,
                                                    theme = theme, rows = NULL), ncol(left_side_data) +
                                 1)
    table_final$widths[ncol(left_side_data) + 1] <- grid::unit(ggplot_width/10,
                                                               "in")
    table_final$heights <- grid::unit(rep(0.255, times = length(table_final$heights)),
                                      "in")
    final <- patchwork::wrap_elements(table_final) + patchwork::inset_element(center,
                                                                              align_to = "full", left = (left_width/total_width),
                                                                              right = ((ggplot_width + left_width)/total_width),
                                                                              top = 1, bottom = 0.35/nrow(gdata))
    if (arrows == TRUE) {
      final <- final + patchwork::inset_element(arrows_plot,
                                                align_to = "full", left = (left_width/total_width),
                                                right = ((ggplot_width + left_width)/total_width),
                                                top = 1.5/nrow(gdata), bottom = 0)
    }
  }
  else {
    tdata_print$`  ` <- paste(rep(" ", times = round(ggplot_width,
                                                     0)), collapse = "")
    table_final <- mono_column(gridExtra::tableGrob(tdata_print,
                                                    theme = theme, rows = NULL), ncol(left_side_data) +
                                 1)
    table_final <- white_column(table_final, ncol(table_final))
    table_final$widths[ncol(left_side_data) + 1] <- grid::unit(ggplot_width/10,
                                                               "in")
    table_final$widths[ncol(table_final)] <- grid::unit(ggplot_width/10,
                                                        "in")
    table_final$heights <- grid::unit(rep(0.255, times = length(table_final$heights)),
                                      "in")
    new_full_width <- total_width + ggplot_width
    png_width <- new_full_width/10 + nudge_x
    if (add_plot_gap) {
      add_plot <- add_plot + ggplot2::scale_y_continuous(limits = c(y_low,
                                                                    y_high), expand = c(0, 0)) + ggplot2::theme_classic() +
        ggplot2::theme(axis.title.y = ggplot2::element_text(colour = "transparent"),
                       axis.text.y = ggplot2::element_text(colour = "transparent"),
                       axis.ticks.y = ggplot2::element_line(colour = "transparent"),
                       axis.line.y = ggplot2::element_line(colour = "transparent"),
                       axis.title.x = ggplot2::element_text(colour = "transparent"),
                       axis.text.x = ggplot2::element_text(colour = "transparent"),
                       axis.ticks.x = ggplot2::element_line(colour = "transparent"),
                       axis.line.x = ggplot2::element_line(colour = "transparent"),
                       panel.background = ggplot2::element_rect(fill = "transparent"),
                       plot.background = ggplot2::element_rect(fill = "transparent",
                                                               color = NA), panel.grid.major = ggplot2::element_blank(),
                       panel.grid.minor = ggplot2::element_blank(),
                       legend.background = ggplot2::element_rect(fill = "transparent"),
                       legend.box.background = ggplot2::element_rect(fill = "transparent"),
                       legend.position = "none")
    }
    else {
      add_plot <- add_plot + ggplot2::scale_y_continuous(limits = c(y_low,
                                                                    y_high), expand = c(0, 0)) + ggplot2::theme_classic() +
        ggplot2::theme(axis.title.x = ggplot2::element_text(colour = "transparent"),
                       axis.text.x = ggplot2::element_text(colour = "transparent"),
                       axis.ticks.x = ggplot2::element_line(colour = "transparent"),
                       axis.line.x = ggplot2::element_line(colour = "transparent"),
                       axis.title.y = ggplot2::element_blank(), axis.text.y = ggplot2::element_blank(),
                       axis.ticks.y = ggplot2::element_blank(), axis.line.y = ggplot2::element_blank(),
                       panel.background = ggplot2::element_rect(fill = "transparent"),
                       plot.background = ggplot2::element_rect(fill = "transparent",
                                                               color = NA), panel.grid.major = ggplot2::element_blank(),
                       panel.grid.minor = ggplot2::element_blank(),
                       legend.background = ggplot2::element_rect(fill = "transparent"),
                       legend.box.background = ggplot2::element_rect(fill = "transparent"),
                       legend.position = "none")
    }
    final <- patchwork::wrap_elements(table_final) + patchwork::inset_element(center,
                                                                              align_to = "full", left = (left_width/new_full_width),
                                                                              right = ((ggplot_width + left_width)/new_full_width),
                                                                              top = 1, bottom = 0.35/nrow(gdata)) + patchwork::inset_element(add_plot,
                                                                                                                                             align_to = "full", left = total_width/new_full_width,
                                                                                                                                             right = 1, top = 1, bottom = 0.35/nrow(gdata))
    if (arrows == TRUE) {
      final <- final + patchwork::inset_element(arrows_plot,
                                                align_to = "full", left = (left_width/new_full_width),
                                                right = ((ggplot_width + left_width)/new_full_width),
                                                top = 1.5/nrow(gdata), bottom = 0)
    }
  }
  ggplot2::ggsave(dpi = dpi, height = png_height, width = png_width,
                  units = "in", filename = file_path)
  if (display == TRUE) {
    magick::image_resize(magick::image_read(file_path),
                         paste0(grDevices::dev.size("px")[1], "x", grDevices::dev.size("px")[2]))
  }
}
