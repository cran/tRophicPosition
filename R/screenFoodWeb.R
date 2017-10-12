#' Function that creates a biplot of a food web with stable isotope values (d13C
#' and d15N)
#'
#' @param df a data frame that contains the isotope values. By defaults, the
#'   data frame needs to have the following columns: d13C, d15N, Species and
#'   FG. Species stands for the scientific name (or common name), and FG stands
#'   for the functional group for each species.
#' @param grouping a vector with the name of the columns (variables) that will
#'   be used to summarize, and plot the data frame.
#' @param title string representing the title of the plot.
#' @param printSummary a logical value to indicate whether the summary is
#'   printed
#' @param ... optional arguments that are passed to the function for later use.
#' @param order logical, if true it orders the legend.
#'
#' @return a ggplot2 object with the biplot of the data frame. Also prints the
#'   summary of the data frame as needed.
#'
#' @export
#'
#' @examples
#' data("Bilagay")
#' subset_CHI <- Bilagay[Bilagay[,"Location"] %in% "CHI",]
#' screenFoodWeb(subset_CHI, grouping = c("Spp", "FG"))

screenFoodWeb <- function (df = NULL,
                           grouping = c("Species", "FG"),
                           title = NULL,
                           # palette = NULL,
                           order = FALSE,
                           printSummary = FALSE, ...){
  #require(ggplot2)

  # Stupid CRAN fix for variables - see here http://stackoverflow.com/questions/
  # 9439256/how-can-i-handle-r-cmd-check-no-visible-binding-for-global-
  # variable-notes-when
  # As seen in
  # https://github.com/andrewcparnell/simmr/blob/master/R/plot.simmr_output.R
  mean_d13C = mean_d15N = SD_d15N = SD_d13C = NULL

  summary <- tRophicPosition::summariseIsotopeData(df,
                                                   grouping,
                                                   order,
                                                   # ordering = FALSE,
                                                   printSummary)
  # To do: add subtitles according to FG levels
  if (isTRUE(order)) {
    levels_FG <- list()
    for (level in seq_along(levels(summary[[grouping[2]]])))
      levels_FG[level] <- levels(summary[[grouping[2]]])[[level]]

    index <- as.integer(summary[[grouping[2]]])
    new_levels <- vector('character')
    j <- index[1]
    for (i in seq_along(summary[[grouping[1]]])){
      # message(paste("i:", i, "; j:", j))
      if (j == index[i]) {
        new_levels <- c(new_levels, unlist(levels_FG[j]))
        j <- j + 1
        }
      new_levels <- c(new_levels, as.character(summary[[grouping[1]]][i]))
    }
    summary[[grouping[1]]] <- factor(summary[[grouping[1]]], levels = new_levels)
    fill <- summary[[grouping[1]]]
  } else fill <- factor(summary[[grouping[1]]])

  shape <- factor(summary[[grouping[2]]])
  shapes <- 20+as.numeric(factor(summary[[grouping[2]]]))

  p <- ggplot2::ggplot(summary,
                       ggplot2::aes(mean_d13C, mean_d15N,
                                    fill = fill,
                                    shape = shape)) +
    ggplot2::scale_fill_discrete(name = grouping[1]) +
    # ggplot2::scale_shape_manual(name = grouping[2],
    #                             values = levels(shape)) +
    ggplot2::geom_errorbar(data = summary,
                           ggplot2::aes(ymin = mean_d15N - SD_d15N,
                                        ymax = mean_d15N + SD_d15N),
                           width = 0.15,
                           color = "black", size = .5) +
    ggplot2::geom_errorbarh(data = summary,
                            ggplot2::aes(xmin = mean_d13C - SD_d13C,
                                         xmax = mean_d13C + SD_d13C),
                            height = 0.15, color = "black", size = .5) +
    ggplot2::geom_point(size = 3, shape = shapes) +
    ggplot2::guides(shape = ggplot2::guide_legend(order = 2)) +
    ggplot2::ylab(expression(paste(delta^{15}, "N (\u2030)"))) +
    ggplot2::xlab(expression(paste(delta^{13}, "C (\u2030)"))) +
    ggplot2::labs(fill = grouping[1], shape = grouping[2]) +
    # scale_colour_brewer(RColorBrewer::brewer.pal(
    #   length(Strangford.summary$Species_name),"Set1"))+
    # title = "Strangford Lough (mean +- sd)") +
    # geom_point(data=Strangford.summary, size=3, shape = Strangford.summary$FG) +
    # scale_shape_discrete(solid = T) +
    # ggplot2::scale_shape()
    # ggplot2::scale_shape_identity(name = grouping[2]) +
    # ggplot2::scale_fill_discrete(name = grouping[1]) +
    # ggplot2::scale_colour_discrete(name = grouping[1])
    ggplot2::theme_bw()

  if (!is.null(title)) p <- p + ggplot2::labs(title = title)

  # if (!is.null(palette)) p <- p + ggplot2::scale_fill_brewer(
  #   RColorBrewer::brewer.pal(length(fill),palette))

  levels <- length(factor(summary[[grouping[2]]]))

  if (levels > 14 && levels < 26)
    p <- p + ggplot2::guides(fill = ggplot2::guide_legend(ncol = 2,
                                                          byrow = FALSE))
  if (levels >= 26)
    p <- p + ggplot2::guides(fill = ggplot2::guide_legend(ncol = 3,
                                                          byrow = FALSE))
  # if (!is.null(scale_colour_manual))
  #   p <- p + ggplot2::scale_colour_manual(values = scale_colour_manual)

  print(p)
  return(p)

}
