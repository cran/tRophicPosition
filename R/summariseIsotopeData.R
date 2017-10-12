#' Function that summarises a data frame containing stable isotope values (d13C
#' and d15N) grouping by Species and FG columns
#'
#' A wrapper of plyr:ddply to summarise a data frame.
#'
#' @param df a data frame that contains the isotope values. It needs to have the
#'   following columns: d13C, d15N, Species and FG. Species stands for the
#'   scientific name (or common name), and FG stands for the functional group
#'   for each species. If the data frame does not have Species and FG columns,
#'   it will raise an error. If the columns change their names, they need to be
#'   stated as well in the grouping variable.
#' @param grouping a vector with the name of the columns (variables) that will
#'   be used to summarize, and plot the data frame.
#' @param printSummary logical value indicating whether the summary is printed.
#' @param ... optional arguments that are passed to the function for later use.
#' @param order logical, if true it orders the levels of the grouping variable.
#'
#' @return a data frame with a numerical summary of the data frame.
#'
#' @export
#'
#' @examples
#' data("Bilagay")
#' subset_CHI <- Bilagay[Bilagay[,"Location"] %in% "CHI",]
#' summariseIsotopeData(subset_CHI, grouping = c("Spp", "FG"))

summariseIsotopeData <- function (df = NULL,
                                  grouping = c("Species", "FG"),
                                  order = FALSE,
                                  # ordering = NULL,
                                  printSummary = FALSE, ...){

  # Stupid CRAN fix for variables - see here http://stackoverflow.com/questions/
  # 9439256/how-can-i-handle-r-cmd-check-no-visible-binding-for-global-variable-
  # notes-when
  # As seen in https://github.com/andrewcparnell/simmr/blob/master/R/plot.simmr_output.R
  d13C = d15N = NULL

  if (is.null(checkNames(df, c("d13C", "d15N", grouping))))
    stop("Check the grouping variable or the names in your dataframe")

  # if (!is.null(ordering))
  #   if (is.null(checkNames(df, ordering)))
  #     stop("Check the ordering variable in your dataframe")

  summary <- plyr::ddply(df, grouping, plyr::summarise,
                         n_d13C = length(d13C),
                         mean_d13C = round(mean(d13C), 1),
                         SD_d13C = round(stats::sd(d13C), 1),
                         #SE_d13C = round(SD_d13C / sqrt(n_d13C), 1),
                         n_d15N = length(d15N),
                         mean_d15N = round(mean(d15N), 1),
                         SD_d15N = round(stats::sd(d15N), 1))
                         #SE_d15N = round(SD_d15N / sqrt(n_d15N), 1))

  # ddimsSortedTable <- ddply(ddply(diamonds, .(color),
  #                                 summarise, depth = mean(depth), table = mean(table)), .(table))


  # # Strange error (can't use neither NULL or FALSE)
  # if(!is.null(ordering)) {
  #   flag <- TRUE
  #   message("flag; ordering is not NULL")
  #   print(str(flag))
  # }
  #
  # if(isTRUE(ordering)) message("ordering is TRUE")
  # if(!isTRUE(ordering)) message("ordering is FALSE")
  #
  # message("Ordering:")
  # str(ordering)
  # message("Missing ordering:")
  # print(missing(ordering))

  # if (!is.null(ordering)){
  #   str(ordering)
  #   message("not null?")
  #   if (!is.null(factor(df[,ordering]))) {
  #     print("we are in?")
  #
  #     summary[,grouping] <- factor(summary[,grouping],
  #                                  levels = levels(reorder(factor(
  #                                    df[,grouping]), df[,ordering])),
  #                                  ordered = TRUE)
  #
  #     summary <- dplyr::arrange_(summary, grouping)
  #
  #   }
  # }

  if (isTRUE(order)) {
    if (length(grouping) == 1) {
      # Not sure why I put this here
      summary[,grouping] <- factor(summary[,grouping],
                                   levels = levels(df[,grouping]),
                                   ordered = TRUE)

      summary <- summary[order(df[grouping]),]

    } else {
      Spp <- grouping[[1]]
      FG <- grouping[[2]]

      summary[,Spp] <- factor(summary[,Spp])
      summary[,FG] <- factor(summary[,FG])

      summary[,Spp] <- factor(summary[,Spp],
                              levels = summary[,Spp][order(summary[,FG])])

      summary <- summary[order(summary[FG]),]
    }

    }

    if (isTRUE(printSummary))  print(summary)

  return(summary)

}
