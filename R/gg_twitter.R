#' Quickly Format and Save a ggplot Object for Posting to Twitter
#'
#' Optionally resizes title and axis labels and saves a 600 dpi png 5 inches
#' tall and ~8.9 inches wide via \code{\link[ggplot2]{ggsave}}. Values are
#' adjustable.
#'
#' @param filename Character string
#' @param path Character string
#' @param plot \code{\link[ggplot2]{ggplot}} object
#' @param resize_labels Logical value
#' @param title_size Numeric value
#' @param axistitle_size Numeric value
#' @param axistext_size Numeric value
#' @param height Numeric value
#' @param width Numeric value
#' @param dpi Numeric value
#'
#'
#' @export
gg_twitter <- function(filename = paste("fig1-", Sys.Date(), ".png", sep = ""),
                       path = getwd(),
                       plot = last_plot(),
                       resize_labels = TRUE,
                       title_size = 16,
                       axistitle_size = 13,
                       axistext_size = 9,
                       height = 5,
                       width = height / 0.5625,
                       dpi = 600) {

  if (resize_labels) {
    plot <- plot +
      theme(
        plot.title = element_text(size = title_size),
        axis.title = element_text(size = axistitle_size),
        axis.text = element_text(size = axistext_size)
      )
  }

  ggsave(
    filename = filename,
    plot = plot,
    path = path,
    width = width,
    height = height,
    dpi = dpi
  )

}
