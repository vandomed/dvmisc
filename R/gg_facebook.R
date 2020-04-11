#' Quickly Format and Save a ggplot Object for Posting to Facebook
#'
#' Optionally resizes title and axis labels and saves a 1000 dpi png 4 inches 
#' tall and ~7.6 inches wide via \code{\link[ggplot2]{ggsave}}. Values are 
#' adjustable.
#'
#' @param plot \code{\link[ggplot2]{ggplot}} object
#' @param filename Character string
#' @param resize_labels Logical value
#' @param title_size,axis_size Numeric value
#' @param height,width Numeric value
#' @param dpi Numeric value
#'
#' @return NULL
#' 
#'
#' @export
gg_facebook <- function(plot = last_plot(), 
                        filename = paste("fig1-", Sys.Date(), ".png", sep = ""), 
                        resize_labels = TRUE, 
                        title_size = 16, 
                        axis_size = 13, 
                        height = 4, 
                        width = 4 / 0.525, 
                        dpi = 1000) {
  
  if (resize_labels) {
    plot <- plot + 
      theme(
        plot.title = element_text(size = title_size), 
        axis.title = element_text(size = axis_size)
      )
  }
  
  ggsave(filename, plot = plot, height = height, width = width, dpi = dpi)
  NULL
  
}
