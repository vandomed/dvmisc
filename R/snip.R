#' Prep for Snipping or Copy/Pasting out of R
#' 
#' Calls \code{\link[knitr]{kable}} and \code{\link[kableExtra]{kable_styling}} 
#' as appropriate based on class of input object and creates object ready to 
#' snip or copy/paste out of R. Currently works for data frames, matrices, and 
#' tables.
#' 
#' @param x Object to snip
#' @param ... Arguments to pass to \code{\link[kableExtra]{kable_styling}}.
#' 
#' 
#' @examples 
#' # Crosstab of gears and cylinder using mtcars dataset
#' snip(table(mtcars$cyl, mtcars$gear))
#' 
#' # Piping works, but you lose the group labels
#' table(mtcars$cyl, mtcars$gear) %>% snip()
#' 
#' 
#' @export
snip <- function(x, ...) {
  
  classx <- class(x)
  if ("data.frame" %in% classx | "matrix" %in% classx) {
    return(x %>% kable() %>% kable_styling(...))
  }
  
  if ("table" %in% classx) {
    
    # If table in name of input, extract variable names for labels
    xstring <- deparse(substitute(x))
    if (grepl("table", xstring, fixed = TRUE)) {
      
      # Figure out variable names
      # Drop spaces, split at comma, drop single ticks, and take whatever comes after $
      varnames <- unlist(strsplit(gsub(")", "", xstring), ", "))
      varnames <- sapply(varnames, function(x) {
        loc <- which(unlist(strsplit(x, "")) == "$")
        gsub("`", "", substring(x, (loc + 1)))
      })
      
      # Convert table to matrix and get group levels
      y <- as.matrix(x)
      rnames <- rownames(y)
      cnames <- colnames(y)
      
      # Add X groups as first column
      y <- matrix(as.character(y), nrow = nrow(y))
      y <- cbind(rnames, y)
      
      # Add Y group as column names
      colnames(y) <- c("", cnames)
      
      # Set first column to x variable name
      y <- cbind(varnames[1], y)
      
      # Create header for y variable name
      header <- c(2, ncol(y) - 2)
      names(header) <- c(" ", varnames[2])
      
      # Output
      return(y %>%
               kable() %>%
               kable_styling(...) %>%
               column_spec(column = 1: 2, bold = TRUE) %>%
               collapse_rows(1) %>%
               add_header_above(header = header))
      
    }
    
    # Just directly call kable and kable_styling
    return(x %>% kable() %>% kable_styling(...))
    
  }
  
}