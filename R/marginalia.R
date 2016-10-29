#' Allow margin_note()-like content including inline chunks.
#' 
#' @param text Character string containing the text to be put in the margin. An inline chunk 
#' can be delimiter like this: @r 3 + 2@. That way, the function call (which must itself be in an
#' inline chunk) can itself contain an inline chunk for creating a table, etc.
#' @param move Distance in cm to move the note. positive means down, negative means up. Works only in PDF mode.
#' @examples
#' \dontrun{In an Rmd file, margin_content("We can add 3 + 2 to get @r 3 + 2@")}
#' @export
margin_content <- function(text, move = 0) {
    library(tint)
    text <- gsub('@r ([^@]*)\\s*@', "`r \\1`", text)
    text <- knitr::knit(text = text)
    # it seems we can only put a table in latex output.
    if (knitr:::is_latex_output()) {
       text <- sprintf("\\marginnote[%scm]{%s}", move, text)
    } 
    text
}

#' @export
margin_table <- function(data, 
                         show_rows = 5, declare_rows = nrow(data),
                         before = "hello", after = "after", move = 0) {
  tformat <- ifelse(knitr:::is_latex_output(), "latex", "markdown")
  table_text <- knitr::kable(data[1:show_rows,], format = tformat)
  margin_content(paste(before, "\n", table_text, after, collapse = "\n"),
                       move = move)
}


