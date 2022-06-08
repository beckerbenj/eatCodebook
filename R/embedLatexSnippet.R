
####
#############################################################################
#' Embed a Latex Snippet.
#'
#' Embed a Latex Snippet in the Latex environment used by \code{\link{codebook}}..
#'
#'@param snippet A Latex snippet.
#'
#'@return A testable latex script.
#'
#'@examples
#'latexSnippet <- embedLatexSnippet("This is a sentence.")
#'
#'@export
embedLatexSnippet <- function(snippet) {
  if(!is.character(snippet)) stop("'snippet' must be a character vector.")

  layout.prae.ges <- layout.prae(var.all = "var1", fbshort= "test", double.vars= character(), deckblatt = "",
                                 makeCounter = FALSE, maxLength.Chap = 10, maxLength.Sec = 10,
                                 maxLength.Subsec = 10, maxLength.Subsubsec = 10)

  c(layout.prae.ges, snippet, "\\end{document}")
}


