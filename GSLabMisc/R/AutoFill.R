#' Export content as TeX macro.
#'
#' @param commandname Command name.
#' @param content Content you would like to export.
#' @param outfile File in which you would like to store the macro.
#' @param mode If specified as "text", will appear as normal text even in math mode. If this is undesirable, use "math".
#' @param appendmode If TRUE, appends to an existing file.
#'
#' @importFrom utils write.table capture.output
#' @export
#' @examples
#' \dontrun{
#' AutoFill(commandname = "pValue", content = "0.0003", "pvalue.tex", mode = "math")
#' 
#' # If you would like to export in text mode:
#' AutoFill(commandname = "SampleStart", content = "January 2010", "samplestart.tex", mode = "text")
#' }

AutoFill <- function(commandname, content, outfile, mode = "math", appendmode = FALSE) {
    if (mode == "text") {
        utils::write.table(utils::capture.output(cat(sprintf("\\newcommand{\\%s}{\\textnormal{%s}}", commandname, content))),
                    outfile, quote = F, col.names = F, row.names = F, append = appendmode)
    } else if (mode == "math") {
        utils::write.table(utils::capture.output(cat(sprintf("\\newcommand{\\%s}{%s}", commandname, content))),
                    outfile, quote = F, col.names = F, row.names = F, append = appendmode)
    }
}
