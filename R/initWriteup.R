
##' Create skeleton for a report or guide. 
##'
##' The installed package includes a set of folders with document
##' templates. \code{initWriteup} is simply an automated way to copy
##' the template folder and skeleton document file into a new
##' directory.
##' \cr
##' \cr
##' The current formats are: \enumerate{
##' \item rmd2html-guide  
##' \item rmd2pdf-report
##' \item rnw2pdf-guide-sweave  
##' \item rnw2pdf-report-sweave  
##' \item rmd2pdf-guide
##' \item rnw2pdf-guide-knit
##' \item rnw2pdf-report-knit
##' \item rnw2pdf-slides-sweave
##' }
##' The names represent the "from" format (rmd or rnw), the "to"
##' format (html or pdf), the document type (guide or report), and the
##' chunck processing program (Sweave or knitr).
##' \cr
##' \cr
##' Each selection offers a self-contained working document and enough
##' information to compile that document. It also includes a shell script
##' that can compile the document.
##'
##' @section Details:
##'
##' The examples demonstrate all of the mix-and-match combinations of
##' input document formats, output formats, and document types. The issues
##' involved are more fully explained in the vignettes provided with the
##' package, but a nutshell summary would be as follows.
##' 
##' Report or Guide?
##' 
##' In our terminology, a guide is a document that includes "raw" code
##' examples and possibly output. A guide is intended for
##' training/teaching purposes, it might be offered on
##' http://crmda.ku.edu/guides. In contrast, a report generally
##' will now include "raw" code and only very rarely will it have
##' raw output.  Calculations should create tables and plots that
##' are inserted as (in LaTeX, floating) tables and figures.
##'
##' Markdown ("*.Rmd") or Noweb ("*.Rnw")?
##' 
##' Documents can be prepared in markdown "Rmd" or R noweb Rnw. Either
##' type of document can include code chunks. For creating HTML
##' output, markdown is perhaps the most workable format. For creating
##' PDF output, the noweb document format is better. When PDF output
##' is desired, a markdown document must be converted to tex and then to
##' PDF. Some LaTeX features are lost in that process, but they are
##' not lost if we prepare the document in a noweb format (which *is*
##' a LaTeX file).  The details are discussed in the vignettes available
##' with this package.
##' 
##' PDF or HTML?
##' 
##' The allowed components in a report or guide depend on whether the
##' eventual output is HTML or PDF.  Many features intended for PDF
##' outpu will not work if the backend is changed to HTML.  The
##' converse is also true.  While this may change in the future, at
##' the current time, only the most basic document which does not
##' include most of the features that we truly need will be compatible
##' with both PDF and HTML output.  That's why we need to have so many
##' mix-and match combinations of document types and output formats.
##'
##' 
##'
##' We may have slide templates as well, at some point in future.
##'
##' 
##' @param type One of these character strings:
##'     \code{c("rmd2html-guide", "rmd2pdf-report",
##'     "rnw2pdf-guide-knit", "rnw2pdf-guide-sweave", "rmd2pdf-guide",
##'     "rnw2pdf-report-knit", "rnw2pdf-report-sweave",
##'     "rnw2pdf-slides-sweave")}
##' @param dir Directory into which files are inserted. Type "." for
##'     current working directory.  Default is a new directory with
##'     name equal to the \code{type} parameter
##' @importFrom kutils initProject
##' @export
##' @return The normalized path of the new directory
##' @author Paul Johnson <pauljohn@@ku.edu>
##' @examples
##' tdir <- tempdir()
##' cat("Will Create All Doc Types in Temp Dir:\n ", tdir, "\n")
##'
##' doctype <- c("rmd2html-guide", "rmd2pdf-report",
##'           "rnw2pdf-guide-knit", "rnw2pdf-guide-sweave",
##'           "rmd2pdf-guide", "rnw2pdf-report-knit",
##'           "rnw2pdf-report-sweave", "rnw2pdf-slides-sweave")
##' folders <- vapply(doctype, function(x){
##'                initWriteup(x, dir = file.path(tdir, "todaytest", x))},
##'            character(1))
##' folders
##' list.files(file.path(tdir, "todaytest"), recursive = TRUE)
##' ## Compile one example in the rnw2pdf-report-sweave folder
##' list.files(file.path(tdir, "todaytest/rnw2pdf-report-sweave"))
##' \donttest{
##' ## requires pdflatex on system
##' rnw2pdf("skeleton.Rnw", wd = file.path(tdir, "todaytest/rnw2pdf-report-sweave"),
##'          engine = "Sweave")
##' ## Check the pdf was created
##' list.files(file.path(tdir, "todaytest/rnw2pdf-report-sweave"))
##' if(interactive()) browseURL(file.path(tdir, "todaytest/rnw2pdf-report-sweave", "skeleton.pdf"))
##' unlink(file.path(tdir, "todaytest"), recursive = TRUE)
##' }
initWriteup <- function(type, dir = type)
{
    wd <- getwd()
    on.exit(setwd(wd))
    ## Only create dir if dir NULL
    if (!dir.exists(dir)){
        dir.create(dir, recursive = TRUE)
    }

    dir.path <- system.file("rmarkdown/templates/", type, "skeleton", package = "stationery")
    
    if (dir.path == "") {
        messg <- paste0("type", type, "not found")
        cat(messg)
        return(invisible(NULL))
    }

    file.copy(from = Sys.glob(paste0(dir.path, "/*")), to = dir,
              recursive = TRUE, copy.date = TRUE) 
    normalizePath(dir)
}

    
