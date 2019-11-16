
##' Convert an Rmd file into an HTML file
##'
##' This is a very simple wrapper around the rmarkdown::render function.
##' It makes sure that the style sheet we want to use is applied to the data.
##'
##' Running this will be the same as running the rmd2html.sh script
##' within the directory.
##' @param fn One or more filenames ending in "*.Rmd".
##' @param wd A working directory name of the Rmd file. If not
##'     specified, then the current working directory from R will be
##'     used.
##' @param ... Arguments that will be passed to \code{rmarkdown::render} and
##'     \code{rmarkdown::html_document}. We usually have customizations
##'     via parameters \code{css} and \code{template},
##'     but many other parameters can be specified to change arguments for
##'     \code{rmarkdown::render} and \code{rmarkdown::html_documents}.
##'     These possible arguments for \code{html_document}: \code{c("toc",
##'     "toc_depth", "toc_float", "number_sections", "section_divs",
##'     "fig_width", "fig_height", "fig_retina", "fig_caption", "dev",
##'     "df_print", "code_folding", "code_download", "smart",
##'     "self_contained", "theme", "highlight", "mathjax", "template",
##'     "extra_dependencies", "css", "includes", "keep_md", "lib_dir",
##'     "md_extensions", "pandoc_args")}. These arguments intended for
##'     \code{render()} are allowed: \code{c("output_file", "output_dir",
##'     "output_options", "intermediates_dir", "knit_root_dir",
##'     "runtime", "clean", "params", "knit_meta", "envir",
##'     "run_pandoc", "quiet", "encoding")}.
##' @param verbose The opposite of render(quiet = TRUE). Shows compile
##'     commentary and pandoc command. Can be informative!
##' @param purl Default TRUE, synonym for tangle. Set either one, or
##'     set both same, result is same.
##' @param tangle Default TRUE, synonym for purl
##' @param backup Default TRUE. Will create backup copies of pdf, R, Rnw
##'     files before replacing them. Uses \code{kutils::file.backup}

##' @importFrom rmarkdown render
##' @importFrom rmarkdown html_document
##' @importFrom utils modifyList
##' @importFrom rmarkdown resolve_output_format
##' @return A vector of output file names
##' @author Paul Johnson <pauljohn@@ku.edu>
##' @export
##' @examples
##' \donttest{
##' tdir <- tempdir()
##' doctype <- "rmd2html-guide"
##' dirout <- initWriteup(doctype, dir = file.path(tdir, doctype))
##' list.files(dirout)
##' 
##' result <- try(rmd2html("skeleton.Rmd", wd = dirout))
##' if(inherits(result, "try-error")){
##'     MESSG <- paste("Compiling the markdown file failed, perhaps",
##'                   "your version of pandoc is not found")
##'     print(MESSG)
##' } else {
##'     ## Check the result file:
##'     MESSG <- paste("Check the directory", dirout, "for results.")
##'     print(MESSG)
##'     list.files(dirout)
##'     if(interactive() && file.exists(file.path(dirout, "skeleton.html"))) {
##'         browseURL(file.path(dirout, "skeleton.html"))
##'     }
##' }
##' unlink(dirout)
##' }
rmd2html <- function(fn = NULL, wd = NULL, ..., verbose = FALSE,
                     purl = TRUE, tangle = purl, backup = TRUE) {
    if (!missing(tangle) && is.logical(tangle)) purl <- tangle
    
    if (!is.null(wd)){
        wd.orig <- getwd()
        setwd(wd)
        on.exit(setwd(wd.orig))
    }
    dots <- list(...)

    ## Use recursion if several files
    if (length(fn) > 1){
        cl <- match.call()
        res <- c()
        for (x in fn){
            cl[["fn"]] <- x
            res <- c(res, eval(cl, parent.frame()))
        }
        return(res)
    }

    Rfn <- gsub("(.*\\.).*$", "\\1R", fn, ignore.case = TRUE)
    htmlfn <- gsub("(.*\\.).*$", "\\1html", fn, ignore.case = TRUE)

    if(backup) kutils::file.backup(htmlfn, verbose = FALSE, keep.old=TRUE)
    
    formals_render <- c("output_file", "output_dir", "output_options",
                        "intermediates_dir", "knit_root_dir",
                        "runtime", "clean", "params", "knit_meta",
                        "envir", "run_pandoc", "quiet", "encoding")
    
    dots_for_render <- dots[formals_render[formals_render %in% names(dots)]]

  
    dots_for_html_document <- dots[setdiff(names(dots), names(dots_for_render))]
    html_args <- list(toc = TRUE,
                      mathjax = "https://mathjax.rstudio.com/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML")
    
    html_argz <- utils::modifyList(html_args, dots_for_html_document, keep.null = TRUE)
    if(verbose) {print(paste("dots_for_html")); lapply(html_argz, print)}
    
    htmldoc <- rmarkdown::resolve_output_format(fn, output_options = html_argz)
    render_args <- list(input = fn, output_format = htmldoc, quiet = !verbose,
                            envir = globalenv())
    render_argz <- utils::modifyList(render_args, dots_for_render, keep.null = TRUE)
    if(purl){
        if(backup) kutils::file.backup(Rfn, verbose = FALSE, keep.old=TRUE)
        knitr::purl(fn, quiet = !verbose)
    }
    if(verbose) {print(paste("dots_for_render"));  lapply(dots_for_render, print)}
    res <- do.call(rmarkdown::render, render_argz)
    res
}




##' Custom output_format object to make MathJax work with custom HTML template
##'
##' The \code{rmarkdown::html_document} fails to use MathJax if a custom HTML
##' template is supplied. This is described here
##' \url{https://github.com/rstudio/rmarkdown/issues/727}.  The workaround
##' is to provide this wrapper.
##'
##' This function is needed only so as to "fool" pandoc in order to
##' make an HTML document compatible with MathJax. See the example
##' files created by \code{initWriteup("rmd2html-guide")} for a usage
##' example.
##' @param template Name of template file.
##' @param ... Any arguments passed along to html_document in
##'     rmarkdown
##' @return html_document object with custom template
##' @importFrom utils modifyList
##' @export
##' @author Paul Johnson
##' @examples
##' tdir <- tempdir()
##' doctype <- "rmd2html-guide"
##' dirout <- initWriteup(doctype, dir = file.path(tdir, doctype))
##' list.files(dirout)
##' MESSG <- paste("Inspect the YAML header output section of 'skeleton.Rmd'.",
##'                "It makes use of 'crmda_html_document'.")
##' cat(MESSG)
crmda_html_document <- function(template = "theme/guide-template.html", ...) {
    base_format <- rmarkdown::html_document(...)
    template_arg <- which(base_format$pandoc$args == "--template") + 1L
    if (!is.null(template))
        base_format$pandoc$args[template_arg] <- template
    base_format
}




##' Convert Rmd to PDF
##'
##' Convert an Rmd file to PDF by Sweaveing or knitting and then compiling. 
##'
##' Running this will be the same as running the rmd2pdf.sh script
##' within the directory.
##' @param fn One or more filenames ending in "*.Rmd". 
##' @param wd A working directory in which the Rmd file exists. Leave
##'     as NULL if file is in current working directory,
##' @param verbose The opposite of render(quiet = TRUE). Shows compile
##'     commentary and pandoc command. Can be informative!
##' @param purl Default TRUE
##' @param tangle Default TRUE, synonym for purl
##' @param backup Default TRUE. Will create backup copies of pdf, R, Rnw
##'     files before replacing them. Uses \code{kutils::file.backup}
##' @param ... Arguments that will be passed to \code{rmarkdown::render} and
##'     \code{rmarkdown::pdf_document}. Our defaults set a LaTeX template, toc =
##'     TRUE, and the pandoc_args includes use of the listings class.
##'     Users may override by specifying named arguments for
##'     \code{render()}: \code{c("output_file", "output_dir",
##'     "output_options", "intermediates_dir", "knit_root_dir",
##'     "runtime", "clean", "params", "knit_meta", "envir",
##'     "run_pandoc", "quiet", "encoding")}. Users may also specify
##'     named arguments for \code{pdf_document:} \code{("toc",
##'     "toc_depth", "number_sections", "fig_width", "fig_height",
##'     "fig_crop", "fig_caption", "dev", "df_print", "highlight",
##'     "template", "keep_tex", "latex_engine", "citation_package",
##'     "includes", "md_extensions", "pandoc_args",
##'     "extra_dependencies")}.
##' @importFrom kutils file.backup
##' @importFrom rmarkdown render
##' @importFrom rmarkdown pdf_document
##' @importFrom utils modifyList
##' @return A vector of output file names
##' @author Paul Johnson <pauljohn@@ku.edu>
##' @export
##' @examples 
##' tdir <- tempdir()
##' fmt <- "rmd2pdf-guide"
##' dirout <- initWriteup(fmt, dir = file.path(tdir, fmt))
##' print(dirout)
##' list.files(dirout)
##' \donttest{
##' of1 <- try(rmd2pdf("skeleton.Rmd", wd = dirout))
##' if(inherits(of1, "try-error")){
##'     MESSG <- paste("Compiling the markdown file failed, perhaps",
##'                   "you should run with parameters verbose=TRUE",
##'                   "and keep_tex=TRUE")
##'     print(MESSG)
##' } else {
##'     ## Check the result file:
##'     MESSG <- paste("Check the directory", dirout, "for results.")
##'     print(MESSG)
##'     list.files(dirout)
##'     if(interactive() && file.exists(file.path(dirout, "skeleton.pdf"))) {
##'         browseURL(of1)
##'     }
##' }
##' }
##' unlink(dirout)
rmd2pdf <- function(fn = NULL, wd = NULL, ..., verbose = FALSE,
                    purl = TRUE, tangle = purl, backup = TRUE){
    if(!missing(tangle) && is.logical(tangle)) purl <- tangle
    
    if (!is.null(wd)){
        wd.orig <- getwd()
        setwd(wd)
        on.exit(setwd(wd.orig))
    }    
    dots <- list(...)

    Rfn <- gsub("(.*\\.).*$", "\\1R", fn, ignore.case = TRUE)
    Rnwfn <- gsub("(.*\\.).*$", "\\1Rnw", fn, ignore.case = TRUE)
    texfn <- gsub("(.*\\.).*$", "\\1tex", fn, ignore.case = TRUE)
    pdffn <- gsub("(.*\\.).*$", "\\1pdf", fn, ignore.case = TRUE)

    if(backup){
        kutils::file.backup(pdffn, verbose = FALSE, keep.old=TRUE)
        kutils::file.backup(Rnwfn, verbose = FALSE, keep.old=TRUE)
        kutils::file.backup(Rfn, verbose = FALSE, keep.old=TRUE)
    }    
    if (length(fn) > 1){
        cl <- match.call()
        res <- c()
        for (x in fn){
            cl[["fn"]] <- x
            res <- c(res, eval(cl, parent.frame()))
        }
        return(res)
    }

    formals_render <- c("output_format", "output_file", "output_dir",
                        "output_options", "intermediates_dir",
                        "knit_root_dir", "runtime", "clean", "params",
                        "knit_meta", "envir", "run_pandoc", "quiet",
                        "encoding")
    dots_for_render <- dots[formals_render[formals_render %in% names(dots)]]

    ## 20180729: change design to reclaim all unused arguments
    ## formals_pdf_document <- c("toc", "toc_depth", "number_sections",
    ##                           "fig_width", "fig_height", "fig_crop",
    ##                           "fig_caption", "dev", "df_print",
    ##                           "highlight", "template", "keep_tex",
    ##                           "latex_engine", "citation_package",
    ##                           "includes", "md_extensions",
    ##                           "pandoc_args", "extra_dependencies")

    ## dots_for_pdf_document <- dots[formals_pdf_document[formals_pdf_document %in%  names(dots)]]
    dots_for_pdf_document <- dots[setdiff(names(dots), names(dots_for_render))]
       
    pdf_args <- list(highlight = "haddock",
                     pandoc_args = "--listings")
    pdf_argz <- utils::modifyList(pdf_args, dots_for_pdf_document)
    if(verbose) {print(paste("dots_for_pdf")); lapply(pdf_argz, print)}
   
    pdfdoc <- rmarkdown::resolve_output_format(fn, output_options = pdf_argz)
    render_args <- list(input = fn, output_format = pdfdoc, quiet = !verbose,
                        envir = globalenv())
    render_argz <- utils::modifyList(render_args, dots_for_render)
    if (purl) knitr::purl(fn)
    if(verbose) {print(paste("dots_for_render"));  lapply(dots_for_render, print)}
    res <- do.call(rmarkdown::render, render_argz)
    
    res
}






##' Convert (Sweave & compile) an Rnw or lyx file to PDF
##'
##' Documents saved with suffix ".lyx" or ".Rnw" will be converted.
##' Note it is very important to specify the engine for the code
##' chunks correctly, this can be either "Sweave" or "knitr".
##' 
##' @param fn One or more file names, should end in either ".Rnw" or
##'     ".lyx"
##' @param wd Directory in which the file to be converted
##'     exists. Leave NULL default if is in current working directory.
##' @param ... Other parameters, not used at the moment.
##' @param engine "knitr" or "Sweave"
##' @param purl Default TRUE. Synonym of tangle: extract R code chunks
##' @param tangle Same as purl, both parameters have same result
##' @param backup Default TRUE. Will create backup copies of pdf, R, Rnw
##'     files before replacing them. Uses \code{kutils::file.backup}
##' @param clean Default TRUE. Remove intermediate LaTeX files when
##'     using texi2pdf
##' @param quiet Default = TRUE.  No output unless an error occurs.
##'     Antonym for \code{verbose}.
##' @param verbose Default = \code{!quiet}. Antonym for
##'     \code{quiet}. Functions try to reduce amount of screen
##'     output. Knitr functions that use \code{quiet} flag will be set to
##'     \code{!verbose}.
##' @param envir environment for evaluation, see \code{knitr}
##'     documents, defaults to parent.frame().
##' @param encoding character encoding, defaults from user options
##' @return names of pdf output files
##' @importFrom kutils file.backup
##' @export
##' @author Paul Johnson <pauljohn@@ku.edu>
##' @importFrom knitr knit2pdf
##' @importFrom knitr knit
##' @importFrom utils Sweave
##' @importFrom utils Stangle
##' @importFrom tools texi2pdf
##' @examples
##' \donttest{
##' tdir <- tempdir()
##' fmt <- "rnw2pdf-guide-sweave"
##' dirout <- initWriteup(fmt, dir = file.path(tdir, fmt))
##' print(dirout)
##' list.files(dirout)
##' of1 <- try(rnw2pdf("skeleton.Rnw", engine = "Sweave", wd = dirout, backup = TRUE))
##' if(inherits(of1, "try-error")){
##'     MESSG <- paste("Compiling the markdown file failed, perhaps",
##'                    "there is an R or LaTeX error.", 
##'                    "Run again with parameters verbose=TRUE",
##'                    "and clean=FALSE")
##'     print(MESSG)
##' } else {
##'     ## Check the result file:
##'     MESSG <- paste("Check the directory", dirout, "for results.")
##'     print(MESSG)
##'     list.files(dirout)
##'     if(interactive() && file.exists(of1)) {
##'         browseURL(of1)
##'     }
##' }
##' unlink(dirout)
##' }
rnw2pdf <- function(fn = NULL, wd = NULL, ..., engine = "knitr", purl = TRUE,
                    tangle = purl, backup = TRUE,  clean = TRUE, quiet = TRUE, verbose = !quiet,
                    envir = parent.frame(), encoding = getOption("encoding"))
{
    engine <- tolower(engine)
    if(!missing(tangle) && is.logical(tangle)) purl <- tangle
    if(tangle != purl) {
        MESSG <- "rnw2pdf: tangle and purl have the same effect. Just set 1 of them"
        stop(MESSG)
    }
    
    wd.orig <- getwd()
    if (!is.null(wd)) {
        setwd(wd)
        on.exit(setwd(wd.orig))
    }

    dots <- list(...)
    
    isWindoze <- if(Sys.info()[['sysname']] == "Windows") TRUE else FALSE
    sysnull <-  if(isWindoze) "> nul" else " > /dev/null"
    
    sysrun <- function(cmd){
        if (isWindoze){
            res <- tryCatch(shell(cmd, intern = TRUE))
        } else {
            res <- tryCatch(system(cmd, intern = TRUE))
        }
        res
    }
    
    ## Use recursion if several files
    if (length(fn) > 1){
        cl <- match.call()
        res <- c()
        for (x in fn){
            cl[["fn"]] <- x
            res <- c(res, eval(cl, parent.frame()))
        }
        return(res)
    }
    
    rptdate <- format(Sys.time(), "%Y%m%d%H%M")
    bakstrng <- paste0("-uniquebackupstring", rptdate)

    run <- function(cmd){
        if (isWindoze) shell(cmd) else system(cmd)
    }

    ## tangle an Rnw file that has split=TRUE in SweaveOpts
    ##
    ## If split=TRUE, R tangle fails. This fixes that by
    ## creating temp Rnw file with split=FALSE, and tangling that.
    ##
    ## @param x file name
    ## @return Text with name of R file that was created
    ## @author Paul Johnson <pauljohn@@ku.edu>
    tangleSplit <- function(x){
        fnbackup <- gsub("(.*)(\\..*$)", paste0("\\1", bakstrng, "\\2"), x)
        file.copy(x, fnbackup, overwrite = TRUE)
        rnwfile <- readLines(fnbackup)
        rnwfile[grep("SweaveOpts", rnwfile)] <-
            gsub("(split\\s*=)\\s*.*,", "\\1FALSE,",
                 rnwfile[grep("SweaveOpts", rnwfile)])
        ## sets the prompt at "> "
        rnwfile[grep("prompt", rnwfile)] <- gsub("prompt\\s*=.*\"", "prompt=\"> \"",
                                                 rnwfile[grep("prompt", rnwfile)])
        writeLines(rnwfile, con = fnbackup)
        
        utils::Stangle(fnbackup, quiet=quiet)

        fnbackupR <- gsub("\\.Rnw", ".R", fnbackup)
        fnR <- gsub(bakstrng, "", fnbackupR)
        MESSG <- paste("tangleSplit(", x, "failed creation of R tangle file")
        if(file.exists(fnbackupR)){
            if (backup) kutils::file.backup(fnR, keep.old=TRUE)
            fnRrename <- file.copy(fnbackupR, fnR, overwrite = TRUE)
            if (!fnRrename) warning(MESSG)
            unlink(paste0("*", bakstrng, "*"))
        } else {
            warning(MESSG)
        }
        if(file.exists(fnbackup)) unlink(fnbackup)
        fnR
    }

    compileme <- function(x) {
        Rfn <- gsub("(.*\\.).*$", "\\1R", fn, ignore.case = TRUE)
        Rnwfn <- gsub("(.*\\.).*$", "\\1Rnw", fn, ignore.case = TRUE)
        texfn <- gsub("(.*\\.).*$", "\\1tex", fn, ignore.case = TRUE)
        pdffn <- gsub("(.*\\.).*$", "\\1pdf", fn, ignore.case = TRUE)

        if(backup){
            kutils::file.backup(pdffn, verbose = FALSE, keep.old=TRUE)
            kutils::file.backup(Rnwfn, verbose = FALSE, keep.old=TRUE)
            kutils::file.backup(Rfn, verbose = FALSE, keep.old=TRUE)
        }
        
        if (length(grep("\\.lyx$", tolower(x)))){
            ## Let lyx compile to pdf

            cmd <- paste("lyx -f all -e pdf2 ", x, if(!verbose) sysnull)
            run(cmd)
            ## Create Rnw file
            cmd <- paste("lyx -f all -e", engine, x, if(!verbose) sysnull)
            run(cmd)
                
            if(tangle){
                ## Remove previous R file, avoid confusion
                if(backup) kutils::file.backup(Rfn, keep.old=TRUE)
                unlink(Rfn)
                ## lyx can directly export r code from knitr engine file 
                if (engine == "knitr"){
                    cmd <- paste("lyx -f all -e r ", x, if(!verbose) sysnull)
                    run(cmd)
                } else {
                    rfile <- tangleSplit(Rnwfn)
                }
            }   
            
        } else if (length(grep("\\.rnw$", tolower(x)))) {
            if (engine == "knitr"){
                if(tangle){
                    kutils::file.backup(Rfn, keep.old=TRUE)
                    knitr::knit(x, quiet = !verbose, tangle = tangle, envir = envir,
                                encoding = encoding)
                }
                pdffn <- knitr::knit2pdf(x, quiet = !verbose, envir = envir)
            } else {
                utils::Sweave(x, quiet = !verbose)
                if (tangle) {
                    rfile <- tangleSplit(x)
                }
                ## 20180731: Try built-in texi2pdf again, instead of home-made method
                tools::texi2pdf(texfn, clean = clean, quiet = quiet)
            }
        }
        
        if (file.exists(pdffn)) {
            return(pdffn)
        } else {
            MESSG <- paste("PDF creation failed:", x)
            warning(MESSG)
            return(MESSG)
        }
    }
    res  <- compileme(fn)
    res
}
