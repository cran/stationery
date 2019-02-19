##' Retrieve theme files
##'
##' A wrapper for file.copy that retieves files from the dn = "theme"
##' directory of a package.  It ties in the file.backup function from
##' kutils so that if a file is overwritten, then a backup copy is
##' created, using the last modification time of the file in the
##' backup file name.
##'
##' This was originally created because we wanted to protect user logo
##' and address files from accidental erasure.  Now it has a little
##' more versatility because \code{overwrite=TRUE} and
##' \code{backup=TRUE} add a little bit of valuable functionality.
##' When a file is overwritten, it should never be lost if
##' \code{backup=TRUE}.
##' @param fn A file name vector. In \code{stationery}, this is
##'     typically retrieving logo images or theme files.
##' @param pkg Package name, default "stationery"
##' @param overwrite Default FALSE, function returns TRUE, meaning
##'     "my work was done before."  If TRUE, old file will be
##'     replaced.
##' @param backup Default is TRUE, if a file is to be overwritten, then a
##'     backup will be created with kutils::file.backup.
##' @param pkgdir package directory name where files are
##'     found. Default is "theme" because that is used in
##'     \code{stationery} and related CRMDA packages.  It is the name
##'     of a directory in the package's \code{inst} folder, the same
##'     place where R \code{system.file} looks for files and
##'     directories.
##' @param outputdir Default is same as \code{pkgdir}. A directory
##'     where file file will be copied within the current working
##'     directory.
##' @return TRUE if succeeded, either a file existed before (and
##'     overwrite=FALSE) or a file was copied.
##' @importFrom kutils file.backup
##' @author Paul Johnson<pauljohn@@ku.edu>
##' @export
##' @examples
##' ## To demonstrate, we use a temporary directory.  Usage of setwd is discouraged
##' ## in examples, so this is a little bit more indirect than a real usage would be:
##' tdir <- tempdir()
##' list.files(file.path(tdir, "theme"))
##' getFiles("logo.pdf", pkg = "stationery", overwrite = TRUE, outputdir = file.path(tdir, "theme"))
##' list.files(file.path(tdir, "theme"))
##' getFiles("logo.pdf", pkg = "stationery", overwrite = TRUE, outputdir = file.path(tdir, "theme"),
##'          backup = TRUE)
##' list.files(file.path(tdir, "theme"))
##' unlink(file.path(tdir, "theme"), recursive = TRUE)
getFiles <- function(fn, pkg = "stationery",
                     overwrite = FALSE, backup = overwrite,
                     pkgdir = "theme", outputdir = "theme")
{
    fc <- function(x, dn){
        if(!dir.exists(system.file(dn, package = pkg))) stop(paste0("getFiles fc dir \"", dn, "\"does not exist"))
        if(file.exists(file.path(outputdir, x))){
            if(!overwrite){
                return(TRUE)
            }
            if (backup){
                kutils::file.backup(file.path(outputdir, x))
            }
        }
        result <- file.copy(from = system.file(file.path(dn, x), package = pkg), to = outputdir,
                            overwrite = TRUE, copy.date = TRUE)
    }
    ## Check no dir exists and no file named theme exists
    if(!dir.exists(outputdir)){
        if(!file.exists(outputdir)) dcreate <- dir.create(outputdir,
                                                          recursive = TRUE,
                                                          showWarnings = FALSE)
        else stop(paste("getFiles cannot create directory:", outputdir))
    }
    result <- vapply(fn, fc, dn = pkgdir, logical(1))
    if(any(!result))stop("getFiles failed")
    TRUE
}
NULL
