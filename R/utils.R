##' Retrieve theme files
##'
##' A wrapper for file.copy that retieves files from the dn = "theme"
##' directory of a package
##' @param fn Vector of file names
##' @param dn "theme", or directory name used in package inst for
##'     theme information.
##' @param pkg Package name, default "stationery"
##' @return TRUE if succeeded
##' @author Paul Johnson<pauljohn@@ku.edu>
##' @export
getFiles <- function(fn, dn = "theme", pkg = "stationery"){
    fc <- function(x, dn){
        if(!dir.exists(dn)) stop(paste("getFiles fc dir", dn, "does not exist"))
        if(file.exists(file.path(dn, x))) return(TRUE)
        result <- file.copy(from = system.file(file.path(dn, x), package = pkg), to = dn)
    }
    ## Check no dir exists and no file named theme exists
    if(!dir.exists(dn)){
        if(!file.exists(dn)) dcreate <- dir.create(dn, recursive = TRUE, showWarnings = FALSE)
        else stop("getFiles cannot create theme directory")
    }
    result <- vapply(fn, fc, dn = dn, logical(1))
    if(any(!result))stop("getFiles failed")
    TRUE
}
