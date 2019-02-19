
##' create white background empty PDF
##'
##' Sometimes you need a placeholder. If you do, this is it!
##' @param file A file name for output
##' @param height inches
##' @param width inches
##' @param messg Default is a reminder statement, but set "" if you don't want it.
##' @param pointsize Default is 12
##' @param col Color for text in \code{messg} parameter. Default is \code{gray50}
##' @return NULL is returned if file was created. Otherwise error is reported.
##' @importFrom grDevices dev.off
##' @importFrom grDevices pdf
##' @importFrom graphics par
##' @importFrom graphics plot
##' @importFrom graphics text
##' @export
##' @author Paul Johnson
##' @examples
##' tdir <- tempdir()
##' dir.create(file.path(tdir, "blanks"), recursive = TRUE)
##' fn1 <- file.path(tdir, "blanks", "blank1.pdf")
##' blankPDF(file = fn1, messg = "Do you want a message?")
##' ## Please inspect
##' if(interactive()) browseURL(fn1)
##' fn2 <- file.path(tdir, "blanks", "blank2.pdf")
##' blankPDF(file = fn2, height = 2, width = 3, messg = "")
##' if(interactive()) browseURL(fn2)
##' ## delete test directory
##' unlink(file.path(tdir, "blanks"), recursive = TRUE)
##' 
blankPDF <- function(file, height=1, width=3.5, messg = "Your Logo Could Be Here",
                     pointsize=12, col = "gray50"){
    pdf(file = file, height=height, width=width, paper="special", pointsize=pointsize)
    par(mar=c(0,0,0,0)) 
    plot(1:2, 1:2, type="n", xlab="", ylab="", axes=FALSE)
    text(1.5, 1.5, messg, col = col)
    dev.off()
    if(file.exists(file)) return(NULL) else stop("blankPDF file error")
}


##' create white background empty PNG
##'
##' Sometimes you need a placeholder. If you do, this is it!
##' @param file A file name for output
##' @param height pixels
##' @param width pixels
##' @param messg Default is a reminder statement inside the resulting image, but set "" if you don't want it.
##' @param pointsize pointsize 12 default
##' @param col Color for text in \code{messg} parameter. Default is \code{gray50}
##' @return NULL is returned if file was created. Otherwise error is reported.
##' @importFrom grDevices dev.off
##' @importFrom grDevices png
##' @importFrom graphics par
##' @importFrom graphics plot
##' @importFrom graphics text
##' @export
##' @author Paul Johnson
##' @examples
##' tdir <- tempdir()
##' dir.create(file.path(tdir, "blanks"), recursive = TRUE)
##' fn1 <- file.path(tdir, "blanks", "blank1.png")
##' blankPNG(file = fn1, messg = "Do you want a message?")
##' ## Please inspect
##' if(interactive()) browseURL(fn1)
##' fn2 <- file.path(tdir, "blanks", "blank2.png")
##' blankPNG(file = fn2, height = 2, width = 3, messg = "")
##' if(interactive()) browseURL(fn2)
##' ## delete test directory
##' unlink(file.path(tdir, "blanks"), recursive = TRUE)
##' 
blankPNG <- function(file, height=250, width=250, messg = "Your Logo Could Be Here",
                     pointsize=12, col="gray50"){
    png(filename=file, height=height, width=width, pointsize=pointsize)
    par(mar=c(0,0,0,0)) 
    plot(1:2, 1:2, type="n", xlab="", ylab="", axes=FALSE)
    text(1.5, 1.5, messg, col = col)
    dev.off()
    if(file.exists(file)) return(NULL) else stop("blankPNG file error")
}

