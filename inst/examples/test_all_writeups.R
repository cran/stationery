## Paul Johnson
## 20180729

library(stationery)

## Change to the temporary directory
dir.orig <- getwd()
## if you want R's temp dir
subdirname <- "todaytest"
tdir <- file.path(tempdir(), subdirname)
## Otherwise, set a real directory so you can test.
tdir <- file.path("/tmp", subdirname)
if(!file.exists(tdir)) dir.create(tdir)
setwd(tdir)

## document types
doctype <- c("rmd2html-guide", "rmd2pdf-report",
             "rnw2pdf-guide-knit", "rnw2pdf-guide-sweave",
             "rmd2pdf-guide", "rnw2pdf-report-knit",
             "rnw2pdf-report-sweave", "rnw2pdf-slides-sweave")
folders <- vapply(doctype, function(x){
    initWriteup(x, dir = file.path(tdir, x))},
    character(1))
folders
list.files(file.path(tdir), recursive = TRUE)
## Compile one example in the rnw2pdf-report-sweave folder
list.files(file.path(tdir, "rnw2pdf-report-sweave"))
rnw2pdf("skeleton.Rnw", wd = file.path(tdir, "rnw2pdf-report-sweave"),
        engine = "Sweave")
## Check the pdf was created
list.files(file.path(tdir, "rnw2pdf-report-sweave"))
if(interactive()) browseURL(file.path(tdir, "rnw2pdf-report-sweave", "skeleton.pdf"))
     
## go into those directories and compile some documents

setwd(dir.orig)
list.files()
