#!/usr/bin/env Rscript
# CLI Wrapper for stationery r*2pdf functions
# Garrett Mills <garrett@glmdev.tech>
# Modified: 2019 April 2

library(stationery)

options <- commandArgs(trailingOnly = TRUE)
supported_extensions <- c(".Rnw", ".lyx")
files <- c()
base_command <- "rnw2pdf"

# default arguments here -- e.g. "--engine=Sweave"
arguments <- c("--engine=knitr")

# A simple function for level-based debug messages
debug_level = 0
debug <- function(output, level=1) {
	if ( debug_level >= level ){
		cat("DEBUG: ")
		print(output)
	}
}

# Iterate over the input arguments and sort them
# arguments that don't begin with a '--' are considered file names
# arguments that DO are considered arguments to be passed to base_command
for ( option in options ){
	if ( startsWith(option, "--") ){
		arguments <- c(arguments, option)
	} else {
		files <- c(files, option)
	}
}

debug("Input files: ")
debug(files)

debug("Input arguments: ")
debug(arguments)

# display the usage information if requested
if ( "--help" %in% arguments ){
	print(help(base_command, help_type="html"))
	cat("Usage information requested. No files were modified.")
	
	# This is required to give the browser time to load the page before we exit. I know, I know.
	Sys.sleep(10)
} else {

	# if no input files are specified, prompt the user
	if ( is.null(files) ){
		cat("No input files specified. Enter the supported files to compile (all): ")
		in_files <- readLines("stdin", n=1)
		in_files <- strsplit(in_files, " ")

		debug("Files entered:")
		debug(in_files)

		if ( in_files[[1]][1] == "all" ){
			for ( extension in supported_extensions ){
				files <- c( files, Sys.glob( paste(sep="", "*", extension) ) )
			}
		} else {
			files <- c(in_files)
		}
	}

	# compile specified files
	for ( file in files ){
		
		# warn and break if the file doesn't end in the proper extension
		if ( !any( endsWith(file, supported_extensions) ) ){
			cat(paste("WARN: Skipping compilation of file with unsupported extension:", file, "\n"))
		} else {

			arg_string <- ""
			arg_names <- c()

			# convert the arguments to a parameter string
			for ( argument in arguments ){
				argument <- strsplit(argument, "=")
				if ( !( argument[[1]][1] %in% arg_names ) ){
					arg_names <- c(arg_names, argument[[1]][1])
					arg_string <- paste(arg_string, substring(argument[[1]][1], 3), "=\"", argument[[1]][2], "\"", ",", sep="")
				}
			}

			# remove the trailing comma
			arg_string <- substring(arg_string, 1, nchar(arg_string)-1)

			debug("Argument string:")
			debug(arg_string)

			# form the function call itself
			function_call <- paste(sep="", base_command, '("', file, '"')

			# if there are arguments to pass, add them
			if ( arg_string != "" ){
				function_call <- paste(sep="", function_call, ", ", arg_string)
			}

			# add the closing parenthesis
			function_call <- paste(sep="", function_call, ")")

			debug("Function call:")
			debug(function_call)

			# execute the function call as R code
			eval(parse(text=function_call))
		}

	}

}
