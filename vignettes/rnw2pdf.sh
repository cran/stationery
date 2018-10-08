#!/bin/bash

## Paul Johnson
## 2018-02-10

scriptname=`basename $0 .sh`

## Default argument settings
pwd=`pwd`
declare -A parms
parms[engine]=\"knitr\"
parms[verbose]=FALSE
parms[clean]=TRUE
parms[tangle]=TRUE
## VERBOSE is local variable, will cause more script output,
## and set parms[verbose]=TRUE
VERBOSE=0
## DEBUG if for author of this script, for fixing arg parsing. Not for users
DEBUG=0

## Receive one filename, then build it.
compileOne(){
	filename=$1
	if [[ -e "$filename" ]]; then
		fn=$(basename "$filename")
		exten="${fn##*.}"
		showme "extension is $exten"
		## check extension, ignore case
		shopt -s nocasematch
		if [[ ("$exten" -eq "Rnw") || ( "$exten" -eq "lyx") ]]; then
			cmd="library(stationery); $scriptname(\"$filename\"$parmstring)"
			if [[ ${VERBOSE} -gt 0 ]]; then echo -e "Running: $cmd"; fi
			Rscript -e "${cmd}"
		else
			echo -e "Error: $filename. Extension should be \"Rnw\" or \"lyx\""
		fi
	else
 		echo -e "$filename not found"
	fi
}

## Scan directory for [Rnw] files and try to compile them
compileall() {
   	echo -e "\n""If no filename is specified, these files will be compiled:"
	find . -type f \( -iname "*.Rnw" -or -iname "*.lyx" \)
	echo -e "Hit Enter to continue, or \"q\" to quit"
	read -p "> " input
	if [[ $input == "q" ]]; then
   		exit 1
	else
		echo "Compiling them all"
		## check extension, ignore case
		shopt -s nocasematch
		for fn in *{.Rnw,.rnw,.lyx} 
		do
		 	compileOne "$fn"
		done
	fi
}

## Gracefully quit with error message
die() {
	printf '%s\n' "$1" >&2
	exit 1
}

## Only prints if DEBUG is set
showme(){
	if [ ${DEBUG} -gt 0 ]; then
		printf 'DEBUG: %s\n' "$1" >&2;
	fi
}

## Prints key=value pairs, one per line
printarr() {
	declare -n __p="$1"
	for k in "${!__p[@]}"
	do printf "     %s=%s\n" "$k" "${__p[$k]}"
	done
} 

## builds $parmstring by concatenating key=value pairs
catarr() {
	parmstring=""
	declare -n __p="$1"
	for k in "${!__p[@]}"
	do parmstring+=", $k=${__p[$k]}"
	done
} 



## Usage instruction.  
usage() {
	echo -e "\nUsage: $0 --arg="value" filename.[Rnw,lyx]".
	echo "Current arguments:"
	printarr parms
	catarr parms
	showme "${parmstring}"
    echo -e "\nThis script reformats and sends request to R:\n"
	echo -e "library(stationery); $scriptname(\"filename.Rnw\""$parmstring")\n"
    echo "Add argument -v for VERBOSE output from this script."
	echo "Any arguments described in documentation for $scriptname R function are allowed."
	echo "CAUTION"
    echo "Arguments that are quoted strings, such as"
    echo "--template=\"guide-template.tex\" need special care when entered from command line."
    echo "It is necessary to 'protect' (escape) the quotation marks."
    echo "We suggest this style:"
    echo "--template='\"guide-template.tex\"'"
}



optspec=":vh-:"
while getopts "$optspec" OPTCHAR; do
    case "${OPTCHAR}" in
        -)
			showme "OPTARG:  ${OPTARG[*]}"
			showme "OPTIND:  ${OPTIND[*]}"
			showme "OPTCHAR: ${OPTCHAR}"
			if [[ ${OPTARG}  == *"="* ]]; then
				showme "There is an equal sign in ${OPTARG}"
				opt=${OPTARG%=*}
				val=${OPTARG#*=}
				showme "--${opt} = \"${val}\""
			   	if [[ -z $val ]]; then
					die "ERROR: $opt is empty."
				fi
				parms[${opt}]=${val}
			elif [[ ${OPTARG} == "help" ]]; then
				## Finds "--help" Because no argument after help, then answer here
				usage
				exit
			fi
			;;
	    h)
            usage
	    	exit 2
		 	;;
        v)
			## if -v flag is present
			echo -e "\n\nverbose flag\n"
            VERBOSE=1
			parms["quiet"]=FALSE
			parms["verbose"]=TRUE
			## must print default parms here, before more parsing
			echo "Parameters are:" >&2
			printarr parms
			;;
        *)
             if [ "$OPTERR" != 1 ] || [ "${optspec:0:1}" = ":" ]; then
                 die "Undefined argument: '-${OPTARG}'"
             fi
             ;;
	esac
done

parmstring=""
catarr parms

## No shifts inside there, so must throw away arguments
## that were processed
shift "$((OPTIND-1))"

showme "After getopts, N of args left $#"
showme "Args:  $@"

if [ ${VERBOSE} -gt 0 ]; then
	echo -e "After parsing command line, the parameters are:"
	printarr parms
fi

## Retrieve the number of arguments that are left
nargs=$#
## If no arguments for file names, print usage, build all. 
if [[ $nargs -lt 1 ]]; then
	usage
	compileall
fi


for filename in "$@"; do
 	compileOne $filename
done

