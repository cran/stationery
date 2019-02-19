for i in rn*knit*; do
    echo $i
    cd $i/skeleton
    for j in *lyx; do
        lyx -f all -e knitr $j
        ./rnw2pdf.sh '--engine="knitr"' $j
    done
    ls -la
    cd ../../
done



for i in rn*sweave; do
    echo $i
    cd $i/skeleton
    echo $i
    for j in *lyx; do
        lyx -f all -e sweave $j
        ./rnw2pdf.sh '--engine="Sweave"' $j
    done
    rm -f Rplots.pdf
    rm -f *.Rout
    ls -la
    cd ../../
done



for i in rmd2html*; do
    echo $i
    cd $i/skeleton
    echo $i
    for j in *Rmd; do
       ./rmd2html.sh  $j
    done
    rm -f Rplots.pdf
    rm -f *.Rout
    ls -la
    cd ../../
done



for i in rmd2pdf*; do
    echo $i
    cd $i/skeleton
    echo $i
    for j in *Rmd; do
       ./rmd2pdf.sh  $j
    done
    rm -f Rplots.pdf
    rm -f *.Rout
    ls -la
    cd ../../
done


# this for cleaning up Latex compiler files too
# for i in rn*sweave; do
#     echo $i
#     cd $i/skeleton
#     echo $i
#     cleanLatex.sh
#     rm -f Rplots.pdf
#     rm -f *.Rout
#     ls -la
#     cd ../../
# done
