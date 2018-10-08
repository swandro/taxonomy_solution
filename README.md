# taxonomy_solution

## OVERVIEW
Shiny app providing a GUI interface for making quick taxonomy barplots from an OTU table from Metaphlan or Midas.
Can handle taxonomy in any format (such as D_0__Bacteria;D_1__Actinobacteria;D_2__Actinobacteria;D_3__Actinomycetales;D_4__Actinomycetaceae;D_5__Actinomyces)
Can download the formatted taxonomy in an OTU table or a melted table for R analysis.

## REQUIREMENTS:
R Studio with the following packages installed: reshape2, ggplot2, shiny.  
Install a package with:
```
	 install.packages("shiny")
```

## HOW TO RUN
In R, run these two lines:
```
	library(shiny)
	runGitHub("taxonomy_solution","swandro")
```

## UPLOAD DATA
Upload the matrix of relative abundances produced from metaphlan2 or MIDAS as a tab separated text file. Samples can be in rows or columns.
