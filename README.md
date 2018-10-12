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
Upload the matrix of relative abundances produced from metaphlan2 or MIDAS as a <strong>tab separated</strong> text file. Samples can be in rows or columns. Choose the taxonomic level to plot and the number of taxa to show before grouping the rest into an "other" category. Click plot to redraw the barbplot after any changes.

## DOWNLOAD FORMATTED DATA
Formatted data can also be downloaded as a tsv. With the "condensed table" option, data is downloaded as a matrix and the taxonomy level and number of taxa will be whatever options are currently selected. Alternatively, the data can be downloaded as a melted (R reshape2) table with all taxonomic levels (not affected by current selections for taxonomic level  or number of taxa).
