Required Software
-----------------
*  R Programming Language
*  R Studio IDE

The following packages are required to ensure all the plots work. Please make sure all dependencies are installed since some of the packages like
 fastICA are implied in caret or any of the other packages:

*  caret, with all its dependencies
*  plyr
*  dplyr
*  reshape2
*  GGally
*  ggplot2
*  ggbiplot
*  magrittr
*  mlbench
*  gridExtra
*  mclust
*  dendextend
*  nnet
*  colorspace
*  e1071
*  randomForest
*  fpc
*  mass
*  flexclust

Folder Structure
----------------

*  R : contains the R code
*  data/winequality : location of the wine quality data set

the data for Pima Indian dataset is part of the mlbench package which would be installed as indicated above.


Running the code
----------------

Each file is self-contained but it is recommended that you run it
in numerical order. There are references to the other files in the R directory,
through the use of `source` code portion. So your working directory
should be the base directory where this readme is located.

Files:

*  00-pima.../00-wine... : these files are for the clustering graphs
*  01-pima.../01-wine... : these files are for the dimension reduction graphs
*  02-pima.../02-wine... : these files look at the unsupervised learning with and without dimension reduction
*  03-wine... : this is the file for Task 5
*  05-wine : this is the file for Task 4.
*  random_projection_gauss.R : script containing code to perform random projection.

