## Test environments
* local Linux Mint 19.2 install, R 3.6.1
* R-hub (4 default rhub::check_for_cran() platforms)
* win-builder (release)

## R CMD check results
There were no ERRORs or WARNINGs. 

Depending on the platform, there were a couple of NOTEs:

1. Local build and R-hub Ubuntu 16.04 build:
   * checking installed package size ... NOTE
       
       installed size is  7.4Mb
       sub-directories of 1Mb or more:
         libs   7.2Mb
  
  COMMENT:
  This is probably because Rcpp is used and most of the functions are C code.

2. R-hub - Windows Server build:
   * checking for non-standard things in the check directory ... NOTE
     'examples_i386' 'examples_x64' 'symmetry-Ex_i386.Rout'
     Found the following files/directories:
     'symmetry-Ex_x64.Rout' 'tests_i386' 'tests_x64'
  
  COMMENT:
  This is only on windows and is built by r-hub, those directories don't exist
  in the package.

3. All platforms
   checking CRAN incoming feasibility ... NOTE
   Maintainer: ‘Blagoje Ivanović <blagoje_ivanovic@matf.bg.ac.rs>’

   New submission
   
  COMMENT:
  This is self-explanatory
