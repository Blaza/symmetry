##  Resubmission

This is a resubmission in response to ERRORs on brought to my attention by Prof
Brian Ripley:

> 1) use of bind seen on fedora-clang and macOS (at least for me).  My 
> guess is that you meant std::bind (several times).
> 
> 2) misuse of sqrt<int> showing on Solaris (and warned about in 'Writing 
> R Extensions') ...

I fixed the errors and ran additional checks on r-hub on the following platforms
which all returned OK:

* Fedora Linux, R-devel, clang, gfortran
* macOS 10.11 El Capitan, R-release
* Oracle Solaris 10, x86, 32 bit, R-patched

The results of other platforms tested are as in the first submission below, i.e.
a NOTE related to the use of Rcpp, along with a NOTE on some platforms:

Possibly mis-spelled words in DESCRIPTION:
  heteroskedasticity
  
COMMENT: I have checked the spelling and it is correct.

---------------- Previous submission(s) below ----------------

##  Resubmission

This is a resubmission in response to the comments on the initial submission.
These were the comments:

> Please always explain all acronyms in the description field.

I have corrected this.

> If there are references describing the methods in your package, please 
add these in the description field of your DESCRIPTION file in the form...

There are no notable references that I would put in this field at the moment.

Thanks!

---------------- Previous submission below ----------------

## Test environments
* local Linux Mint 19.2 install, R 3.6.1
* R-hub (the 4 default rhub::check_for_cran() platforms)
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
