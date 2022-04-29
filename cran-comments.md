## Updates

This is an update to correct an issue in R-check results that triggered warning. We have corrected this (\href{mailto: ..} pls drop the space after mailto:..).

Across operational systems we tested, there are no errors, one platform-specific warning and three notes that can be ignored. See below for the notes.

It will be great if you can help us publish this update. Thank you very much! Feel free to let us know if you have any questions. 

## Test environments
* local Windows 10, R 4.1.3
* check_rhub() 
* check_win_devel()

## R CMD check results
0 errors √ | 0 warnings √ | 1 notes x (local Windows 10 64-bit)
0 errors √ | 0 warnings √ | 1 notes √ (rhub, Ubuntu Linux)
0 errors √ | 1 warnings x | 1 notes √ (rhub, Fedora Linux)
0 errors √ | 0 warnings √ | 2 note x (rhub, windows server 2022)
0 errors √ | 0 warnings √ | 1 notes x  (win-builder)

## Reverse dependencies

There are no reverse dependencies.

---
Platform:	Fedora Linux, R-devel, clang, gfortran
! LaTeX Error: File `framed.sty' not found.
--- 
Warning

Platform:	Fedora Linux, R-devel, clang, gfortran
* checking re-building of vignette outputs ... WARNING
Error(s) in re-building vignettes:
--- re-building ‘Hom-vignette.Rmd’ using rmarkdown
Loading required package: lavaan
This is lavaan 0.6-11
lavaan is FREE software! Please report any bugs.
Loading required package: SEMsens
! LaTeX Error: File `framed.sty' not found.

This warning is platform specific, and the platform needs the framed package. 

---
Note 1
 checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
  Maintainer: 'Walter Leite <walter.leite@coe.ufl.edu>'
  
The switch of emails had been previously confirmed. 
---
Note 2
 Possibly misspelled words in DESCRIPTION:
    Suen (23:53)
  
  Found the following (possibly) invalid DOIs:
    DOI: 10.1177/00131644211073121
      From: DESCRIPTION
      Status: Service Unavailable
      Message: 503

This doi and name are correct.
--- 
Note 3 

 checking sizes of PDF files under 'inst/doc' ... NOTE
  Unable to find GhostScript executable to run checks on size reduction

Response: This may be due to the platform does not have GhostScript. 
---    
Note 4

checking for detritus in the temp directory ... NOTE
  Found the following files/directories:
    'lastMiKTeXException'

Response: This seems to be platform specific that can be ignored. 
