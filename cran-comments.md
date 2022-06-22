## Updates

This is an update to correct an issue in R-check results that triggered warning. We have corrected the errors associated with rebuilding vignettes.

Across operational systems we tested, there are no errors, and three notes can be ignored. See below for the notes and responses.

It will be great if you can help us publish this update. Thank you very much! Feel free to let us know if you have any questions. 

## Test environments
* local Windows 10, R 4.2.0
* check_rhub() 
* check_win_devel()

## R CMD check results
0 errors √ | 0 warnings √ | 0 notes √ (local Windows 10 64-bit)
0 errors √ | 0 warnings √ | 2 notes √ (rhub, Ubuntu Linux)
0 errors √ | 0 warnings √ | 1 notes √ (rhub, Fedora Linux)
0 errors √ | 0 warnings √ | 2 note x (rhub, windows server 2022)
0 errors √ | 0 warnings √ | 1 notes x  (win-builder)

## Reverse dependencies

There are no reverse dependencies.


---
Note 1
 checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
  Maintainer: 'Walter Leite <walter.leite@coe.ufl.edu>'
  
Response: The switch of emails had been previously confirmed. 

---
Note 2
 Possibly misspelled words in DESCRIPTION:
    Suen (23:53)
  
  Found the following (possibly) invalid DOIs:
    DOI: 10.1177/00131644211073121
      From: DESCRIPTION
      Status: Service Unavailable
      Message: 503

Response: The doi and name are correct.

---    
Note 3

checking for detritus in the temp directory ... NOTE
  Found the following files/directories:
    'lastMiKTeXException'

Response: This seems to be platform specific that can be ignored. 
