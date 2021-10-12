## Updates

This is an update to correct an issue in rebuilding a vignette. We also updated the reference.

Across operational systems we tested, there are no errors, one warning is associated with Fedora Linux platform on Rhub (File `framed.sty' not found), and two notes can be ignored. See below for the warning and notes.

It will be great if you can help us publish this update. Thank you very much! Feel free to let us know if you have any questions. 

## Test environments
* local Windows 10, R 4.0.5
* check_rhub() 
* check_win_devel()

## R CMD check results
0 errors √ | 0 warnings √ | 0 notes √(local Windows 10 64-bit)
0 errors √ | 0 warnings √ | 0 notes √ (rhub, Ubuntu Linux)
0 errors √ | 0 warnings x | 0 notes √ (rhub, Fedora Linux)
0 errors √ | 0 warnings √ | 1 note x (rhub, windows server 2008)
0 errors √ | 0 warnings √ | 1 notes x  (win-builder)

## Reverse dependencies

There are no reverse dependencies.

---
Platform:	Fedora Linux, R-devel, clang, gfortran
! LaTeX Error: File `framed.sty' not found.
--- 
Note 1 (on R-hub and win-builder)
 checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
  Maintainer: 'Walter Leite <walter.leite@coe.ufl.edu>'
  
The switch of email had been previously confirmed. 
--- 
Note 2 on R-hub (only on Windows Server 2008 R2 SP1, not on Ubuntu Linux):

 checking sizes of PDF files under 'inst/doc' ... NOTE
  Unable to find GhostScript executable to run checks on size reduction

Response: This may be due to Windows Server does not have GhostScript?. 
---    


