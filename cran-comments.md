## Test Environments
+ local windows x86_64-w64-mingw32 R 3.2.0
+ Linux - [Travis](https://travis-ci.org/ropensci/neotoma) and [Appveyor](https://ci.appveyor.com/project/sckott/neotoma/branch/master) tests are passing.

## R CMD check results:
+ There were no ERRORs or WARNINGs
+ Travis gives two notes:
  +  Cyclic repository
  +  License file
+ In the last submission Uwe pointed out several "no visible binding" warnings.  I am unable to replicate this warning but have tried to address it by explicitly binding to non-base functions where possible.
  
## Downstream Dependencies:
NA