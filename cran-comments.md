## Test Environments
+ local windows x86_64-w64-mingw32 R 3.3.1 passes.
+ Linux - [Travis](https://travis-ci.org/ropensci/neotoma) tests are passing. (Note:  [Appveyor](https://ci.appveyor.com/project/sckott/neotoma/branch/master) tests are failing, but likely as a result of an upstream error "can't find Rcpp", poss. related to https://github.com/hadley/devtools/issues/1246).

## R CMD check results:
+ There were no ERRORs or WARNINGs
+ Travis gives no notes.
+ Appveyor fails, but see note above.
  
## Downstream Dependencies:
NA