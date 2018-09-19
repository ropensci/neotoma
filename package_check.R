#  Building and checking the package.
#  This will get added to the build-ignore file:
library(codetools)

# Devtools - build and check the package first:
devtools::check()
devtools::build()

# Use static code analysis to look for possible bugs:
checkUsagePackage('neotoma', all = TRUE)
