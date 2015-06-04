#  Building and checking the package.
#  This will get added to the build-ignore file:
library(devtools)
library(codetools)

# Devtools - build and check the package first:
check()
build()

# Use static code analysis to look for possible bugs:
checkUsagePackage('neotoma', all = TRUE)
