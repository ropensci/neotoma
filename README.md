neotoma
========

[![Build Status](https://api.travis-ci.org/ropensci/neotoma.png)](https://travis-ci.org/ropensci/neotoma)
[![Build status](https://ci.appveyor.com/api/projects/status/t2xyqbs0d8h998cb/branch/master)](https://ci.appveyor.com/project/sckott/neotoma/branch/master)
[![rstudio mirror downloads](http://cranlogs.r-pkg.org/badges/grand-total/neotoma)](https://github.com/metacran/cranlogs.app)
[![cran version](http://www.r-pkg.org/badges/version/neotoma)](http://cran.rstudio.com/web/packages/neotoma)

The `neotoma` package is a programmatic R interface to the [Neotoma Paleoecological Database](http://www.neotomadb.org/). The package is intended to both allow users to search for sites and to download data for use in analyical workflows of paleoecological research.

`neotoma` is part of the [rOpenSci](http://ropensci.org) project and is also hosted on [Figshare](http://dx.doi.org/10.6084/m9.figshare.677131).  The `neotoma` is also available on [CRAN](http://cran.r-project.org/web/packages/neotoma/index.html) as of May 3, 2015.

For more information on the package please refer to: 

Goring, S., Dawson, A., Simpson, G. L., Ram, K., Graham, R. W., Grimm, E. C., & Williams, J. W.. (2015). neotoma: A Programmatic Interface to the Neotoma Paleoecological Database. *Open Quaternary*, 1(1), Art. 2. DOI: [10.5334/oq.ab](http://doi.org/10.5334/oq.ab)

### Development
+ [Simon Goring](http://downwithtime.wordpress.com) - University of Wisconsin-Madison, Department of Geography

### Contributors
+ [Gavin Simpson](http://www.fromthebottomoftheheap.net/) - University of Regina, Department of Biology
+ [Jeremiah Marsicek](http://geoweb.uwyo.edu/ggstudent/jmarsice/Site/Home.html) - University of Wyoming, Department of Geology and Geophysics
+ [Karthik Ram](http://nature.berkeley.edu/~kram/) - University of California - Berkely, Berkeley Institue for Data Science.
+ [Luke Sosalla](https://github.com/sosalla) - University of Wisconsin, Department of Geography

Package functions resolve various Neotoma APIs and re-form the data returned by the Neotoma database into R data objects.  The format of the Neotoma data, and the actual API functions can be accessed on the Neotoma API [website](http://api.neotomadb.org/doc/resources/home).

### Currently implemented in `neotoma`

More functions are available through the package help.  These represent the core functions:

+ `get_site` - obtain information on sites in the Neotoma dataset (which may contain multiple datasets). [API](http://api.neotomadb.org/doc/resources/sites)
+ `get_dataset` - obtain dataset metadata from Neotoma. [API](http://api.neotomadb.org/doc/resources/datasets)
+ `get_download` - obtain full datasets (pollen or mammal) from Neotoma. [API](http://api.neotomadb.org/doc/resources/downloads)
+ `compile_list` - using established pollen-related taxonomies from the literature, take the published taxon list and standardize it to allow cross site analysis.
+ `get_contact` - find contact information for data contributors to Neotoma. [API](http://api.neotomadb.org/doc/resources/contacts)
+ `get_publication` - obtain publication information from Neotoma. [API](http://api.neotomadb.org/doc/resources/publications)
+ `get_table` - return matrices corresponding to one of the Neotoma database tables. [tables](http://api.neotomadb.org/doc/resources/dbtables)
+ `get_taxa` - Get taxon information from Neotoma. [API](http://api.neotomadb.org/doc/resources/taxa)
+ `get_chroncontrol` - Get chronological information used to build the age-depth model for the record. [API](http://api.neotomadb.org/doc/resources/chroncontrol)

### Recent Changes
+ 1.4.0: Added `plot` method for datasets, sites & downloads.  Fixed a bug with records missing chronologies.
+ 1.3.3: Modified `get_download` to ensure the default chronolgy is always associated with `sample.meta`.  Bugfix for issue [#187](https://github.com/ropensci/neotoma/issues/187).  Empty `dataset_list`s now return a NULL value and a warning.
+ 1.3.2: Bugfix for `get_download`, assignment to the `dataset.id` was incorrectly placed causing the columns to be out of place relative to the data (see issue [#197](https://github.com/ropensci/neotoma/issues/197)).
+ 1.3.1: Bugfix for `write_agefile` and the addition of a `browse` method.  See [this gist](https://gist.github.com/SimonGoring/877dd71cc3ad6bf8531e) for an example using `neotoma` and Bacon (Blaauw and Christen, 2011 [[pdf](http://ba.stat.cmu.edu/journal/2011/vol06/issue03/christen.pdf)]).
+ 1.3.0: Improved functionality of `get_chroncontrol` to allow it to deal with empty tables and work directly with `download` and `download_list` objects.
+ 1.2-1: Fix for empty `chroncontrol` tables [issue 178](https://github.com/ropensci/neotoma/issues/178).  Added new methods for `get_chroncontrols` to streamline access from `download` and `download_list` objects.
+ 1.2-0: Added a set of new datasettypes to reflect the increasing breadth of the Neotoma Database.
+ 1.2-0: Fixed bugs relates to the gpid parameter in `get_dataset`.

### Install `neotoma`

+ CRAN:
```r
install.packages('neotoma')
```

+ Development version from GitHub:
```r
install.packages("devtools")
library(devtools)
install_github("ropensci/neotoma")
library(neotoma)
```

### A few examples

#### Find the distribution of sites with Mammoth fossils in Neotoma

```r
#  Example requires the mapdata package:
library('mapdata')

#  You may use either '%' or '*' as wildcards for search terms:
test <- get_dataset(taxonname='Mammuthus*')

The API call was successful, you have returned  3273 records.

site.locs <- get_site(test)

# A crude way of making the oceans blue.
plot(1, type = 'n',
     xlim=range(site.locs$long)+c(-10, 10),
     ylim=range(site.locs$lat)+c(-10, 10),
     xlab='Longitude', ylab = 'Latitude')
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "lightblue")
map('world',
    interior=TRUE,
    fill=TRUE,
    col='gray',
    xlim=range(site.locs$long)+c(-10, 10),
    ylim=range(site.locs$lat)+c(-10, 10),
    add=TRUE)

points(site.locs$long, site.locs$lat, pch=19, cex=0.5, col='red')

```
![thing](inst/img/mammothsites.png)

### Plot the proportion of publications per year for datasets in Neotoma

```R
# Requires ggplot2
library('ggplot2')
library('plyr')
pubs <- get_publication()

pub.years <- ldply(pubs, "[[", "meta")

ggplot(data=pub.years, aes(x = year)) +
     stat_bin(aes(y=..density..*100, position='dodge'), binwidth=1) +
     theme_bw() +
     ylab('Percent of Publications') +
     xlab('Year of Publication') +
     scale_y_continuous(expand = c(0, 0.1)) +
     scale_x_continuous(breaks = seq(min(pub.years$year, na.rm=TRUE), 2014, by=20))

```

![thing](inst/img/histogramplot.png)

### Cumulative plot of record uploads to Neotoma since 1998.
Found on [this gist](https://gist.github.com/SimonGoring/718a654f304f2d16ce4b)

### 
---

[![](http://ropensci.org/public_images/github_footer.png)](http://ropensci.org)
