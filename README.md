neotoma
========

[![Build Status](https://api.travis-ci.org/ropensci/neotoma.png)](https://travis-ci.org/ropensci/neotoma)
[![Build status](https://ci.appveyor.com/api/projects/status/t2xyqbs0d8h998cb/branch/master)](https://ci.appveyor.com/project/sckott/neotoma/branch/master)


*PLEASE NOTE*: The most recent change has standardized all function names to singular.    This may break functionality.

The `neotoma` package is a programmatic R interface to the [Neotoma Paleoecological Database](http://www.neotomadb.org/). The package is intended to both allow users to search for sites and to download data for use in analyical workflows of paleoecological research.

`neotoma` is part of the [rOpenSci](http://ropensci.org) project and is also hosted on [Figshare](http://dx.doi.org/10.6084/m9.figshare.677131)

### Development
+ [Simon Goring](http://downwithtime.wordpress.com) - University of Wisconsin-Madison, Department of Geography

### Contributors
+ [Gavin Simpson](http://www.fromthebottomoftheheap.net/) - University of Regina, Department of Biology
+ [Jeremiah Marsicek](http://geoweb.uwyo.edu/ggstudent/jmarsice/Site/Home.html) - University of Wyoming, Department of Geology and Geophysics
+ [Karthik Ram](http://nature.berkeley.edu/~kram/) - University of California - Berkely, College of Natural Resources

Package functions resolve various Neotoma APIs and re-form the data returned by the Neotoma database into R data objects.  The format of the Neotoma data, and the actual API functions can be accessed on the Neotoma API [website](http://api.neotomadb.org/doc/resources/home).

### Currently implemented in `neotoma`
+ compile_list - using established pollen-related taxonomies from the literature, take the published taxon list and standardize it to allow cross site analysis.
+ get_contact - find contact information for data contributors to Neotoma. [API](http://api.neotomadb.org/doc/resources/contacts)
+ get_dataset - obtain dataset metadata from Neotoma. [API](http://api.neotomadb.org/doc/resources/datasets)
+ get_download - obtain full datasets (pollen or mammal) from Neotoma. [API](http://api.neotomadb.org/doc/resources/downloads)
+ get_publication - obtain publication information from Neotoma. [API](http://api.neotomadb.org/doc/resources/publications)
+ get_site - obtain information on sites in the Neotoma dataset (which may contain multiple datasets). [API](http://api.neotomadb.org/doc/resources/sites)
+ get_table - return matrices corresponding to one of the Neotoma database tables. [tables](http://api.neotomadb.org/doc/resources/dbtables)
+ get_taxa - Get taxon information from Neotoma. [API](http://api.neotomadb.org/doc/resources/taxa)
+ get_chroncontrol - Get chronological information used to build the age-depth model for the record. [API](http://api.neotomadb.org/doc/resources/chroncontrol)

### Recent Changes
+ Major changes to `dataset` and `site` objects, as well as the `metadata` for `downloads`.  This involved cleaning up the column names.
+ Added classes to `download`, `dataset` and `site` objects.

### Coming soon
+ A broader set of taxonomies for use with datasets, including geography based taxonomies.

### Install `neotoma`

+ Development version from GitHub:

```coffee
install.packages("devtools")
require(devtools)
install_github("neotoma", "ropensci")
require(neotoma)
```

### A few examples

#### Find the distribution of sites with Mammoth fossils in Neotoma

```coffee
#  Example requires the mapdata package:
library('mapdata')

#  You may use either '%' or '*' as wildcards for search terms:
test <- get_dataset(taxonname='Mammuthus*')

The API call was successful, you have returned  3273 records.

site.locs <- get_site(dataset = test)

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

ggplot(data=pub.years, aes(x = Year)) +
     stat_bin(aes(y=..density..*100, position='dodge'), binwidth=1) +
     theme_bw() +
     ylab('Percent of Publications') +
     xlab('Year of Publication') +
     scale_y_continuous(expand = c(0, 0.1)) +
     scale_x_continuous(breaks = seq(min(pub.years$Year, na.rm=TRUE), 2014, by=20))

```

![thing](inst/img/histogramplot.png)

---

[![](http://ropensci.org/public_images/github_footer.png)](http://ropensci.org)
