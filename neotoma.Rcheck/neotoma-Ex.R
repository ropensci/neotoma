pkgname <- "neotoma"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
base::assign(".ExTimings", "neotoma-Ex.timings", pos = 'CheckExEnv')
base::cat("name\tuser\tsystem\telapsed\n", file=base::get(".ExTimings", pos = 'CheckExEnv'))
base::assign(".format_ptime",
function(x) {
  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]
  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]
  options(OutDec = '.')
  format(x[1L:3L], digits = 7L)
},
pos = 'CheckExEnv')

### * </HEADER>
library('neotoma')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("Stratiplot.download")
### * Stratiplot.download

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: Stratiplot.download
### Title: Palaeoecological stratigraphic diagrams
### Aliases: Stratiplot.download

### ** Examples

## Not run: 
##D lake_o_dl <- get_download(15925)
##D Stratiplot(lake_o_dl[[1]])
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("Stratiplot.download", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("Stratiplot.download_list")
### * Stratiplot.download_list

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: Stratiplot.download_list
### Title: Palaeoecological stratigraphic diagrams
### Aliases: Stratiplot.download_list

### ** Examples

## Not run: 
##D lake_o_dl <- get_download(15925)
##D # This works:
##D Stratiplot(lake_o_dl)
##D 
##D lakes_o_nw <- get_download(get_site(sitename = "Lake B%"))
##D # This Fails:
##D # Stratiplot(lake_o_nw)
##D 
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("Stratiplot.download_list", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("ages")
### * ages

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: ages
### Title: Access proxy age data
### Aliases: ages ages.download ages.download_list

### ** Examples

## Not run: 
##D ostracodes <- get_dataset(datasettype = 'ostracode')
##D 
##D ostro.dl <- get_download(ostracodes)
##D ostro.ages <- ages(ostro.dl)
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("ages", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("bind")
### * bind

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: bind
### Title: Function to bind objects together into a longer object.
### Aliases: bind
### Keywords: utilities

### ** Examples

## Not run: 
##D #  Search for sites with "Thuja" pollen that are older than 8kyr BP and
##D #  that are on the west coast of North America:
##D t8kyr.poa <- get_dataset(taxonname="Thuja*", 
##D                          loc=c(-150, 20, -100, 60), ageyoung = 8000)
##D t8kyr.canis <- get_dataset(taxonname="Canis*", 
##D                            loc=c(-150, 20, -100, 60), ageyoung = 8000)
##D 
##D t8kyr.co_site <- bind(t8kyr.poa, t8kyr.canis)
##D plot(t8kyr.co_site)
##D 
##D ####
##D # We want to look at four different dataset types across a forest-prairie 
##D # boundary:
##D dataset_types <- c("ostracode surface sample",
##D                    "water chemistry",
##D                    "diatom surface sample",
##D                    "pollen surface sample")
##D 
##D # Run the `get_dataset` function for each of the different dataset types 
##D dataset_lists <- lapply(dataset_types, 
##D                           function(x) { 
##D                             get_dataset(datasettype=x, 
##D                                         loc = c(-100,43,-92,48))
##D                                         })
##D 
##D # Using do.call here to make sure that I don't have to split the list out.
##D new_datasets <- do.call(bind, dataset_lists)
##D 
##D # And voila!
##D plot(new_datasets)
##D 
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("bind", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("browse")
### * browse

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: browse
### Title: Open a browser window to display a Neotoma dataset within the
###   Neotoma Explorer
### Aliases: browse
### Keywords: IO connection

### ** Examples

## Not run: 
##D # Where are the XRF data?
##D 
##D xrf.data <- get_dataset(datasettype='X-ray fluorescence (XRF)')
##D browse(xrf.data)
##D 
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("browse", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("compile_downloads")
### * compile_downloads

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: compile_downloads
### Title: Function to convert multiple downloads into a single large
###   table.
### Aliases: compile_downloads
### Keywords: utilities

### ** Examples

## Not run: 
##D #  Search for sites with "Thuja" pollen that are older than 8kyr BP and
##D #  that are on the west coast of North America:
##D t8kyr.datasets <- get_dataset(taxonname='Thuja*', 
##D                               loc=c(-150, 20, -100, 60), 
##D                               ageyoung = 8000)
##D 
##D #  Returns 3 records (as of 04/04/2013), get dataset for the first record, 
##D #  Gold Lake Bog.
##D thuja.sites <- get_download(t8kyr.datasets)
##D 
##D gold.p25 <- compile_taxa(thuja.sites, 'P25')
##D 
##D all.gold <- compile_downloads(gold.p25)
##D 
##D pollen.sums <- rowSums(all.gold[,11:ncol(all.gold)], na.rm=TRUE)
##D 
##D plot(x = all.gold$age, 
##D      y = all.gold$Cupressaceae.Taxaceae / pollen.sums, 
##D      col = all.gold$site.name,
##D      pch = 19)
##D 
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("compile_downloads", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("compile_taxa")
### * compile_taxa

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: compile_taxa
### Title: Function to convert assemblage taxa to standardized lists.
### Aliases: compile_taxa
### Keywords: utilities

### ** Examples

## Not run: 
##D #  Search for sites with "Thuja" pollen that are older than 8kyr BP and
##D #  that are on the west coast of North America:
##D t8kyr.datasets <- get_dataset(taxonname='Thuja*', loc=c(-150, 20, -100, 60), ageyoung = 8000)
##D 
##D #  Returns 3 records (as of 04/04/2013), get dataset for the first record, Gold Lake Bog.
##D GOLDKBG <- get_download(t8kyr.datasets[[1]])
##D 
##D gold.p25 <- compile_taxa(GOLDKBG, 'P25')
##D 
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("compile_taxa", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("counts")
### * counts

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: counts
### Title: Access proxy count data
### Aliases: counts counts.download counts.download_list

### ** Examples

## Not run: 
##D marion <- get_site('Marion Lake%')
##D louise <- get_site('Louise Pond%')
##D western.sites <- rbind(marion, louise)
##D western.data  <- get_dataset(western.sites)
##D 
##D western.dl <- get_download(western.data)
##D western.cnt <- counts(western.dl)
##D sapply(western.cnt, dim)
##D marion.cnt<- counts(western.dl[[1]])
##D dim(marion.cnt)
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("counts", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("depths")
### * depths

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: depths
### Title: Extracts the depth values from a 'download' object
### Aliases: depths depths.default depths.download depths.download_list
### Keywords: IO connection

### ** Examples

## Not run: 
##D # Provide a vector of depths to generate a new age model:
##D # The dataset id 684 is for Devils Lake, a record published by Louis Maher Jr.
##D 
##D pollen.data <- get_download(684)
##D pollen.chron <- get_chroncontrol(pollen.data)[[1]]
##D 
##D age_sds <- pollen.chron$chron.control$age - focal$chron.control$age.young,
##D get_curves <- ifelse(regexpr("Radiocarbon",
##D                              pollen.chron$chron.control$control.type) > -1, 
##D                      'intcal13', 'normal')
##D 
##D new_chron <- Bchron::Bchronology(ages   = pollen.chron$chron.control$age,
##D                                  ageSds = age_sds
##D                                  positions = pollen.chron$chron.control$depth,
##D                                  calCurves = , 
##D                                  predictPositions = depths(pollen.data))
##D 
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("depths", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("get_chroncontrol")
### * get_chroncontrol

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: get_chroncontrol
### Title: Function to return chronological control tables used to build
###   age models.
### Aliases: get_chroncontrol
### Keywords: IO connection

### ** Examples

## Not run: 
##D #  The point of pulling chronology tables is to re-build or examine the 
##D #  chronological information that was used to build the age-depth model for 
##D #  the core.  You can do this by hand, but the `write_agefile` function works 
##D #  with `download` objects directly.
##D 
##D three_pines <- get_download(get_dataset(get_site("Three Pines Bog"), 
##D                                         datasettype = "pollen"))
##D pines_chron <- get_chroncontrol(three_pines)
##D 
##D # Spline interpolation:
##D model <- smooth.spline(x = pines_chron[[1]]$chron.control$depth,
##D                        y = pines_chron[[1]]$chron.control$age)
##D                        
##D new_ages <- predict(model, x = three_pines[[1]]$sample.meta$depth)
##D 
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("get_chroncontrol", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("get_closest")
### * get_closest

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: get_closest
### Title: Find the closest dataset records to a site, dataset or long/lat
###   pair in Neotoma
### Aliases: get_closest
### Keywords: API Neotoma Palaeoecology

### ** Examples

## Not run: 
##D #  The point of pulling chronology tables is to re-build or examine the chronological 
##D #  information that was used to build the age-depth model for the core.
##D # Find the closest records to Madison, WI:
##D get_closest(x = c(-89.4012, 43.0731), n = 10, buffer = 5, datasettype = "pollen")
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("get_closest", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("get_contact")
### * get_contact

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: get_contact
### Title: Get contact information.
### Aliases: get_contact
### Keywords: IO connection

### ** Examples

## Not run: 
##D #  To find all data contributors who are active:
##D active.cont <- get_contact(contactstatus = 'active')
##D 
##D # To find all data contributors who have the last name "Smith"
##D smith.cont <- get_contact(familyname = 'Smith')
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("get_contact", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("get_dataset")
### * get_dataset

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: get_dataset
### Title: Obtain dataset information from the Neotoma Paleoecological
###   Database or an existing object.
### Aliases: get_dataset
### Keywords: IO connection

### ** Examples

## Not run: 
##D # Search for sites with "Thuja" pollen that are older than 8kyr BP and
##D # that are on the west coast of North America:
##D t8kyr.datasets <- get_dataset(taxonname='Thuja*', 
##D                               loc=c(-150, 20, -100, 60), 
##D                               ageyoung = 8000)
##D 
##D # Search for vertebrate fossils in Canada (gpid: 756) within the last 2kyr.
##D gpids <- get_table(table.name='GeoPoliticalUnits')
##D canID <- gpids[which(gpids$GeoPoliticalName == 'Canada'),1]
##D 
##D v2kyr.datasets <- get_dataset(datasettype='vertebrate fauna', 
##D                               gpid=canID, 
##D                               ageold = 2000)
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("get_dataset", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("get_download")
### * get_download

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: get_download
### Title: Function to return full download records using 'site's,
###   'dataset's, or dataset IDs.
### Aliases: get_download
### Keywords: IO connection

### ** Examples

## Not run: 
##D #  Search for sites with "Pseudotsuga" pollen that are older than 8kyr BP and
##D #  that are roughly within western British Columbia:
##D t8kyr.datasets <- get_dataset(taxonname='*Picea*', loc=c(-90, 41, -89, 44),
##D                               ageold = 20000, ageyoung=10000)
##D 
##D #  Returns 20 records (as of 04/04/2013), get the dataset for all records:
##D pollen.records <- get_download(t8kyr.datasets)
##D 
##D #  Standardize the taxonomies for the different records using the WS64 taxonomy.
##D compiled.sites <- compile_taxa(pollen.records, list.name='WS64')
##D 
##D #  Extract the Pseudotsuga curves for the sites:
##D get.curve <- function(x, taxa) {
##D                if (taxa %in% colnames(x$counts)) {
##D                  count <- x$counts[,taxa]/rowSums(x$counts, na.rm=TRUE)
##D                } else {
##D                  count <- rep(0, nrow(x$count))
##D                }
##D                data.frame(site = x$dataset$site.data$site.name,
##D                age = x$sample.meta$age,
##D                count = count)
##D              }
##D 
##D curves <- do.call(rbind.data.frame,
##D                   lapply(compiled.sites, get.curve, taxa = 'Larix/Pseudotsuga'))
##D 
##D #  For illustration, remove the sites with no Pseudotsuga occurance:
##D curves <- curves[curves$count > 0, ]
##D 
##D smooth.curve <- predict(loess(sqrt(count)~age, data=curves),
##D                         data.frame(age=seq(20000, 0, by = -100)))
##D 
##D plot(sqrt(count) ~ age, data = curves,
##D      ylab = '% Pseudotsuga/Larix', xlab='Calibrated Years BP', pch=19,
##D      col=rgb(0.1, 0.1, 0.1, 0.1), xlim=c(0, 20000))
##D lines(seq(20000, 0, by = -100), smooth.curve, lwd=2, lty=2, col=2)
##D 
##D #  This figure shows us an apparent peak in Larix/Pseudotsuga pollen in the
##D #  early-Holocene that lends support to a warmer, drier early-Holocene in
##D #  western North America.
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("get_download", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("get_geochron")
### * get_geochron

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: get_geochron
### Title: Function to return geochronological data from records.
### Aliases: get_geochron
### Keywords: IO connection

### ** Examples

## Not run: 
##D #  Search for the sites around Marion Lake, BC.  I want to find sites within 
##D #  about 1km.
##D 
##D marion <- get_site(sitename = "Marion Lake*")
##D 
##D marion_close <- get_closest(marion, n = 10, buffer = 1)
##D 
##D #  Returns 116 records (as of 13/07/2015).  These are the pollen records though, 
##D #  we want the sites:
##D geochron.records <- get_geochron(marion_close)
##D 
##D #  We want to extract all the radiocarbon ages from the records:
##D 
##D get_ages <- function(x){
##D   any.ages <- try(x[[2]]$age[x[[2]]$age.type == 'Radiocarbon years BP'])
##D   if(class(any.ages) == 'try-error') output <- NA
##D   if(!class(any.ages) == 'try-error') output <- unlist(any.ages)
##D   output
##D }
##D 
##D radio.chron <- unlist(sapply(geochron.records, get_ages))
##D 
##D hist(radio.chron[radio.chron<40000], breaks=seq(0, 25000, by = 1000),
##D      main = 'Radiocarbon dates for Pseudotsuga records',
##D      xlab = 'Radiocarbon date (14C years before 1950)')
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("get_geochron", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("get_publication")
### * get_publication

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: get_publication
### Title: A function to get publications for sites or datasets in the
###   Neotoma Database using the API.
### Aliases: get_publication
### Keywords: IO connection

### ** Examples

## Not run: 
##D #  To find all publications from 1998:
##D year.cont <- get_publication(year = 1998)
##D 
##D # To find all data contributors who have the last name "Smith"
##D smith.cont <- get_publication(author = 'Smith')
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("get_publication", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("get_site")
### * get_site

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: get_site
### Title: Return Site Information.
### Aliases: get_site
### Keywords: IO connection

### ** Examples

## Not run: 
##D #  What is the distribution of site elevations in Neotoma?
##D all.sites <- get_site()  #takes a bit of time.
##D 
##D plot(density(all.sites$elev, from = 0, na.rm=TRUE),
##D main = 'Altitudinal Distribution of Neotoma Sites', xlab = 'Altitude (m)', log='x')
##D 
##D #  Get site information from a dataset:
##D nw.datasets <- get_dataset(loc = c(-140, 50, -110, 65), 
##D                            datasettype='pollen',
##D                            taxonname='Pinus*')
##D                            
##D nw.sites <- get_site(nw.datasets)
##D 
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("get_site", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("get_table")
### * get_table

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: get_table
### Title: Get Neotoma value tables.
### Aliases: get_table
### Keywords: IO connection

### ** Examples

## Not run: 
##D taxon.table <- get_table('Taxa')
##D 
##D #  Get the frequency of a random taxon in Neotoma.
##D tax_sample <- sample(nrow(taxon.table), 1)
##D cat("The taxon", 
##D     taxon.table$TaxonName[tax_sample], 
##D     "occurs in Neotoma", 
##D     length(get_dataset(taxonname = taxon.table$TaxonName[tax_sample])), 
##D     "times.")
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("get_table", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("get_taxa")
### * get_taxa

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: get_taxa
### Title: Get taxon information from Neotoma.
### Aliases: get_taxa
### Keywords: IO connection

### ** Examples

## Not run: 
##D ## Return all species taxa with "Abies" in name - note wildcard
##D taxa <- get_taxa(taxonname = "Abies*")
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("get_taxa", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("read.tilia")
### * read.tilia

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: read.tilia
### Title: Read proxy data from Tilia TLX files
### Aliases: read.tilia

### ** Examples

## Not run: 
##D   crystal <- read.tilia('crystal.tlx')
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("read.tilia", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("read_bacon")
### * read_bacon

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: read_bacon
### Title: Function to read in defined Bacon outputs.
### Aliases: read_bacon

### ** Examples

## Not run: 
##D # Download the record for Lake O' Pines:
##D lake_o_dl <- get_download(15925)
##D 
##D # This assumes that you have Bacon installed in a folder and have
##D # set it to your working directory.
##D 
##D write_agefile(lake_o_dl[[1]], path = ".", chronology = 1, 
##D               corename = "LAKEPINES", cal.prog = 'Bacon') 
##D 
##D source("Bacon.R") 
##D 
##D # These defaults just help the core run quickly, they're not 
##D # neccesarily good parameters.
##D 
##D Bacon("LAKEPINES", acc.mean = 10, 
##D       thick = 50, depths.file = TRUE, 
##D       suggest = FALSE, ask = FALSE)
##D 
##D lake_o_dl <- read_bacon("LAKEPINES", add = TRUE, 
##D                         download = download, sections = 17)
##D 
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("read_bacon", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("taxa")
### * taxa

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: taxa
### Title: Access proxy taxonomic data
### Aliases: taxa taxa.download taxa.download_list

### ** Examples

## Not run: 
##D ostracodes <- get_dataset(datasettype = 'ostracode')
##D 
##D ostro.dl <- get_download(ostracodes)
##D ostro.taxa <- taxa(ostro.dl)
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("taxa", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("write_agefile")
### * write_agefile

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: write_agefile
### Title: Write age control file to disk formatted for either Bacon or
###   Clam
### Aliases: write_agefile
### Keywords: API Neotoma Palaeoecology

### ** Examples

## Not run: 
##D # Find a particular record:
##D 
##D three_pines <- get_download(get_dataset(get_site("Three Pines Bog"), 
##D                                         datasettype = "pollen"))
##D 
##D # You will need to edit the `path` argument here to point to a directory that 
##D # contains a `Cores` directory.
##D 
##D write_agefile(download = three_pines[[1]], 
##D               path = "./inst", 
##D               corename = "THREEPINES", 
##D               cal.prog = "Bacon")
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("write_agefile", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
