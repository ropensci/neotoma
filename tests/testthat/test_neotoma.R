#  Tests for the neotoma package.  Mostly validating that changes to the functions
#  do not break the requirements for data formatting.

library("testthat")
library("neotoma")
library("RJSONIO")

# context('The API itself is working properly')
# test_that('The API is returning data as expected from its documentation',
#       {
#           aa <- fromJSON('http://api.neotomadb.org/v1/data/datasets?siteid=1')
#           bb <- fromJSON('http://api.neotomadb.org/v1/data/datasets?gpid=756')
#           expect_is(aa, 'list')
#           expect_is(bb, 'list')
#           expect_equal(length(aa), 2)
#           expect_equal(aa[[1]], 1)
#           expect_more_than(length(aa[[2]]), 0)
#           expect_equal(fromJSON('http://api.neotomadb.org/v1/data/datasets?banana')[[1]], 0)
#           expect_equal(bb[[1]], 1)
#           expect_more_than(length(bb[[2]]), 0)
#       })
# 
# context('get_site works as expected')
# test_that('get_site accepts and returns the right data types',
#       {
#           expect_that('site' %in% class(get_site()), is_true())
#           expect_that('site' %in% class(get_site(get_download(1))),
#                       shows_message('API call was'))
#       })
# 
# 
# ## Turning just this and only the first expect_error is enough to throw
# ## an error on Travis:
# context('get_contact work as expected')
# test_that('get_contact accepts and returns the right data types',
#       {
#           expect_error(get_contact(contactid='aaa'))
#           expect_error(get_contact(contactname=12))
#           expect_error(get_contact(contactstatus=1))
#           expect_error(get_contact(familyname=12))
#           expect_message(get_contact(contactid=1), 'The API call')
#           expect_message(get_contact(familyname='Smith'), 'The API call')
#           expect_message(get_contact(contactname='*Smith*'), 'The API call')
#      })
# 
# #-----------------------------------------------------
# 
# ## context('get_site works as expected')
# ## test_that('get_site accepts and returns the right data types',
# ## {
# ##   expect_that('site' %in% class(get_site()), is_true())
# ##   expect_that('site' %in% class(get_site(get_download(1))),
# ##   expect_is(get_site(gpid='Canada'), 'site')
# ##               shows_message('API call was'))
# ## })
# 
# #-----------------------------------------------------
# 
# context('get_downloads works as expected')
# 
# test_that('get_download accepts numeric values and returns values as expected',
# {
#   expect_error(get_download('a'))
#   expect_error(get_download(factor('a')))
#   expect_error(get_download(c('a', 'b')))
#   expect_message(get_download(1), 'API call was successful')
#   expect_that(length(get_download(1)) == 1, is_true())
#   expect_that(length(get_download(c(1,2))) == 2, is_true())
#   expect_is(get_download(1, verbose=FALSE), 'download_list')
# })
# 
# #-----------------------------------------------------
# 
# context('get_dataset works as expected')
# 
# test_that('is get_dataset working?',
# {
#   expect_error(get_dataset(x='a'))
#   expect_error(get_dataset(datasettype=10))
#   expect_error(get_dataset(datasettype='banana'))
#   expect_error(get_dataset(piid='a'))
#   expect_error(get_dataset(altmin='low'))
#   expect_error(get_dataset(altmax='low'))
#   expect_error(get_dataset(loc=10))
#   expect_error(get_dataset(loc=c('a', 'b', 'c')))
#   expect_error(get_dataset(taxonids='Pine'))
#   expect_error(get_dataset(taxonname=10))
#   expect_error(get_dataset(ageold='min'))
#   expect_error(get_dataset(ageyoung='max'))
#   expect_error(get_dataset(ageof=10))
#   expect_error(get_dataset(ageof='taxon'))
#   expect_error(get_dataset(subdate=10))
#   expect_is(get_dataset(gpid=10), 'dataset_list')
#   expect_is(get_dataset(x = 1), 'dataset_list')
#   expect_is(get_dataset(x = 1)[[1]], 'dataset')
#   expect_is(get_dataset(gpid='Canada'), 'dataset_list')
# })
# 
# #-----------------------------------------------------
# 
# context('Crossing sites, datasets and downloads, using the API:')
# test_that('Crossing APIs',
# {
#   expect_is(get_dataset(get_download(100)), 'dataset_list')            # test download_list
#   expect_is(get_dataset(get_download(100)[[1]]), 'dataset_list')       # test download
#   expect_is(get_dataset(get_site(sitename='Marion%')), 'dataset_list') # test site
#   expect_is(get_download(x=c(1642, 1705, 1772)), 'download_list') # test site
#   expect_is(get_site(get_download(100)), 'site')                       # test download_list
#   expect_is(get_site(get_download(100)[[1]]), 'site')                  # test download
#   expect_is(get_site(get_dataset(x=100)), 'site')                      # test dataset_list
#   expect_is(get_site(get_dataset(x=100)[[1]]), 'site')                 # test dataset
# })
# 
# #-----------------------------------------------------
# 
# context('Compiling objects and returning what is expected:')
# test_that('Compiling',
# {
#   expect_is(compile_downloads(get_download(100:103)), 'data.frame')
#   expect_is(compile_downloads(get_download(4559:4564)), 'data.frame')
#   expect_is(compile_taxa(get_download(100), 'P25'), 'download_list')
#   expect_is(compile_taxa(get_download(100)[[1]], 'P25'), 'download')
# })
# 
# #-----------------------------------------------------
# 
# context('Test new chroncontrol methods and fixes')
# test_that('Compiling',
# {
#   expect_is(get_chroncontrol(get_download(get_dataset(datasettype='pollen', ageold = 12000,ageyoung=-100,altmin = 101, altmax = 103))), 'data.frame')
#   expect_is(get_chroncontrol(1392), 'list')                          # test empty table
#   expect_named(get_chroncontrol(1392), c('chron.control', 'meta))    # test empty table
#   expect_is(get_chroncontrol(1376), 'list')                          # test partial table
#   expect_named(get_chroncontrol(1376), c('chron.control', 'meta))    # test partial table
#   expect_is(get_chroncontrol(1000), 'list')                          # test full table
#   expect_named(get_chroncontrol(1000), c('chron.control', 'meta))    # test full table
#
# })
# 
