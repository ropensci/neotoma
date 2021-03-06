 #  # Tests for the neotoma package.  Mostly validating that changes to the functions
 #  ## do not break the requirements for data formatting.
 # 
 #   library("testthat")
 #   library("neotoma")
 #   library("analogue")
 #   library("dplyr")
 #   library("purrr")
 # 
 #  context('Get closest is working')
 #  test_that('the right number of samples are returned',
 #            {
 #              cl_smp <- get_closest(x = c(-89.4012, 43.0731), n = 5, buffer = 50000, datasettype = "pollen")
 #              testthat::expect_is(cl_smp, 'dataset_list')
 #              testthat::expect_equal(length(cl_smp), 5)
 #              testthat::expect_identical(unique(sapply(cl_smp, function(x) x$dataset.meta$dataset.type)), "pollen")
 #            })
 # 
 #  context('Testing print methods')
 # 
 #  test_that('Printing objects works',
 #            {
 #              sts <- get_site(gpid = 'New Brunswick')
 #              dst <- get_dataset(sts)
 #              dls <- get_download(dst, verbose = FALSE)
 #              gcr <- get_geochron(dst)
 #              testthat::expect_output(print(sts), regexp=".*site object containing.*")
 #              testthat::expect_output(print(dst), regexp="A dataset_list containing.*")
 #              testthat::expect_output(print(dls), regexp="A download_list containing.*")
 #              testthat::expect_output(print(gcr), regexp="A geochronology_list containing.*")
 #            })
 # 
 #  context('The API itself is working properly')
 #  test_that('The API is returning data as expected from its documentation',
 #        {
 #            aa <- jsonlite::fromJSON(httr::content(httr::GET('http://wnapi.neotomadb.org/v1/data/datasets?siteid=1'), as = 'text'))
 #            bb <- jsonlite::fromJSON(httr::content(httr::GET('http://wnapi.neotomadb.org/v1/data/datasets?gpid=756'), as = 'text'))
 #            expect_is(aa, 'list')
 #            expect_is(bb, 'list')
 #            expect_equal(length(aa), 2)
 #            expect_equal(aa[[1]], 1)
 #            expect_more_than(length(aa[[2]]), 0)
 #            expect_equal(jsonlite::fromJSON(httr::content(httr::GET('http://wnapi.neotomadb.org/v1/data/datasets?banana'), as = 'text'))[[1]], 0)
 #            expect_equal(bb[[1]], 1)
 #            expect_more_than(length(bb[[2]]), 0)
 #        })
 # 
 #  context('get_site works as expected')
 #  test_that('get_site accepts and returns the right data types',
 #        {
 #            expect_that('site' %in% class(get_site()), is_true())
 #            expect_that('site' %in% class(get_site(get_download(1))),
 #                        shows_message('API call was'))
 #            expect_that('site' %in% class(get_site()), is_true())
 #            expect_that('site' %in% class(get_site(get_download(1))),
 #                      shows_message('API call was'))
 #            expect_is(get_site(gpid = 'Canada'), 'site')
 #            expect_is(get_site(100:130), 'site')
 #            expect_that(nrow(get_site(200:220)) == 19, is_true())
 #        })
 # 
 # 
 #  ## Turning just this and only the first expect_error is enough to throw
 #  ## an error on Travis:
 #  context('get_contact work as expected')
 #  test_that('get_contact accepts and returns the right data types',
 #        {
 #            expect_error(get_contact(contactid = 'aaa'))
 #            expect_error(get_contact(contactname = 12))
 #            expect_error(get_contact(contactstatus = 1))
 #            expect_error(get_contact(familyname = 12))
 #            expect_message(get_contact(contactid = 1), 'The API call')
 #            expect_message(get_contact(familyname = 'Smith'), 'The API call')
 #            expect_message(get_contact(contactname = '*Smith*'), 'The API call')
 #       })
 # 
 #  #-----------------------------------------------------
 # 
 #   context('get_publication')
 #   test_that('get_publication accepts and returns the right data types',
 #   {
 #     expect_is(get_publication(10), 'list')
 #   })
 # 
 #  #-----------------------------------------------------
 # 
 #  context('get_downloads works as expected')
 # 
 #  test_that('get_download accepts numeric values and returns values as expected',
 #  {
 #    expect_error(get_download('a', verbose = FALSE))
 #    expect_error(get_download(factor('a', verbose = FALSE)))
 #    expect_error(get_download(c('a', 'b', verbose = FALSE)))
 #    expect_message(get_download(1), 'API call was successful')
 #    expect_that(length(get_download(1, verbose = FALSE)) == 1, is_true())
 #    expect_that(length(get_download(c(1,2), verbose = FALSE)) == 2, is_true())
 #    expect_is(get_download(1, verbose = FALSE), 'download_list')
 #    expect_true(is.numeric(get_download(3031, verbose = FALSE)[[1]]$sample.meta$chronology.id[1]))
 #    expect_true(is.numeric(get_download(3031, verbose = FALSE)[[1]]$sample.meta$dataset.id[1]))
 #    expect_true(is.numeric(get_download(6000, verbose = FALSE)[[1]]$sample.meta$chronology.id[1]))
 #    expect_true(is.numeric(get_download(6000, verbose = FALSE)[[1]]$sample.meta$dataset.id[1]))
 #    expect_true(is.data.frame(get_download(6283, verbose = FALSE)[[1]]$taxon.list))
 #    expect_equal(get_download(17387)$`17387`$sample.meta$chronology.id[1],25001)
 #    # A set that failed because of weird chronology tables:
 #    expect_is(get_download(1776, verbose = FALSE), 'download_list')
 #    expect_is(get_download(13046, verbose = FALSE), 'download_list')
 #    expect_is(get_download(15108, verbose = FALSE), 'download_list')
 #    expect_is(get_download(15080, verbose = FALSE), 'download_list')
 #    expect_is(get_download(14196, verbose = FALSE), 'download_list') # weird chronology situation.
 # 
 #  })
 # 
 #  #-----------------------------------------------------
 # 
 #  context('get_dataset works as expected')
 # 
 #  test_that('is get_dataset working?',
 #  {
 #    expect_error(get_dataset(x='a'))
 #    expect_error(get_dataset(datasettype=10))
 #    expect_error(get_dataset(datasettype='banana'))
 #    expect_error(get_dataset(piid='a'))
 #    expect_error(get_dataset(altmin='low'))
 #    expect_error(get_dataset(altmax='low'))
 #    expect_error(get_dataset(loc=10))
 #    expect_error(get_dataset(loc=c('a', 'b', 'c')))
 #    expect_error(get_dataset(taxonids='Pine'))
 #    expect_error(get_dataset(taxonname=10))
 #    expect_error(get_dataset(ageold='min'))
 #    expect_error(get_dataset(ageyoung='max'))
 #    expect_error(get_dataset(ageof=10))
 #    expect_error(get_dataset(ageof='taxon'))
 #    expect_error(get_dataset(subdate=10))
 #    expect_is(get_dataset(gpid=756), 'dataset_list')
 #    expect_is(get_dataset(x = 1), 'dataset_list')
 #    expect_is(get_dataset(x = 1)[[1]], 'dataset')
 #    expect_is(get_dataset(gpid='Canada'), 'dataset_list')
 #    expect_is(get_dataset(100:102), 'dataset_list')
 #    expect_error(get_dataset(datasettype='pollen', gpid=c("Wisconsin", "Michigan", "Minnesota", "Barf Town", "Poop Ville")))
 #    expect_is(get_dataset(get_site(sitename = "Lac à Sam%")), "dataset_list")
 #    expect_is(get_dataset(gpid=c(756, 2845, 5242), datasettype = "pollen"), "dataset_list") # Canada, Mexico, Spain
 #  })
 # 
 #  #-----------------------------------------------------
 # 
 #  context('Crossing sites, datasets and downloads, using the API:')
 #  test_that('Crossing APIs',
 #  {
 #    expect_is(get_dataset(get_download(100, verbose = FALSE)), 'dataset_list')            # test download_list
 #    expect_is(get_dataset(get_download(100, verbose = FALSE)[[1]]), 'dataset_list')       # test download
 #    expect_is(get_dataset(get_site(sitename='Marion%')), 'dataset_list') # test site
 #    expect_is(get_download(x = c(1642, 1705, 1772)), 'download_list')    # test site
 #    expect_is(get_site(get_download(100, verbose = FALSE)), 'site')                       # test download_list
 #    expect_is(get_site(get_download(100, verbose = FALSE)[[1]]), 'site')                  # test download
 #    expect_is(get_site(get_dataset(x=100)), 'site')                      # test dataset_list
 #    expect_is(get_site(get_dataset(x=100)[[1]]), 'site')                 # test dataset
 #  })
 # 
 #  #-----------------------------------------------------
 # 
 #  context('Compiling objects and returning what is expected:')
 #  test_that('Compiling',
 #  {
 #    expect_is(compile_downloads(get_download(100:103, verbose = FALSE)), 'data.frame')
 #    expect_is(compile_downloads(get_download(4559:4564, verbose = FALSE)), 'data.frame')
 #    expect_is(compile_taxa(get_download(100, verbose = FALSE), 'P25'), 'download_list')
 #    expect_is(compile_taxa(get_download(100, verbose = FALSE)[[1]], 'P25'), 'download')
 #    expect_equal(nrow(compile_downloads(get_download(4565, verbose = FALSE))), 1)
 # 
 #  })
 # 
 #  #-----------------------------------------------------
 # 
 #  context('Test new chroncontrol methods and fixes')
 #  test_that('Compiling',
 #  {
 #    expect_is(get_chroncontrol(get_download(get_dataset(datasettype='pollen', ageold = 12000,ageyoung=-100,altmin = 101, altmax = 103))), 'list')
 #    expect_warning(get_chroncontrol(get_download(1176)))      # test missing chronID table.
 #    expect_named(get_chroncontrol(get_download(1176))[[1]], c('chron.control', 'meta', 'access.date', 'parent'))    # test empty table
 #    expect_named(get_chroncontrol(1392), c('chron.control', 'meta', 'access.date', 'parent'))    # test empty table
 #    expect_is(get_chroncontrol(1376), 'list')                          # test partial table
 #    expect_named(get_chroncontrol(1376), c('chron.control', 'meta', 'access.date', 'parent'))    # test partial table
 #    expect_is(get_chroncontrol(1000), 'list')                          # test full table
 #    expect_named(get_chroncontrol(1000), c('chron.control', 'meta', 'access.date', 'parent'))    # test full table
 # 
 #  })
 # 
 #  #-----------------------------------------------------
 # 
 #  context('Test geochron methods')
 #  test_that('Geochron',
 #  {
 #    expect_is(get_geochron(16128), 'geochronologic_list')
 #    expect_silent(get_geochron(16128, verbose = FALSE))
 #    expect_that(length(get_geochron(16225)[[1]][[2]]) > 0, is_true())
 #    expect_is(get_geochron(8444)[[1]], 'geochronologic')
 #    expect_is(get_geochron(c(8444, 8445)), 'geochronologic_list')
 #    expect_error(get_geochron(1001), 'no geochronological')
 #    expect_is(get_geochron(c(1001, 8445)), 'geochronologic_list')
 #    expect_that(length(get_geochron(c(1001,8444))) == 1, is_true())
 #  })
 # 
 # 
 #  #-----------------------------------------------------
 # 
 #   context('Test get_taxon works:')
 #   test_that('Getting Taxa',
 #   {
 #     expect_is(get_taxa(taxonname = "Abies*"), 'data.frame')
 #     expect_is(get_taxa(taxonid = 19), 'data.frame')
 #     expect_error(get_taxa(taxonname = "Abies*", taxonid = 19))
 #   })
 # 
 #   #-----------------------------------------------------
 # 
 #   context('Get the tables:')
 #   test_that('Getting Tables',
 #             {
 #               expect_is(get_table('Taxa'), 'data.frame')
 #               expect_is(get_table('Tephras'), 'data.frame')
 #             })
 # 
 #   #-----------------------------------------------------
 # 
 #  context('Trying to bind:')
 #   test_that('bind',
 #             {
 #   #              gpid_table <- neotoma::get_table("GeoPoliticalUnits") %>%
 #   # dplyr::filter(GeoPoliticalName %in% c('Canada', 'United States', 'Mexico')) %>%
 #   # dplyr::select(GeoPoliticalID) %>%
 #   # unlist %>%
 #   # purrr::map(~ neotoma::get_dataset(gpid = .x, datasettype = 'pollen'))
 #   #              expect_is(bind(bind(gpid_table[[1]], gpid_table[[2]]), gpid_table[[3]]), 'dataset_list')
 # 
 #               expect_error(bind(get_download(1001, verbose = FALSE), get_dataset(1001)), "Objects must be")
 #               expect_is(bind(get_dataset(1001), get_dataset(1001)), 'dataset_list')
 #               expect_is(bind(get_download(1001, verbose = FALSE), get_download(1002, verbose = FALSE)), 'download_list')
 #             })
 # 
 #   #-----------------------------------------------------
 # 
 #   context('Trying to browse:')
 #   test_that('browse',
 #             {
 #               expect_error(browse(), "argument")
 #             })
 # 
 #   #-----------------------------------------------------
 # 
 #   # context('Read Tilia files:')
 #   # test_that('read.tilia',
 #   #           {
 #   #             print(getwd())
 #   #             if (!length(grep('testthat', getwd())) == 0) {
 #   #               fileloc = '../../inst/crystal.tlx'
 #   #             } else {
 #   #               fileloc = 'inst/crystal.tlx'
 #   #             }
 #   #             crystal <- read.tilia(fileloc)
 #   #             expect_is(crystal, "download")
 #   #             expect_is(taxa(crystal), "neo_taxa")
 #   #             expect_true(ncol(taxa(crystal)) == 7)
 #   #             expect_true(nrow(ages(crystal)) == nrow(crystal$sample.meta))
 #   #           })
 # # 
 #   #-----------------------------------------------------
 # 
 #   context('Stratiplot tests')
 #   test_that('Stratiplot.download',
 #             {
 #                testthat::expect_silent(Stratiplot(get_download(15887, verbose = FALSE)))
 #                testthat::expect_is(Stratiplot(get_download(15887, verbose = FALSE)), "trellis")
 #             })
 # 
 #   #-----------------------------------------------------
 # 
 #   context('Surface Sample errors')
 #   test_that('compiling surface samples',
 #             {
 # 
 #                testthat::expect_silent(Stratiplot(get_download(15887, verbose = FALSE)))
 #                testthat::expect_is(Stratiplot(get_download(15887, verbose = FALSE)), "trellis")
 #             })
 # 
 #   #-----------------------------------------------------
 #   
 #   context('Testing plot_leaflet')
 #   test_that('methods work:',
 #             {
 #               testthat::expect_error(neotoma::plot_leaflet(1234))
 #               
 #               aa <- neotoma::get_site(gpid="Alberta")
 #               bb <- neotoma::plot_leaflet(aa)
 #               testthat::expect_is(bb, 'leaflet')
 #               
 #               aa <- neotoma::get_dataset(gpid="New Brunswick", datasettype="pollen")
 # 
 #               testthat::expect_is(neotoma::plot_leaflet(aa), 'leaflet')
 #               testthat::expect_is(neotoma::plot_leaflet(aa[[1]]), 'leaflet')
 #               
 #               testthat::expect_is(neotoma::plot_leaflet(get_download(aa, verbose = FALSE)), 'leaflet')
 #               testthat::expect_is(neotoma::plot_leaflet(get_download(aa[[1]], verbose = FALSE)[[1]]), 'leaflet')
 #               
 #             })