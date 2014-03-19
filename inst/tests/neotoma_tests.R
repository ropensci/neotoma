context('get_contacts work as expected')

test_that('get_contacts accepts and returns the right data types', 
          {
            expect_error(get_contacts(contactid='aaa'))
            expect_error(get_contacts(contactname=12))
            expect_error(get_contacts(contactstatus=1))
            expect_error(get_contacts(familyname=12))
            expect_message(get_contacts(contactid=1), 'The API call')
            expect_message(get_contacts(familyname='Smith'), 'The API call')
            expect_message(get_contacts(contactname='*Smith*'), 'The API call')
          })

#-----------------------------------------------------

context('get_downloads works as expected')

test_that('get_download accepts numeric values and returns values as expected',
          {
            expect_error(get_download('a'))
            expect_error(get_download(factor('a')))
            expect_error(get_download(c('a', 'b')))
            expect_message(get_download(1), 'API call was successful')
            expect_that(length(get_download(1)) == 6, is_true())
            expect_that(length(get_download(c(1,2))) == 2, is_true())
            expect_is(get_download(1, verbose=FALSE), 'list')
          })


#-----------------------------------------------------

context('get_datasets works as expected')

test_that('is get_datasets working?', 
          {
            expect_error(get_datasets(siteid='a'))
            expect_error(get_datasets(datasettype=10))
            expect_error(get_datasets(datasettype='banana'))
            expect_error(get_datasets(piid='a'))
            expect_error(get_datasets(altmin='low'))
            expect_error(get_datasets(altmax='low'))
            expect_error(get_datasets(loc=10))
            expect_error(get_datasets(loc=c('a', 'b', 'c')))
            expect_error(get_datasets(gpid=10))
            expect_error(get_datasets(taxonids='Pine'))
            expect_error(get_datasets(taxonname=10))
            expect_error(get_datasets(ageold='min'))
            expect_error(get_datasets(ageyoung='max'))
            expect_error(get_datasets(ageof=10))
            expect_error(get_datasets(ageof='taxon'))
            expect_error(get_datasets(subdate=10))
            expect_is(get_datasets(siteid=1), 'list')
})

#-----------------------------------------------------
