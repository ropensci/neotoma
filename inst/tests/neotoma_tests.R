context('get_contacts work as expected')

test_that('get_contacts accepts and returns the right data types', {
          expect_error(get_contacts(contactid='aaa'))
          expect_error(get_contacts(contactname=12))
          expect_error(get_contacts(contactstatus=1))
          expect_error(get_contacts(familyname=12))
          expect_message(get_contacts(contactid=1), 'The API call')
          expect_message(get_contacts(familyname='Smith'), 'The API call')
          expect_message(get_contacts(contactname='*Smith*'), 'The API call')
          })


context('get_downloads work as expected')

test_that('get_download accepts numeric values and returns values as expected',{
  expect_error(get_download('a'))
  expect_error(get_download(factor('a')))
  expect_error(get_download(c('a', 'b')))
  expect_message(get_download(1), 'API call was successful')
  expect_that(length(get_download(1)) == 6, is_true())
  expect_that(length(get_download(c(1,2))) == 2, is_true())
})

