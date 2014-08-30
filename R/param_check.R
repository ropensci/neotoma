#' @title Internal function to check passed parameters.
#'
#' @description Functions \code{\link{get_site}}, \code{\link{get_dataset}} and others pass parameters to \code{param_check}, which tells them if there's a problem.
#' @param cl Contact ID is a numerical value associated with the Neotoma
#'    Contact table's numerical Contact ID.
#' @author Simon J. Goring \email{simon.j.goring@@gmail.com}
#' @return A list with two components:
#'
#'  \item{flag}{Returns a 0 if everything's fine, a 1 if there's a problem.}
#'  \item{message}{A list of error messages.}
#'
#' @references
#' Neotoma Project Website: http://www.neotomadb.org
#' API Reference:  http://api.neotomadb.org/doc/resources/contacts
#' @keywords Neotoma Palaeoecology API
#' @export
param_check <- function(cl){
  #  A long list of proper parameter formatting so that it's not stuck in all
  #  the basic functions.

  error <- list(flag = 0,
                message = list())

  if ('ageof' %in% names(cl)){
    if (!cl$ageof %in% c('sample', 'taxon', 'dataset')){
      error$flag <- 1
      error$message[[length(error$message) + 1]] <- 'ageof parameter must be one of: sample, taxon or dataset'
    } else {
      if (any(c('taxonid', 'taxonname') %in% names(cl)) & !cl$ageof == 'taxon'){
        error$flag <- 1
        error$message[[length(error$message) + 1]] <- 'When taxonid or taxonname is invoked, ageof must be taxon'
      }
    }
    if (!any(c('ageyoung', 'ageold') %in% names(cl))){
      error$flag <- 1
      error$message[[length(error$message) + 1]] <- paste0('When ageof in invoked you also need to provide ',
                                                           'an age range using ageyounger or ageolder.')
    }
  }

  # Parameter check on ages:
  if (all(c('ageold', 'ageyoung') %in% names(cl))){
    # If the user defines a minimum and maximum age, make sure that the
    # minimum is smaller than the max.
    if (cl$ageyoung > cl$ageold){
      error$flag <- 1
      error$message[[length(error$message) + 1]] <- paste0('ageyoung must be smaller than ageold, ',
                                                           'values were switched in call.')
    }
  }

  # Parameter check on altitudes.  This gets reused, we could turn it into a
  # higher level function to save reading lame code:
  if (all(c('altmin', 'altmax') %in% names(cl))){
    # If the user defines a minimum and maximum altitude, make sure that the
    # minimum is smaller than the max.
    if (cl$altmin > cl$altmax){
      error$flag = 1
      error$message[[length(error$message) + 1]] <- 'altmin must be smaller than altmax.'
    }
  }

  # Parameter check on contactid:
  if ('contactid' %in% names(cl)){
    if (!is.numeric(cl$contactid)){
      error$flag = 1
      error$message[[length(error$message) + 1]] <- 'The contactid must be numeric.'
    }
  }

  # Parameter check on contactname:
  if ('contactname' %in% names(cl)){
    if (!is.character(cl$contactname)){
      error$flag = 1
      error$message[[length(error$message) + 1]] <- 'The contactname must be a character string.'
    }
  }

  #  Parameter check on contactstatus:
  if ('contactstatus' %in% names(cl)){
    if (!is.character(cl$contactstatus)){
      error$flag = 1
      error$message[[length(error$message) + 1]] <- 'The contactstatus must be a character string.'
    } else {
      if (!cl$contactstatus %in% c('active', 'deceased', 'defunct',
                                   'extant', 'inactive', 'retired', 'unknown')){
        error$flag = 1
        error$message[[length(error$message) + 1]] = paste0('status must be an accepted term.  ',
                                                            'Use get_table(\'ContactStatuses\')')
      }
    }
  }

  # Parameter check on familyname:
  if ('familyname' %in% names(cl)){
    if (!is.character(cl$familyname)){
      error$flag = 1
      error$message[[length(error$message) + 1]] <- 'The familyname must be a character string.'
    }
  }

  #  Test the geographic identification against the Geopolitical name table.
  if ('gpid' %in% names(cl)){
    if (is.character(cl$gpid)){
      data(gp.table)
      gprow <- match(x=gpid, table=gp.table$GeoPoliticalName)
      if (is.na(gprow)){
        error$flag <- 1
        error$message[[length(error$message) + 1]] <- 'Cannot find a match for the gpid provided.'
      }
      gpid <- gp.table$GeoPoliticalID[gprow]
    } else {
      if (!is.numeric(cl$gpid)){
        error$flag <- 1
        error$message[[length(error$message) + 1]] <- 'The gpid must be either a character string or an integer.'
      }
    }
  }

  # Parameter check on 'loc', ought to be a comma separated list of
  # lonW, latS, lonE, latN when it is passed out, but it's probably
  # better to pass in a vector.  Although it might be better to associate
  # it with a spatial object existing in R like an extent or bbox.
  if ('loc' %in% names(cl)){
    cl$loc <- eval(cl$loc)

    if (class(cl$loc) == 'numeric' & length(cl$loc == 4)){

      # The latitudes must be from -90 to 90
      # The longitudes must be from -180 to 180
      if (all(findInterval(cl$loc[c(2,4)], c(-90, 90)) == 1) &
            all(findInterval(cl$loc[c(1,3)], c(-180, 180)) == 1)){
        cl$loc <- paste(cl$loc, collapse = ',')
      } else {
        error$flag <- 1
        error$message[[length(error$message) + 1]] <- paste0('loc must be in the form c(lonW, latS, lonE, latN).',
                    '\nLongitudes from -180 to 180, latitudes from -90 to 90.')
      }
    } else {
      error$flag <- 1
      error$message[[length(error$message) + 1]] <- 'The loc must be a numeric vector: lonW, latS, lonE, latN.\n'
    }
  }

  #  Test the PI identification.
  if ('piid' %in% names(cl)){
    # piid must be the numeric PI id number in the Neotoma database.
    if (!is.numeric(cl$piid)){
      error$flag = 1
      error$message[[length(error$message) + 1]] = 'piid must be a numeric value.'
    }
  }

  # Parameter check on siteid:
  if ('siteid' %in% names(cl)){
    if (!is.numeric(cl$siteid)){
      error$flag <- 1
      error$message[[length(error$message) + 1]] <- 'siteid must be numeric.'
    }
  }

  if ('taxonname' %in% names(cl)){
    if (!class(cl$taxonname) == 'character'){
      error$flag <- 1
      error$message[[length(error$message) + 1]] <- 'The taxonname must be a character.'
    }
  }


  error

}
