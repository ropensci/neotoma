##' Read proxy data from a Tilia TLX format file.
##'
##' @importFrom xml2 xml_attr read_xml as_list xml_text xml_find_one xml_find_all
##' @title Read proxy data from Tilia TLX files
##'
##' @param file a string representing a Tilia TLX format file.
##' @return Return a `download` object.
##'
##' @author Simon J. Goring \email{simon.j.goring@@gmail.com}
##'
##' @export
##' @rdname read.tilia
##'
##' @examples
##' \dontrun{
##' marion <- read.tilia('crystal.tlx')
##'
##' western.cnt <- counts(western.dl)
##' sapply(western.cnt, dim)
##' marion.cnt<- counts(western.dl[[1]])
##' dim(marion.cnt)
##' }
##' 
`read.tilia` <- function(file) {
    tilia_xml <- xml2::read_xml(file)
    tilia_list <- xml2::as_list(tilia_xml)
    
    find_NA <- function(x,y) {
      wrap <- try(xml2::xml_text(xml2::xml_find_first(x,y)), silent = TRUE)
      if (class(wrap) == 'try-error') wrap <- NA
      return(wrap)
    }
    
    # Making the site data:
    tilia_site <- xml2::xml_find_all(tilia_xml, '//Site')
    
    site <- data.frame(site.id     = NA,
                       site.name   = find_NA(tilia_site,'.//SiteName'),
                       long        = mean(as.numeric(find_NA(tilia_site,'.//LongEast')),
                                          as.numeric(find_NA(tilia_site,'.//LongWest')), na.rm = TRUE),
                       lat         = mean(as.numeric(find_NA(tilia_site,'.//LatNorth')), 
                                          as.numeric(find_NA(tilia_site,'.//LatSouth')), na.rm = TRUE),
                       elev        = as.numeric(find_NA(tilia_site,'.//Altitude')),
                       description = as.character(find_NA(tilia_site,'.//Notes')),
                       long.acc    = abs(as.numeric(find_NA(tilia_site,'.//LongEast')) - 
                                              as.numeric(find_NA(tilia_site,'.//LongWest'))),
                       lat.acc     = abs(as.numeric(find_NA(tilia_site,'.//LatNorth')) - 
                                           as.numeric(find_NA(tilia_site,'.//LatSouth'))),
                       row.names   = find_NA(tilia_site,'.//SiteName'),
                       stringsAsFactors = FALSE)
    
    class(site) <- c('site', 'data.frame')
    
    #######################################################################
    # Pull the contact objects:
    contact_nodes <- xml2::xml_find_all(tilia_xml, '//Contact')
    
    contacts <- do.call(rbind.data.frame,lapply(contact_nodes, function(x) {
      data.frame(contact.name     = find_NA(x,'.//FullContactName'),
                 contact.status   = find_NA(x,'.//Status'),
                 family.name      = find_NA(x,'.//FamilyName'),
                 leading.initials = find_NA(x,'.//LeadingInitials'),
                 given.names      = find_NA(x,'.//GivenNames'),
                 suffix           = find_NA(x,'.//Suffix'),
                 title            = find_NA(x,'.//Title'),
                 phone            = find_NA(x,'.//Phone'),
                 fax              = find_NA(x,'.//Fax'),
                 email            = find_NA(x,'.//Email'),
                 url              = find_NA(x,'.//URL'),
                 address          = find_NA(x,'.//Address'),
                 notes            = find_NA(x,'.//Notes'),
                 contact.id       = find_NA(x,'.//NeotomaContactID'),
                 alias.id         = find_NA(x,'.//NeotomaAliasID'),
                 stringsAsFactors = FALSE)
      }))
    
    contacts <- contacts[!rowSums(is.na(contacts)) == ncol(contacts),]
    class(contacts) <- c('contact', 'data.frame')
    
    #####################################################################
    
    dataset <- list(
      site.data = site,
      dataset.meta = data.frame(dataset.id = NA,
                                dataset.name      = find_NA(tilia_xml, 
                                                            './/Dataset//Name'),
                                collection.type   = find_NA(tilia_xml, 
                                                            './/CollectionUnit//CollectionType'),
                                collection.handle = find_NA(tilia_xml,
                                                            './/CollectionUnit//Handle'),
                                dataset.type      = find_NA(tilia_xml, 
                                                            './/Dataset//DatasetType'),
                                stringsAsFactors = FALSE),
      pi.data      = data.frame(ContactID = contacts$contact.id,
                                ContactName = contacts$contact.name),
      submission   = data.frame(submission.date = NA,
                                submission.type = NA,
                                stringsAsFactors = FALSE),
      access.date = Sys.Date())
    
    class(dataset) <- c('dataset', 'list')
    
    ###############################################
    #
    spreadsheet <- xml2::xml_find_all(tilia_xml,'//SpreadSheet')
    
    empty.frame <- rep(NA, max(sapply(1:length(xml2::xml_children(spreadsheet)), 
                                      function(x)length(xml2::xml_children(xml2::xml_children(spreadsheet)[x]))), 
                               na.rm = TRUE))
    
    # Find the "Data" table:
    # x comes in as (xml_children(spreadsheet)[1])
    
    sample_pull <- function(x) {
      cells <- as.numeric(xml2::xml_attr(xml2::xml_children(x), 'row'))
      
      empty.frame[cells] <- xml2::xml_text(xml2::xml_children(x))
      
      empty.frame
    }
    
    cells <- xml2::xml_attrs(xml2::xml_find_all(spreadsheet, "//cell"))
    rows <- sapply(cells, as.numeric)
    
    columns <- xml2::xml_attrs(xml2::xml_find_all(spreadsheet, "//Col"))
    cols <- sapply(columns, function(x)as.numeric(x["ID"]))
    
    
    
    all_sample <- do.call(rbind.data.frame, 
                    lapply(1:length(xml2::xml_children(spreadsheet)),
                      function(x)as.character(sample_pull(xml2::xml_children(spreadsheet)[x]))))
    
    all_sample <- apply(all_sample, 2, as.character)
    colnames(all_sample) <- NULL
    
    ##############################################################
    #
    #  Build the sample meta:
    
    chrons <- grep("Chron", all_sample[1,])
    chron_nos <- regexpr('[0-9]',all_sample[1,chrons], perl = TRUE)
    unique_chrons <- unique(substr(all_sample[1,chrons], chron_nos, chron_nos))
    
    if (length(unique_chrons) == 0) {
      unique_chrons <- "1"
    }
    
    # Now we have unique chronology numbers.
    
    sample.meta <- list()
    
    for (i in unique_chrons) {
      
      chron_set <- all_sample[,chrons[unique_chrons == i]]
      
      # This doesn't work for mammal assemblage data where there's only one assemblage:
      depths <- !is.na(all_sample[,1])
      
      if (any(depths)) {
        
        if (!is.null(ncol(chron_set))) {
          # This tests whether there are multiple chronologies (there would be multiple columns)
          # Get age elements if there are multiple chronologies:  
          if (length(grep('old', chron_set[1,], ignore.case = TRUE)) > 0) {
            age_older <- suppressWarnings(as.numeric(gsub('\n', '', 
                                                      chron_set[depths,grep('old', chron_set[1,], 
                                                                        ignore.case = TRUE)])))
          } else {
            age_older <- rep(NA, sum(depths)) 
          }
          
          if (length(grep('young', chron_set[1,], ignore.case = TRUE)) > 0) {
            age_younger <- suppressWarnings(as.numeric(gsub('\n', '', 
                                                      chron_set[depths,grep('young', chron_set[1,], 
                                                                        ignore.case = TRUE)])))
          } else {
            age_younger <- rep(NA, sum(depths))
          }
          
          if (length(grep(paste0('^\n#Chron',i,'\n$'), chron_set[1,])) > 0) {
            age <- suppressWarnings(as.numeric(gsub('\n', '', 
                                                    chron_set[depths,grep(paste0('^\n#Chron',i,'\n$'), 
                                                                      chron_set[1,], 
                                                                      ignore.case = TRUE)])))  
          } else {
            age <- rep(NA, sum(depths))
          }
            
        } else {
          # There's only one vector of ages, check if they're the age, 
          # older or younger
          if (length(grep(paste0("Chron", i, ".Old"), chron_set[1])) == 0) {
            age_older <- rep(NA, sum(depths))
          } else {
            age_older <- suppressWarnings(as.numeric(chron_set[depths]))
          }
          if (length(grep(paste0("Chron", i, ".Young"), chron_set[1])) == 0) {
            age_younger <- rep(NA, sum(depths))
          } else {
            age_younger <- suppressWarnings(as.numeric(chron_set[depths]))
          }
          if (length(grep(paste0("^\n#Chron", i, "\n$"), chron_set[1])) == 0) {
            age <- rep(NA, sum(depths))
          } else {
            age <- suppressWarnings(as.numeric(chron_set[depths]))
          }
        }
        
        if (is.null(ncol(chron_set))) {
          chron_name <- gsub('\n', '', chron_set[2])
          age_type   <- gsub('\n', '', chron_set[4])
        } else {
          chron_name <- gsub('\n', '', chron_set[2, which.min(nchar(chron_set[2,]))])
          age_type   <- gsub('\n', '', chron_set[4, which.min(nchar(chron_set[2,]))])
        }
        
        sample.meta[[i]] <- data.frame(depth = as.numeric(gsub('\n', '', all_sample[depths,1])),
                                  thick = NA,
                                  age.older = age_older,
                                  age = age,
                                  age.younger = age_younger,
                                  chronology.name = chron_name,
                                  age.type = age_type,
                                  chronology.id = NA,
                                  dataset.id = NA)
      }
      else {
      
        # There's no actual date stuff, just a raw age model:
        models <- xml2::xml_find_all(tilia_xml, ".//AgeModel")
        
        if (length(models) == 0) {
          sample.meta[[i]] <- data.frame(depth = NA,
                                         thick = NA,
                                         age.older = NA,
                                         age = NA,
                                         age.younger = NA,
                                         chronology.name = NA,
                                         age.type = NA,
                                         chronology.id = NA,
                                         dataset.id = NA)
          
        } else {
          sample.meta[[i]] <- data.frame(depth = NA,
                                         thick = NA,
                                         age.older = xml2::xml_double(xml2::xml_find_all(models, ".//AgeBoundOlder")),
                                         age = NA,
                                         age.younger = xml2::xml_double(xml2::xml_find_all(models, ".//AgeBoundYounger")),
                                         chronology.name = xml2::xml_text(xml2::xml_find_all(models, ".//ChronologyName")),
                                         age.type = xml2::xml_text(xml2::xml_find_all(models, ".//AgeUnits")),
                                         chronology.id = NA,
                                         dataset.id = NA)
        }
      }
    }
    
    # There can be only one sample.meta though.  The default will be (from now on) the
    # chronology with the highest number in the chron index (if there's more than one).
    chronologies <- sample.meta
    
    if (length(sample.meta) > 1) {
      sample.meta <- sample.meta[[which.max(names(sample.meta))]]
      if (colSums(apply(do.call(rbind, chronologies), 2, duplicated)) == (length(chronologies) - 1)) {
        # This gets rid of empty chronologies, which seem to be a thing that happens . . .
        chronologies <- chronologies[[1]]
      }
    } else {
      sample.meta <- sample.meta[[1]]
    }
    
    # Here we want to push the chroncontrols for each model into a list:
    
    models <- xml2::xml_find_all(tilia_xml, ".//AgeModel")
    
    if (length(models) == 0) {
      chron_controls <- list(data.frame(age.older = NA,
                                        age = NA,
                                        age.younger =  NA,
                                        chronology.name = NA,
                                        age.type = NA,
                                        chronology.id = NA,
                                        dataset.id = NA))
      warning("There is no age model for this record.")
    } else {
    
      get_controls <- function(x) {
        
        #default = which(sapply(tilia_list$AgeModels, function(x)x$Default == "True"))
        controls <- xml2::xml_find_first(x, ".//ChronControls")
        
        if (xml2::xml_attr(controls, "Count") == 0) {
          # This is a special case for directly dated material (mostly?) where you have a single sample:
          controls <- data.frame(age.older = as.numeric(xml2::xml_text(xml2::xml_find_all(x, ".//AgeBoundOlder"))),
                                 age = NA,
                                 age.younger =  as.numeric(xml2::xml_text(xml2::xml_find_all(x, ".//AgeBoundYounger"))),
                                 chronology.name = (xml2::xml_text(xml2::xml_find_all(x, ".//ChronologyName"))),
                                 age.type = (xml2::xml_text(xml2::xml_find_all(x, ".//AgeUnits"))),
                                 chronology.id = NA,
                                 dataset.id = NA)
        } else {
          
          controls <- data.frame(age.older = as.numeric(xml2::xml_text(xml2::xml_find_all(controls, ".//AgeLimitOlder"))),
                                 age = as.numeric(xml2::xml_text(xml2::xml_find_all(controls, ".//Age"))),
                                 age.younger =  as.numeric(xml2::xml_text(xml2::xml_find_all(controls, ".//Age"))),
                                 chronology.name = (xml2::xml_text(xml2::xml_find_all(x, ".//ChronologyName"))),
                                 age.type = (xml2::xml_text(xml2::xml_find_all(x, ".//AgeUnits"))),
                                 chronology.id = NA,
                                 dataset.id = NA)
        }
        
        return(controls)
        
      }
      
      chron_controls <- lapply(models, get_controls)
    }
    
    # Put information in sample.meta if it was missing because we were looking at directly dated material:
    if (all(is.na(sample.meta)) & length(chron_controls) == 1 & nrow(chron_controls[[1]]) == 1) {
      
      matched_cols <- colnames(sample.meta)[colnames(sample.meta) %in% colnames(chron_controls[[1]])]
      sample.meta[,matched_cols] <- chron_controls[[1]][,matched_cols]
      chronologies[[1]] <- sample.meta
      
    }
    
    #  Everything in the spreadsheet table,
    #  but everything (chrons &cetera) are all together.
    #  First we need to pull out the taxonomy data (it doesn't start with a hash mark)
    
    count_cols <- regexpr('#', all_sample[1,]) < 0 & !is.na(all_sample[1,])
    
    # This is problematic.  There are some records where the "Elements" and others
    # are not entered.
    
    if (nrow(all_sample) < 5) {
      stop("Tilia file is missing variable context - Units, Context or Elements are missing.")
    }
    
    taxon_sub <- all_sample[1:5, count_cols]
    
    col_vis <- function(x, sheet){
      # parse the boolean attributes
      if (x %in% cols) {
        col <- which(xml2::xml_attr(xml2::xml_find_all(sheet, "//Col"), "ID") == x)
        
        # Fails if there are no age models:
        first_row <- grep("#", all_sample[1,])
        first_row <- ifelse(length(first_row) == 0, 3, max(first_row))
        
        return(all_sample[col,(first_row + 1):ncol(all_sample)])
      } else {
        NA
      }
    }
    
    taxon_table <- get_table("Taxa")
    
    taxa_list <- data.frame(taxon.name          = col_vis(2, spreadsheet),
                            variable.units      = col_vis(4, spreadsheet),
                            variable.element    = col_vis(3, spreadsheet),
                            variable.context    = col_vis(5, spreadsheet),
                            variable.taphonomy  = col_vis(6, spreadsheet),
                            taxon.group         = NA,
                            ecological.group    = col_vis(7, spreadsheet))
    
    if (!all(is.na(taxa_list$taxon.name))) {
      taxa_list$taxon.group <- taxon_table$TaxaGroupID[match(taxa_list$taxon.name, 
                                                             taxon_table$TaxonName)]
    }
    
    # Where is count starting:
    count_start <- sum(!apply(apply(taxa_list, 2, is.na), 2, all)) + 1
    # Second most common choke point for records:
    count_data <- matrix(as.numeric(gsub('\n', '', all_sample[count_start:nrow(all_sample), count_cols])),
                         ncol = sum(count_cols))
    
    lab_data   <- count_data[, taxa_list$ecological.group == 'LABO']
    count_data <- count_data[, !taxa_list$ecological.group == 'LABO']
    
    aa <- list(dataset        = dataset,
               sample.meta    = sample.meta,
               taxon_list     = taxa_list,
               counts         = count_data,
               lab.data       = lab_data,
               chron_controls = chron_controls,
               chronologies   = chronologies)
    
    class(aa) <- c('download', 'list')
    
    aa
}

