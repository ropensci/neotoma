### Neotoma - fully test the `get_download` method (using the USA as a subset):
### Run through each dataset type and figure out which records fail.
### Returns a big long list with the error information, dataset type & dataset ID
### for further testing.

types <- c('geochronologic','loss-on-ignition','pollen',
           'plant macrofossil','vertebrate fauna','macroinvertebrate',
           'pollen surface sample','insect','ostracode',
           'water chemistry','diatom','ostracode surface sample',
           'diatom surface sample','geochemistry','physical sedimentology',
           'charcoal','testate amoebae','X-ray fluorescence (XRF)',
           'X-ray diffraction (XRD)','Energy dispersive X-ray spectroscopy (EDS/EDX)')

errors <- list()
count <- 1

for(j in 1:length(types)){
  datasets <- get_dataset(datasettype = types[j], gpid= 6129)
  if(length(datasets) > 0){
    for(i in 1:length(datasets)){
      test <- try(get_download(datasets[[i]]))
      if(class(test) == 'try-error'){
        errors[[count]] <- data.frame(id = datasets[[i]]$dataset.meta$dataset.id,
                                      type = types[j],
                                      msg = test[1])
        count <- count + 1
      }
    }
  }
}

get_download(error.df$id)

