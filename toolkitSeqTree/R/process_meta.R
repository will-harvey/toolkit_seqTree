#' Process metadata
#'
#' @param meta_dat data frame with columns 'Isolate_Id', 'Isolate_Name', 'Subtype', Location','Host', 'Collection_Date', 'Domestic_Status'.
#' @param drop.recombinant option to drop any isolates labelled as recombinant from output. Defaults to
#' @param verbose option to print information on variables as they are created. Defaults to FALSE
#'
#' @return
#' @export
#'
process_meta <- function(meta_dat = NA, drop.laboratory = TRUE, verbose = FALSE) {

  meta_dat <- meta_dat[, c('Isolate_Id', 'Isolate_Name', 'Subtype', 'Location',
                           'Host', 'Collection_Date', 'Domestic_Status')]

  names(meta_dat) <- tolower(names(meta_dat))


  ### --------------------------------------------------------------------------------
  ### Drop laboratory-derived isolates
  if (drop.laboratory == T) {
    meta_dat <- meta_dat[grepl('recombinant', meta_dat$isolate_name) == F,]
    meta_dat <- meta_dat[grepl('reassortant', meta_dat$isolate_name) == F,]
    meta_dat <- meta_dat[grepl('laboratory', meta_dat$host) == F,]
  }

  ### --------------------------------------------------------------------------------
  ### Subtype
  meta_dat$subtype <- gsub('\\s', '', meta_dat$subtype)
  meta_dat$subtype <- gsub('^A/', '', meta_dat$subtype)

  ### --------------------------------------------------------------------------------
  ### Dates
  names(meta_dat) <- gsub('^collection_date$', 'date', names(meta_dat))
  # some wrong dates from early 1900s
  meta_dat$date <- ifelse(meta_dat$date < '1910-01-01', NA, meta_dat$date)
  # variable - is there a full date (10 characters)
  meta_dat$date_complete <- ifelse(stringr::str_length(meta_dat$date) == 10,
                                   TRUE, FALSE)
  # variable - get decimal date from date
  meta_dat$date_frac <- ifelse(meta_dat$date_complete == TRUE,
                               ggtree::Date2decimal(meta_dat$date), NA)
  meta_dat$date_frac <- round(meta_dat$date_frac, 3)
  # Get year (from decimal date)
  meta_dat$date_year <- floor(meta_dat$date_frac)
  # Get year and month from formatted full date
  meta_dat$date_year_month <- ifelse(meta_dat$date_complete == TRUE,
                                     gsub('-[0-9][0-9]$', '', meta_dat$date), NA)
  # Get a variable which is the day of the year
  meta_dat$date_yday <- NA
  meta_dat$date_year_week <- NA
  for (i in 1:nrow(meta_dat)) {
    if (is.na(meta_dat$date_complete[i]) == F) {
      if (meta_dat$date_complete[i] == TRUE) {
        meta_dat$date_yday[i] <- lubridate::yday(meta_dat$date[i])
        meta_dat$date_year_week[i] <- paste(meta_dat$date_year[i],
                                            (meta_dat$date_yday[i] %/% 7) + 1, sep = '.')
      }
    }
  }


  ### --------------------------------------------------------------------------------
  ### Host
  # meta host
  meta_dat$host_meta <- tolower(meta_dat$host)
  meta_dat$host_meta <- gsub('^[[:space:]]+', '', meta_dat$host_meta)
  meta_dat$host_meta <- gsub('[[:space:]]+$', '', meta_dat$host_meta)
  meta_dat$host_meta <- gsub('[[:space:]]+', '_', meta_dat$host_meta)
  table(meta_dat$host_meta)

  # host is tidied version of name as appears in name
  # extract from name (has more detail than metadata)
  meta_dat$host <- meta_dat$isolate_name
  meta_dat$host <- gsub('^[[:space:]]+', '', meta_dat$host)
  meta_dat$host <- gsub('[[:space:]]+$', '', meta_dat$host)
  meta_dat$host <- gsub('[[:space:]]+', '_', meta_dat$host) # replace whitespace
  meta_dat$host <- gsub('^A\\/', '', meta_dat$host) # remove A/
  meta_dat$host <- stringr::str_extract(meta_dat$host, "^[A-Za-z0-9\\-_\\'\\s\\(\\)]+/") # extract [species]/
  meta_dat$host <- gsub('/$', '', meta_dat$host)
  # Now have host extracted
  meta_dat$host <- tolower(meta_dat$host)
  meta_dat$host <- gsub("'", '', meta_dat$host) # remove '
  meta_dat$host <- gsub('"', '', meta_dat$host) # remove "
  meta_dat$host <- gsub('_$', '', meta_dat$host) # remove trailing '_'
  meta_dat$host <- gsub('^gray', 'grey', meta_dat$host)
  meta_dat$host <- gsub('geese', 'goose', meta_dat$host)
  meta_dat$host <- gsub('cignus', 'cygnus', meta_dat$host)
  meta_dat$host <- gsub('aalopochen', 'alopochen', meta_dat$host)
  meta_dat$host <- gsub('^barnacle$', 'barnacle_goose', meta_dat$host)
  meta_dat$host <- gsub('canade_goose', 'canada_goose', meta_dat$host)
  meta_dat$host <- gsub('chichen', 'chicken', meta_dat$host)
  meta_dat$host <- gsub('chiken', 'chicken', meta_dat$host)
  meta_dat$host <- gsub('towny_owel', 'tawny_owl', meta_dat$host)
  meta_dat$host <- gsub('withe-tiled_eagle', 'white-tailed_eagle', meta_dat$host)
  meta_dat$host <- ifelse(meta_dat$host_meta == 'human', 'human', meta_dat$host)

  meta_dat$host_order <- parse_avian_species(host = meta_dat$host)

  if (verbose == TRUE) {
    cat("\nNumber of isolates in each host order:")
    print(table(meta_dat$host_order, useNA = 'always'))
    cat("\n'Host' from name where order is NA")
    print(unique(meta_dat$host[is.na(meta_dat$host_order)]))
    cat("\nNumber of isolates in each host order assigned domestic and wild:")
    print(table(meta_dat$host_order, meta_dat$domestic_status))
  }

  ### Domestic v wild
  meta_dat$domestic_wild <- 'wild'
  meta_dat$domestic_wild <- ifelse(meta_dat$host_order == 'environment',
                                   NA, meta_dat$domestic_wild)
  meta_dat$domestic_wild <- ifelse(meta_dat$host_order == 'Galliformes',
                                   'domestic', meta_dat$domestic_wild)
  meta_dat$domestic_wild <- ifelse(meta_dat$host %in% c('domestic_duck',
                                                        'domestic_goose',
                                                        'duck',
                                                        'goose',
                                                        'muscovy_duck',
                                                        'ornamental_bird'),
                                   'domestic', meta_dat$domestic_wild)
  # overwrite if info on status appears in isolate name
  meta_dat$domestic_wild <- ifelse(grepl('domestic', meta_dat$host),
                                   'domestic', meta_dat$domestic_wild)
  meta_dat$domestic_wild <- ifelse(grepl('wild', meta_dat$host),
                                   'wild', meta_dat$domestic_wild)
  # overwrite with info in metadata
  meta_dat$domestic_wild <- ifelse(meta_dat$domestic_status == 'Domestic',
                                   'domestic', meta_dat$domestic_wild)
  meta_dat$domestic_wild <- ifelse(meta_dat$domestic_status == 'Wild',
                                   'wild', meta_dat$domestic_wild)

  if (verbose == TRUE) {
    cat("\nNumber of isolates in domestic and wild catergories:")
    print(table(meta_dat$domestic_wild, useNA = 'ifany'))
    cat("\nHost from name for any isolates with NA for domestic status:")
    print(meta_dat$host[is.na(meta_dat$domestic_wild)])
  }


  ### Host class
  # Galliformes, Anseriformes domestic, Anseriformes wild, Charadriiformes
  # Other avian, Mammal
  meta_dat$host_class <- 'Avian other'
  meta_dat$host_class <- ifelse(meta_dat$host_order == 'Galliformes',
                                'Galliformes', meta_dat$host_class)
  meta_dat$host_class <- ifelse(meta_dat$host_order == 'Anseriformes' &
                                  meta_dat$domestic_wild == 'domestic',
                                'Anseriformes domestic', meta_dat$host_class)
  meta_dat$host_class <- ifelse(meta_dat$host_order == 'Anseriformes' &
                                  meta_dat$domestic_wild == 'wild',
                                'Anseriformes wild', meta_dat$host_class)
  meta_dat$host_class <- ifelse(meta_dat$host_order == 'Charadriiformes',
                                'Charadriiformes', meta_dat$host_class)
  meta_dat$host_class <- ifelse(meta_dat$host_order == 'Mammal',
                                'Mammal', meta_dat$host_class)
  meta_dat$host_class <- ifelse(meta_dat$host_order == 'environment',
                                'environment', meta_dat$host_class)
  if (verbose == TRUE) {
    cat("\nNumber of isolates in each host class:")
    print(table(meta_dat$host_class, useNA = 'always'))
  }
  meta_dat
}
