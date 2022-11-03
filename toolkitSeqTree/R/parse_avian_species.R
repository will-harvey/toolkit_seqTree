#' Parse avian species
#'
#' Tidy up some commonly occurring errors in bird species names in influenza
#' sequence data bases and match to an avian order when possible
#'
#' @param dat dataframe with column
#' @param species_col the name of the column with location info to be parsed
#'
#' @return dataframe with tidied species
#' @export
#'
#' @examples
parse_avian_species <- function(dat = NA, species = 'species') {

  species <- dat[[species]]
  species <- tolower(species)
  species <- tolower(species)
  # remove trailing underscore
  species <- gsub('_$', '', species)
  species <- gsub('geese', 'goose', species)
  species <- gsub('^gray', 'grey', species)
  species <- gsub('aalopochen', 'alopochen', species)
  species <- gsub('^barnacle$', 'barnacle_goose', species)
  species <- gsub('canade_goose', 'canada_goose', species)
  species <- gsub('chichen', 'chicken', species)
  species <- gsub('chiken', 'chicken', species)
  species <- gsub('^cignus', 'cygnus_olor', species)
  species <- gsub('towny_owel', 'tawny_owl', species)
  species <- gsub('withe-tiled_eagle', 'white-tailed_eagle', species)


  ## Maintain lists of bird species as appear in data for each order
  accipitriformes <- c('bald_eagle', 'black_vulture', 'buteo_buteo', 'buzzard', 'common_buzzard', "cooper's_hawk", 'eagle', 'eastern_buzzard', 'golden_eagle', 'goshawk', 'gypaetus_barbatus', 'hawk', 'kite','northern_goshawk', 'red-shouldered_hawk', 'sea_eagle', 'sparrowhawk', 'vulture', 'western_marsh_harrier', 'white-tailed_eagle')
  anseriformes <- c('american_blue-winged_teal', 'american_buff_goose', 'american_wigeon', 'anas_platyrhynchos', 'anas_platyrhynchos_domestica', 'anser_albifrons', 'anser_anser', 'anser_anser_domesticus', 'anser_brachyrhynchus', 'anser_brachyrhynchus_anser_anser', 'bar_headed_goose', 'bar-headed_goose', 'barnacle_goose', 'bean_goose', 'black_swan', 'brant_goose', 'branta_canadensis', 'branta_leucopsis', 'brent_goose', 'cascade_duck', 'cignus_olor', 'common_eider', 'common_golden_eye', 'common_goldeneye', 'common_pochard', 'common_teal', 'cygnus_columbianus', 'cygnus_olor', 'domestic_duck', 'domestic_goose', 'duck', 'egyptian_goose', 'embden_goose', 'emperor_goose', 'eurasian_teal', 'eurasian_wigeon', 'ferruginous_duck', 'gadwall', 'garganey', 'geese', 'glaucous-winged_gull', 'goose', 'greater_canada_goose', 'greater_white-fronted_goose', 'green-winged_teal', 'green-winged-teal', 'greylag_goose', 'greylag_goose_', 'hawaiian_goose', 'lesser_black-backed_gull', 'lesser_white-fronted_goose', 'mallard', 'mallard_duck', 'mandarin_duck', 'mule_duck', 'muscovy_duck', 'mute_swan', 'northern_pintail', 'northern_shoveler', 'pink_footed_goose', 'pink-footed_goose', 'pomeranian_goose', 'red-breasted_goose', 'sebastopol_goose', 'snow_goose', 'spot-billed_duck', 'steamer_duck', 'swan', 'taiga_bean_goose', 'teal', 'tufted_duck', 'tundra_bean_goose', 'tundra_swan', 'waterfowl', 'whistling_duck', 'white-fronted_goose', 'whooper_swan', 'wigeon', 'wild_duck', 'wild_goose')
  casuariiformes <- c('emu')
  charadriiformes <- c('black-backed_gull', 'black-headed_gull', 'brown-headed_gull', 'canada_goose', 'caspian_gull', 'chlidonias_hybrida', 'common_gull', 'common_murre', 'common_snipe', 'common_tern', 'curlew', 'eurasian_curlew', 'eurasian_oystercatcher', 'european_herring_gull', 'great_black-backed_gull', 'great_skua', 'green_sandpiper', 'gull', 'herring_gull', 'knot_wader', 'lapwing', 'laridae', 'larus_argentatus', 'larus_canus', 'little_gull', 'northern_lapwing', 'numenius_arquata', 'oystercatcher', 'red_knot', 'ruddy_turnstone', 'sanderling', 'sandwich_tern', 'seagull', 'thalasseus_sandvicensis', 'whiskered_tern', 'yellow-legged_gull')
  ciconiiformes <- c('ciconia_ciconia', 'stork', 'white_stork')
  columbiformes <- c('columba_palumbus', 'dove', 'pigeon')
  falconiformes <- c('common_kestrel', 'falco_peregrinus', 'falcon', 'kestrel', 'peregrine_falcon')
  galliformes <- c('australian_brushturkey', 'broiler', 'broiler_chicken', 'chicken','common_peacock', 'common_pheasant', 'common_pheasant_', 'common_quail', 'fancy_chicken', 'gallus_gallus', 'guinea_fowl', 'guineafowl', 'hen', 'layer', 'laying_hen', 'meleagris_gallopavo', 'partridge', 'peacock', 'peafowl', 'pekin_duck', 'pheasant', 'poultry', 'quail', 'rooster', 'silkie_chicken', 'turkey', 'white_peacock')
  gruiformes <- c('common_coot', 'common_crane', 'coot', 'crane', 'hooded_crane')
  otidiformes <- c('bustard')
  passeriformes <- c('common_raven', 'crow', 'garrulus_glandarius', 'house_sparrow', 'magpie', 'pica_pica', 'song_thrush', 'western_jackdaw')
  pelecaniformes <- c('ardea_cinerea', 'dalmatian_pelican', 'egret', 'eurasian_spoonbill', 'great_egret', 'great-white_pelican', 'grey_heron', 'heron', 'pelecanus_crispus', 'pelican')
  phoenicopteriformes <- c('flamingo')
  podicipediformes <- c('grebe', 'podiceps_cristatus', 'tachybaptus_ruficollis')
  procellariiformes <- c('albatross', 'petrel', 'shearwater', 'storm_petrel')
  rheiformes <- c('greater_rhea', 'rhea')
  psittaciformes <- c('amazon_parrot', 'catalina_macaw', 'macaw', 'parrot')
  strigiformes <- c('eagle_owl', 'eurasian_eagle-owl', 'little_owl', 'long-eared_owl', 'owl', 'short-eared_owl', 'tawny_owl', 'tyto_alba')
  suliformes <- c('cormorant', 'great_cormorant', 'northern_gannet', 'nothern_gannet', 'phalacrocorax_carbo')

  # Create a vector to record order matched from species
  order <- rep(NA, length(species))
  order <- ifelse(order %in% accipitriformes, 'Accipitriformes', order)
  order <- ifelse(order %in% anseriformes, 'Anseriformes', order)
  order <- ifelse(order %in% casuariiformes, 'Casuariiformes', order)
  order <- ifelse(order %in% charadriiformes, 'Charadriiformes', order)
  order <- ifelse(order %in% ciconiiformes, 'Ciconiiformes', order)
  order <- ifelse(order %in% columbiformes, 'Columbiformes', order)
  order <- ifelse(order %in% falconiformes, 'Falconiformes', order)
  order <- ifelse(order %in% galliformes, 'Galliformes', order)
  order <- ifelse(order %in% gruiformes, 'Gruiformes', order)
  order <- ifelse(order %in% otidiformes, 'Otidiformes', order)
  order <- ifelse(order %in% passeriformes, 'Passeriformes', order)
  order <- ifelse(order %in% pelecaniformes, 'Pelecaniformes', order)
  order <- ifelse(order %in% phoenicopteriformes, 'Phoenicopteriformes', order)
  order <- ifelse(order %in% podicipediformes, 'Podicipediformes', order)
  order <- ifelse(order %in% procellariiformes, 'Procellariiformes', order)
  order <- ifelse(order %in% rheiformes, 'Rheiformes', order)
  order <- ifelse(order %in% psittaciformes, 'Psittaciformes', order)
  order <- ifelse(order %in% strigiformes, 'Strigiformes', order)
  order <- ifelse(order %in% suliformes, 'Suliformes', order)

  # replace input column with tidied version and add column for order
  dat[[species]] <- species
  dat$order <- order

  dat
}
