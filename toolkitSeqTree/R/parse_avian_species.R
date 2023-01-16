#' Parse avian species
#'
#' Tidy up some commonly occurring errors in bird species names in influenza
#' sequence data bases and match to an avian order when possible
#'
#' @param host vector of host labels extracted from virus isolate names
#' @param output desired output type
#'
#' @return dataframe with tidied species
#' @export
#'
#' @examples
parse_avian_species <- function(host = NA, output = 'order') {

  if (output == 'order') {
    host <- tolower(host)

    ## Maintain lists of avian hosts per avian order
    accipitriformes <- c( 'buteo_buteo', 'gypaetus_barbatus', 'kite', 'western_marsh_harrier')
    anseriformes <- c('alopochen_aegyptiaca', 'american_wigeon', 'bean_goose(anser_fabalis)', 'branta_canadensis', 'branta_leucopsis', 'common_eider', 'common_eiders', 'common_golden_eye', 'common_goldeneye', 'common_pochard', 'cygnus_columbianus', 'cygnus_olor', 'eurasian_wigeon', 'gadwall', 'garganey', 'hooded_merganser', 'mallard', 'mallard(anas_platyrhynchos)',  'mixed_domestic_anatidae', 'northern_pintail', 'northern_shoveler', 'waterfowl', 'white-fronted_goose(anser_albifrons)', 'wigeon')
    casuariiformes <- c('cassowary', 'emu')
    charadriiformes <- c('chlidonias_hybrida', 'common_murre', 'common_snipe', 'curlew', 'eurasian_curlew', 'eurasian_oystercatcher', 'great_skua', 'green_sandpiper', 'knot_wader', 'lapwing', 'laridae', 'larus_argentatus', 'larus_canus', 'northern_lapwing', 'numenius_arquata', 'oystercatcher', 'red_knot', 'red-necked_stint', 'ruddy_turnstone', 'sanderling', 'thalasseus_sandvicensis', 'white-backed_stilt')
    ciconiiformes <- c('ciconia_ciconia', 'stork')
    columbiformes <- c('columba_palumbus', 'dove', 'pigeon', 'streptopelia_decaocto')
    falconiformes <- c('common_kestrel', 'falco_peregrinus', 'falcon', 'kestrel', 'peregrine_falcon')
    galliformes <- c('australian_brushturkey', 'backyard_poultry', 'brahma_chicken', 'broiler', 'broiler_chicken', 'chicken', 'chickenl', 'common_peacock', 'common_pheasant', 'common_pheasant_', 'common_quail', 'fancy_chicken', 'gallus_gallus', 'gallus_gallus_domesticus', 'guinea_fowl', 'guineafowl', 'hen', 'korean_native_chicken', 'layer', 'laying_hen', 'meleagris_gallopavo', 'mixed_domestic_phasianidae', 'pavo', 'pavo_cristatus', 'partridge', 'peacock', 'peafowl', 'phasianidae', 'pheasant', 'poultry', 'quail', 'rooster', 'silkie_chicken', 'turkey', 'white_peacock', 'wild_turkey')
    gruiformes <- c('common_coot', 'common_crane', 'coot', 'crane', 'hooded_crane')
    otidiformes <- c('bustard')
    passeriformes <- c('bulbul', 'common_raven', 'garrulus_glandarius', 'house_sparrow', 'luscinia_cyane', 'magpie', 'pica_pica', 'song_thrush', 'western_jackdaw')
    pelecaniformes <- c('ardea_cinerea', 'dalmatian_pelican', 'egret', 'eurasian_spoonbill', 'great_egret', 'great-white_pelican', 'grey_heron', 'heron', 'pelecanus_crispus', 'pelecanus_occidentalis', 'pelican', 'spoonbill')
    phoenicopteriformes <- c('flamingo')
    podicipediformes <- c('grebe', 'little_grebe', 'podiceps_cristatus', 'tachybaptus_ruficollis')
    procellariiformes <- c('albatross', 'fulmar', 'petrel', 'shearwater', 'storm_petrel')
    rheiformes <- c('greater_rhea', 'rhea')
    psittaciformes <- c('amazon_parrot', 'catalina_macaw', 'macaw', 'parrot')
    strigiformes <- c('tyto_alba')
    struthioniformes <- c('common_ostrich', 'ostrich', 'somali_ostrich')
    suliformes <- c('cormorant', 'great_cormorant', 'northern_gannet', 'phalacrocorax_carbo')

    mammal <- c('black_bear', 'bobcat', 'bottlenose_dolphin', 'dolphin', 'european_polecat', 'human', 'mink', 'porpoise', 'raccoon', 'skunk', 'striped_skunk', 'swine','tanuki', 'virginia_opossum', 'vulpes_vulpes')
    unknown_avian <- c('avian', 'bird', 'ornamental_bird', 'wild_bird', 'wild_waterbird')

    output <- rep(NA, length(host))
    output <- ifelse(grepl('buzzard$', host), 'Accipitriformes', output)
    output <- ifelse(grepl('eagle$', host), 'Accipitriformes', output)
    output <- ifelse(grepl('hawk$', host), 'Accipitriformes', output)
    output <- ifelse(grepl('vulture$', host), 'Accipitriformes', output)
    output <- ifelse(grepl('duck$', host), 'Anseriformes', output)
    output <- ifelse(grepl('goose$', host), 'Anseriformes', output)
    output <- ifelse(grepl('swan$', host), 'Anseriformes', output)
    output <- ifelse(grepl('teal$', host), 'Anseriformes', output)
    output <- ifelse(grepl('scaup$', host), 'Anseriformes', output)
    output <- ifelse(grepl('^anas_', host), 'Anseriformes', output)
    output <- ifelse(grepl('^anser_', host), 'Anseriformes', output)
    output <- ifelse(grepl('^cygnus_', host), 'Anseriformes', output)
    output <- ifelse(grepl('gull$', host), 'Charadriiformes', output)
    output <- ifelse(grepl('tern$', host), 'Charadriiformes', output)
    output <- ifelse(grepl('stork$', host), 'Ciconiiformes', output)
    output <- ifelse(grepl('dove$', host), 'Columbiformes', output)
    output <- ifelse(grepl('pigeon$', host), 'Columbiformes', output)
    output <- ifelse(grepl('falcon$', host), 'Falconiformes', output)
    output <- ifelse(grepl('pheasant$', host), 'Galliformes', output)
    output <- ifelse(grepl('chicken', host), 'Galliformes', output)
    output <- ifelse(grepl('_hen$', host), 'Galliformes', output)
    output <- ifelse(grepl('coot$', host), 'Gruiformes', output)
    output <- ifelse(grepl('crane$', host), 'Gruiformes', output)
    output <- ifelse(grepl('crow$', host), 'Passeriformes', output)
    output <- ifelse(grepl('ibis$', host), 'Pelecaniformes', output)
    output <- ifelse(grepl('pelican$', host), 'Pelecaniformes', output)
    output <- ifelse(grepl('grebe$', host), 'Podicipediformes', output)
    output <- ifelse(grepl('albatross$', host), 'Procellariiformes', output)
    output <- ifelse(grepl('petrel$', host), 'Procellariiformes', output)
    output <- ifelse(grepl('parrot$', host), 'Psittaciformes', output)
    output <- ifelse(grepl('macaw$', host), 'Psittaciformes', output)
    output <- ifelse(grepl('penguin$', host), 'Sphenisciformes', output)
    output <- ifelse(grepl('owl$', host), 'Strigiformes', output)
    output <- ifelse(grepl('booby$', host), 'Suliformes', output)
    output <- ifelse(grepl('cormorant$', host), 'Suliformes', output)
    output <- ifelse(grepl('gannet$', host), 'Suliformes', output)
    output <- ifelse(grepl('shag$', host), 'Suliformes', output)
    output <- ifelse(grepl('fox$', host), 'Mammal', output)
    output <- ifelse(grepl('seal$', host), 'Mammal', output)

    output <- ifelse(host %in% accipitriformes, 'Accipitriformes', output)
    output <- ifelse(host %in% anseriformes, 'Anseriformes', output)
    output <- ifelse(host %in% casuariiformes, 'Casuariiformes', output)
    output <- ifelse(host %in% charadriiformes, 'Charadriiformes', output)
    output <- ifelse(host %in% ciconiiformes, 'Ciconiiformes', output)
    output <- ifelse(host %in% columbiformes, 'Columbiformes', output)
    output <- ifelse(host %in% falconiformes, 'Falconiformes', output)
    output <- ifelse(host %in% galliformes, 'Galliformes', output)
    output <- ifelse(host %in% gruiformes, 'Gruiformes', output)
    output <- ifelse(host %in% otidiformes, 'Otidiformes', output)
    output <- ifelse(host %in% passeriformes, 'Passeriformes', output)
    output <- ifelse(host %in% pelecaniformes, 'Pelecaniformes', output)
    output <- ifelse(host %in% phoenicopteriformes, 'Phoenicopteriformes', output)
    output <- ifelse(host %in% podicipediformes, 'Podicipediformes', output)
    output <- ifelse(host %in% procellariiformes, 'Procellariiformes', output)
    output <- ifelse(host %in% rheiformes, 'Rheiformes', output)
    output <- ifelse(host %in% psittaciformes, 'Psittaciformes', output)
    output <- ifelse(host %in% strigiformes, 'Strigiformes', output)
    output <- ifelse(host %in% struthioniformes, 'Struthioniformes', output)
    output <- ifelse(host %in% suliformes, 'Suliformes', output)
    output <- ifelse(host %in% mammal, 'Mammal', output)
    output <- ifelse(host %in% unknown_avian, 'unknown avian', output)
    output <- ifelse(host %in% c('env', 'enviroment', 'environment', 'environmental_(em)', 'water', 'wild_bird_feces'), 'environment', output)
  }

  output
}
