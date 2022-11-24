#' Avian orders
#'
#' Information to remind on birds comprising avian orders.
#'
#' @param input by default, all info will be returned though a specific order can be supplied to minimise info returned
#'
#' @export
avian_orders <- function(input = NA) {
  orders <- c('accipitriformes', 'anseriformes', 'casuariiformes', 'charadriiformes',
              'ciconiiformes', 'columbiformes', 'falconiformes', 'galliformes',
              'gruiformes', 'otidiformes', 'passeriformes', 'pelecaniformes',
              'phoenicopteriformes', 'podicipediformes', 'procellariiformes',
              'rheiformes', 'psittaciformes', 'sphenisciformes', 'strigiformes',
              'struthioniformes', 'suliformes')
  orders <- data.frame(order = orders,
                       info = NA)
  orders$info[orders$order == 'accipitriformes'] <- c('Diurnal birds of prey excl. falcons')
  orders$info[orders$order == 'anseriformes'] <- c('Waterfowl incl. ducks, geese & swans')
  orders$info[orders$order == 'casuariiformes'] <- c('Cassowary and emu')
  orders$info[orders$order == 'charadriiformes'] <- c('Shorebirds: waders, gulls, terns & auks')
  orders$info[orders$order == 'ciconiiformes'] <- c('Storks')
  orders$info[orders$order == 'columbiformes'] <- c('Doves and pigeons')
  orders$info[orders$order == 'falconiformes'] <- c('Falcons')
  orders$info[orders$order == 'galliformes'] <- c('Landfowl incl. turkeys, chickens & quails')
  orders$info[orders$order == 'gruiformes'] <- c('Cranes, crakes & rails (e.g. coot, moorhen)')
  orders$info[orders$order == 'otidiformes'] <- c('Bustards')
  orders$info[orders$order == 'passeriformes'] <- c('Perching birds')
  orders$info[orders$order == 'pelecaniformes'] <- c('Ibises & spoonbills, herons, pelicans, shoebill & hamerkop')
  orders$info[orders$order == 'phoenicopteriformes'] <- c('Flamingoes')
  orders$info[orders$order == 'podicipediformes'] <- c('Grebes')
  orders$info[orders$order == 'procellariiformes'] <- c('Albatrosses, petrels and shearwaters')
  orders$info[orders$order == 'psittaciformes'] <- c('Parrots')
  orders$info[orders$order == 'rheiformes'] <- c('The Rhea')
  orders$info[orders$order == 'sphenisciformes'] <- c('Penguins')
  orders$info[orders$order == 'strigiformes'] <- c('Owls')
  orders$info[orders$order == 'struthioniformes'] <- c('Ostriches')
  orders$info[orders$order == 'suliformes'] <- c('Cormorants & Shags, Boobys & Gannets, Darters & Frigatebirds')

  # If an order is provided, return info for it only
  input <- tolower(input)
  if (is.na(input)) {
    return(orders)
  } else if (input %in% orders$order) {
    return(orders[orders$order == input,])
  } else if (!input %in% orders$order) {
    cat('Order not recognised. Returning full data frame:\n')
    return(orders)
  }

}
