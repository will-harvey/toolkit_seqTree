#' Amino acid information
#'
#' Returnes a data frame with information on each amino acid including
#' a biophysics-based colouring scheme
#'
#' @return data frame with amino acid information
#' @export
#'
#' @examples
#' aa_info()
aa_info <- function() {
  aa.single.letter <- c("G", "A", "I", "L", "P", "V", "F", "Y", "W", "D",
                        "E", "H", "K", "R", "S", "T", "C", "M", "N", "Q")

  x <- data.frame(aa = aa.single.letter,
                  aminoacid = NA,
                  category = NA,
                  plot_col = NA,
                  plot_order = 1:20)

  x$code3[x$aa == "G"] <- "Gly"
  x$code3[x$aa == "A"] <- "Ala"
  x$code3[x$aa == "I"] <- "Ile"
  x$code3[x$aa == "L"] <- "Leu"
  x$code3[x$aa == "P"] <- "Pro"
  x$code3[x$aa == "V"] <- "Val"
  x$code3[x$aa == "F"] <- "Phe"
  x$code3[x$aa == "Y"] <- "Tyr"
  x$code3[x$aa == "W"] <- "Trp"
  x$code3[x$aa == "D"] <- "Asp"
  x$code3[x$aa == "E"] <- "Glu"
  x$code3[x$aa == "H"] <- "His"
  x$code3[x$aa == "K"] <- "Lys"
  x$code3[x$aa == "R"] <- "Arg"
  x$code3[x$aa == "S"] <- "Ser"
  x$code3[x$aa == "T"] <- "Thr"
  x$code3[x$aa == "C"] <- "Cys"
  x$code3[x$aa == "M"] <- "Met"
  x$code3[x$aa == "N"] <- "Asn"
  x$code3[x$aa == "Q"] <- "Gln"

  x$aminoacid[x$aa == "G"] <- "Glycine"
  x$aminoacid[x$aa == "A"] <- "Alanine"
  x$aminoacid[x$aa == "I"] <- "Isoleucine"
  x$aminoacid[x$aa == "L"] <- "Leucine"
  x$aminoacid[x$aa == "P"] <- "Proline"
  x$aminoacid[x$aa == "V"] <- "Valine"
  x$aminoacid[x$aa == "F"] <- "Phenylalanine"
  x$aminoacid[x$aa == "Y"] <- "Tyrosine"
  x$aminoacid[x$aa == "W"] <- "Tryptophan"
  x$aminoacid[x$aa == "D"] <- "Aspartic acid"
  x$aminoacid[x$aa == "E"] <- "Glutamic acid"
  x$aminoacid[x$aa == "H"] <- "Histodine"
  x$aminoacid[x$aa == "K"] <- "Lysine"
  x$aminoacid[x$aa == "R"] <- "Arginine"
  x$aminoacid[x$aa == "S"] <- "Serine"
  x$aminoacid[x$aa == "T"] <- "Threonine"
  x$aminoacid[x$aa == "C"] <- "Cysteine"
  x$aminoacid[x$aa == "M"] <- "Methionine"
  x$aminoacid[x$aa == "N"] <- "Asparagine"
  x$aminoacid[x$aa == "Q"] <- "Glutamine"

  x$category[x$aa == "G"] <- "aliphatic"
  x$category[x$aa == "A"] <- "aliphatic"
  x$category[x$aa == "I"] <- "aliphatic"
  x$category[x$aa == "L"] <- "aliphatic"
  x$category[x$aa == "P"] <- "aliphatic"
  x$category[x$aa == "V"] <- "aliphatic"
  x$category[x$aa == "F"] <- "aromatic"
  x$category[x$aa == "Y"] <- "aromatic"
  x$category[x$aa == "W"] <- "aromatic"
  x$category[x$aa == "D"] <- "acidic"
  x$category[x$aa == "E"] <- "acidic"
  x$category[x$aa == "H"] <- "basic"
  x$category[x$aa == "K"] <- "basic"
  x$category[x$aa == "R"] <- "basic"
  x$category[x$aa == "S"] <- "hydroxylic"
  x$category[x$aa == "T"] <- "hydroxylic"
  x$category[x$aa == "C"] <- "sulfur-containing"
  x$category[x$aa == "M"] <- "sulfur-containing"
  x$category[x$aa == "N"] <- "amidic"
  x$category[x$aa == "Q"] <- "amidic"

  x$plot_col[x$aa == "G"] <- "grey95"
  x$plot_col[x$aa == "A"] <- "grey80"
  x$plot_col[x$aa == "I"] <- "grey60"
  x$plot_col[x$aa == "L"] <- "grey40"
  x$plot_col[x$aa == "P"] <- "grey20"
  x$plot_col[x$aa == "V"] <- "black"
  x$plot_col[x$aa == "F"] <- "olivedrab3"
  x$plot_col[x$aa == "Y"] <- "olivedrab4"
  x$plot_col[x$aa == "W"] <- "olivedrab1"
  x$plot_col[x$aa == "D"] <- "orange"
  x$plot_col[x$aa == "E"] <- "darkorange3"
  x$plot_col[x$aa == "H"] <- "skyblue1"
  x$plot_col[x$aa == "K"] <- "dodgerblue1"
  x$plot_col[x$aa == "R"] <- "dodgerblue4"
  x$plot_col[x$aa == "S"] <- "firebrick1"
  x$plot_col[x$aa == "T"] <- "firebrick3"
  x$plot_col[x$aa == "C"] <- "gold"
  x$plot_col[x$aa == "M"] <- "yellow"
  x$plot_col[x$aa == "N"] <- "darkorchid1"
  x$plot_col[x$aa == "Q"] <- "darkorchid4"

  x
}
