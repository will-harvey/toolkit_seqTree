#' Root-to-tips assignment of states to internal nodes
#'
#' Pre-order traversal of phylogeny assigning ancestral states to internal nodes
#' based on maximum parsimony. Function is designed to be run following a post-order
#' tips-to-root traversal that determines trait options at each internal node under
#' a maximum parsimony scheme.
#'
#' The function requires a data frame with column 'node' and 'parent'.
#' Such a dataframe is generated using ggtree::fortify
#'
#' @param tree_dat dataframe describing phylogeny with internal nodes labelled by tips_to_root.R
#'
#' @return dataframe with umambiguous moximum parsimony solution to ancestral state reconstruction
#' @export
#'
#' @examples
root_2_tips <- function(tree_dat = NA, trait_var = 'trait') {

  N.tips <- sum(tree_dat$isTip)

  # First - enter trait info for unambiguous internal nodes
  # loop through rows and find internal nodes where length of options == 1
  for (i in 1:nrow(tree_dat)) {
    if (is.na(tree_dat$label[i])) {
      if (length(unlist(tree_dat$trait_opts[[i]])) == 1) {
        tree_dat[i, trait_var] <- tree_dat$trait_opts[[i]][1]
      }
    }
  }

  # Special case if root (node == parent) is ambiguous
  if (is.na(tree_dat[tree_dat$node == tree_dat$parent, trait_var]) == T) {
    parent <- tree_dat$node[tree_dat$node == tree_dat$parent]

    # identify child nodes (use setdiff to avoid getting root as parent and child)
    children <- setdiff(tree_dat$node[tree_dat$parent == parent], parent)
    c1 <- children[1]
    c2 <- children[2]

    # identify the 4 grandchild nodes
    c1_gc1 <- tree_dat$node[tree_dat$parent == c1][1]
    c1_gc2 <- tree_dat$node[tree_dat$parent == c1][2]
    c2_gc1 <- tree_dat$node[tree_dat$parent == c2][1]
    c2_gc2 <- tree_dat$node[tree_dat$parent == c2][2]

    # for each child, identify shared options in grandchildren
    shared_c1_gc <- intersect(unlist(tree_dat$trait_opts[tree_dat$node == c1_gc1]),
                              unlist(tree_dat$trait_opts[tree_dat$node == c1_gc2]))

    shared_c2_gc <- intersect(unlist(tree_dat$trait_opts[tree_dat$node == c2_gc1]),
                              unlist(tree_dat$trait_opts[tree_dat$node == c2_gc2]))
    shared_in_grandchildren <- union(shared_c1_gc, shared_c2_gc)

    # if there are no shared option in grandchildren, try union
    if (length(shared_in_grandchildren) == 0) {
      shared_c1_gc <- union(unlist(tree_dat$trait_opts[tree_dat$node == c1_gc1]),
                            unlist(tree_dat$trait_opts[tree_dat$node == c1_gc2]))

      shared_c2_gc <- union(unlist(tree_dat$trait_opts[tree_dat$node == c2_gc1]),
                            unlist(tree_dat$trait_opts[tree_dat$node == c2_gc2]))
      shared_in_grandchildren <- union(shared_c1_gc, shared_c2_gc)
    }

    # if either child of root is tip, add trait to shared in shared_in_grandchildren
    if (tree_dat$isTip[tree_dat$node == c1]) {
      shared_in_grandchildren <- c(shared_in_grandchildren,
                                   tree_dat[tree_dat$node == c1, trait_var])
    }
    if (tree_dat$isTip[tree_dat$node == c2]) {
      shared_in_grandchildren <- c(shared_in_grandchildren,
                                   tree_dat[tree_dat$node == c2, trait_var])
    }

    # create temp, subset which has trait in union of these two intersects
    if (length(shared_in_grandchildren) > 1) {
      temp <- tree_dat[which(tree_dat$trait_A %in% shared_in_grandchildren),]
    } else {
      temp <- tree_dat[which(tree_dat$trait_A == shared_in_grandchildren),]
    }

    # identify oldest node in temp, min(x), and assign to root
    root.trait <- temp[temp$x == min(temp$x), trait_var]
    tree_dat[tree_dat$node == tree_dat$parent, trait_var] <- root.trait
  }

  # Second - use algorithm to resolve ambiguous nodes
  # resolve nodes starting with root
  while (sum(is.na(tree_dat[,trait_var])) > 0) {
    # loop trough non tip rows
    for (i in (N.tips + 1):nrow(tree_dat)) {
      parent <- i

      # procede if trait for node is not NA (only root in iteration 1) otherwise skip
      if (is.na(tree_dat[tree_dat$node == parent, trait_var]) == F) {
        # record current parental trait
        parent_trait <- tree_dat[tree_dat$node == parent, trait_var]

        # identify child nodes (use setdiff to avoid getting root as parent and child)
        children <- setdiff(tree_dat$node[tree_dat$parent == parent], parent)
        c1 <- children[1]
        c2 <- children[2]

        # identify the 4 grandchild nodes
        c1_gc1 <- tree_dat$node[tree_dat$parent == c1][1]
        c1_gc2 <- tree_dat$node[tree_dat$parent == c1][2]
        c2_gc1 <- tree_dat$node[tree_dat$parent == c2][1]
        c2_gc2 <- tree_dat$node[tree_dat$parent == c2][2]

        # for each child, identify shared options in grandchildren
        shared_c1_gc <- intersect(unlist(tree_dat$trait_opts[tree_dat$node == c1_gc1]),
                                  unlist(tree_dat$trait_opts[tree_dat$node == c1_gc2]))

        shared_c2_gc <- intersect(unlist(tree_dat$trait_opts[tree_dat$node == c2_gc1]),
                                  unlist(tree_dat$trait_opts[tree_dat$node == c2_gc2]))

        # deal with child 1 if it has NA for trait
        if (is.na(tree_dat[tree_dat$node == c1, trait_var])) {
          # 1. if parent trait is an option, retain it
          if (parent_trait %in% unlist(tree_dat$trait_opts[tree_dat$node == c1])) {
            tree_dat[tree_dat$node == c1, trait_var] <- parent_trait
            # 2. if there's a single common trait among the child's children, use this
          } else if (length(shared_c1_gc) == 1) {
            tree_dat[tree_dat$node == c1, trait_var] <- shared_c1_gc
            # 3. if no single common ancestor, retain the parent state
          } else {
            tree_dat[tree_dat$node == c1, trait_var] <- parent_trait
          }
        } # deal with child 1

        # deal with child 2 if it has NA for trait
        if (is.na(tree_dat[tree_dat$node == c2, trait_var])) {
          # 1. if parent trait is an option, retain it
          if (parent_trait %in% unlist(tree_dat$trait_opts[tree_dat$node == c2])) {
            tree_dat[tree_dat$node == c2, trait_var] <- parent_trait
            # 2. if there's a single common trait among the child's children, use this
          } else if (length(shared_c2_gc) == 1) {
            tree_dat[tree_dat$node == c2, trait_var] <- shared_c2_gc
            # 3. if no single common ancestor, retain the parent state
          } else {
            tree_dat[tree_dat$node == c2, trait_var] <- parent_trait
          }
        } # deal with child 2

      } # if parent is non-NA
    } # for loop
  } # while loop

  # return phylogenetic DF with solved ancestral states
  tree_dat
}
