#' Phylogenetic clusters
#'
#' Given a data frame describing tree structure and branch lengths and a
#' defined threshold, determine clusters on phylogeny.
#'
#' @param tree_dat Data frame describing tree structure
#' @param t Threshold value for branch lengths defining clusters
#' @param suppress_singletons Logical - whether or not to require greater isolation of singletons
#'
#' @returns Input data frame with addition of column with clusters
#' @export
#'
phylo_clusters <- function(tree_dat, t = NA, w = 1, suppress_singletons = TRUE,
                           suppress_doubles = FALSE) {

  ### input check
  required <- c('node', 'parent', 'isTip', 'branch.length')
  tree_dat_names <- names(tree_dat)
  if (length(intersect(required, tree_dat_names)) != length(required)) {
    stop('Error: tree_dat does not have full set of required column names:\n',
         paste(required, collapse = ', '))
  }
  ###

  tree_dat$clust <- NA

  # Function generates novel 4-letter cluster label
  new_code <- function(tree_dat, var = 'clust') {
    unique <- F
    while(unique == F) {
      code <- paste(sample(LETTERS, 4, replace = T), collapse = '')
      if (!code %in% unique(tree_dat[['clust']])) {
        unique <- T
      }
    }
    code
  }

  ## calculate depths of nodes and a correction based on this
  tree_dat <- tree_node_depth(tree_dat)
  # tree_dat$depth <- 1 - tree_dat$depth^(1/3) # / sum(tree_dat$branch.length) # max(tree_dat$depth, na.rm = T) # sum(tree_dat$branch.length) # max(tree_dat$depth, na.rm = T)
  tree_dat$depth <- 1 - (tree_dat$depth / sum(tree_dat$branch.length))^(1/w)



  ### Solve clusters for root and children
  # set state for root
  root <- tree_dat$node[tree_dat$parent == tree_dat$node]
  tree_dat$clust[tree_dat$node == root] <- new_code(tree_dat)

  # ID children and contrast (distance betweeen them)
  children <- tree_dat$node[tree_dat$parent == root]
  children <- setdiff(children, root)
  root_contrast <- sum(tree_dat$branch.length[tree_dat$node %in% children])
  # If contrast > t, one child gets new cluster, else both children get root cluster
  if (root_contrast > t) {
    tree_dat$clust[tree_dat$node == children[1]] <- tree_dat$clust[tree_dat$node == root]
    tree_dat$clust[tree_dat$node == children[2]] <- new_code(tree_dat)
  } else {
    tree_dat$clust[tree_dat$node == children[1]] <- tree_dat$clust[tree_dat$node == root]
    tree_dat$clust[tree_dat$node == children[2]] <- tree_dat$clust[tree_dat$node == root]
  }
  root_children <- children

  ### Solve clusters for remaining nodes
  # loop to iterate while NA still exist in tree_dat$clust
  while (sum(is.na(tree_dat$clust)) > 0) {
    # work through nodes, looking for NA nodes that can be solved
    # nodes below root will be solved first
    for (i in 1:nrow(tree_dat)) {
      # identify unsolved node ...
      if (is.na(tree_dat$clust[i]) == T) {
        unsolved <- tree_dat$node[i]
        parent <- tree_dat$parent[i]

        # ... with solved (non-NA) parent node
        if (is.na(tree_dat$clust[tree_dat$node == parent]) == F) {
          children <- tree_dat$node[tree_dat$parent == parent]
          children <- setdiff(children, parent) # special case: removes root from children
          sibling <- setdiff(children, unsolved)

          dist_child_1 <- tree_dat$branch.length[tree_dat$node == unsolved]
          dist_child_2 <- tree_dat$branch.length[tree_dat$node == sibling]
          dist_parent <- tree_dat$branch.length[tree_dat$node == parent]
          # if the parent node is a child of the root, replace dist_parent with contrast
          if (parent %in% root_children) {
            dist_parent <- root_contrast
          }

          isolation <- dist_child_1 + dist_child_2 + dist_parent

          # depth of parent node impacts calculation
          depth_parent <- tree_dat$depth[tree_dat$node == parent]
          isolation_t <- (3*t*depth_parent)

          if (dist_child_1 > (t*depth_parent) & isolation > isolation_t) {
            tree_dat$clust[tree_dat$node == unsolved] <- new_code(tree_dat)
          } else {
            tree_dat$clust[tree_dat$node == unsolved] <- tree_dat$clust[tree_dat$node == parent]
          }

          if (dist_child_2 > (t*depth_parent) & isolation > isolation_t) {
            tree_dat$clust[tree_dat$node == sibling] <- new_code(tree_dat)
          } else {
            tree_dat$clust[tree_dat$node == sibling] <- tree_dat$clust[tree_dat$node == parent]
          }


        } # ... with solved (non-NA) parent node
      } # identify unsolved node ...
    } # for loop running through nodes
  } # while loop

  ### Singletons
  # Option to merge singletons with a nearby node if stricter criteria not met
  if (suppress_singletons == TRUE) {
    t_single <- t#*2
    for (i in 1:sum(tree_dat$isTip == T)) {

      tip <- tree_dat$node[i]
      parent <- tree_dat$parent[i]

      cluster_tip <- tree_dat$clust[tree_dat$node == tip]
      cluster_parent <- tree_dat$clust[tree_dat$node == parent]

      # ID singletons - tips that don't match parent cluster
      if (cluster_tip != cluster_parent) {

        # id sibling
        children <- tree_dat$node[tree_dat$parent == parent]
        children <- setdiff(children, parent) # special case: removes root from children
        sibling <- setdiff(children, tip)

        # get distances above parent and from parent to sibling
        dist_child_2 <- tree_dat$branch.length[tree_dat$node == sibling]
        dist_parent <- tree_dat$branch.length[tree_dat$node == parent]
        # if the parent node is a child of the root, replace dist_parent with contrast
        if (parent %in% root_children) {
          dist_parent <- root_contrast
        }

        # either nearby node is close, change singleton
        # if parent-sib distance < t and < parent-gparent dist
        if (dist_child_2 < dist_parent & dist_child_2 < t_single) {
          cluster_sibling <- tree_dat$clust[tree_dat$node == sibling]
          tree_dat$clust[tree_dat$node == tip] <- cluster_sibling
          tree_dat$clust[tree_dat$node == parent] <- cluster_sibling
          # if parent-gparent dist < t and < parent-sib dist
        } else if (dist_parent < dist_child_2 & dist_parent < t_single) {
          cluster_parent <- tree_dat$clust[tree_dat$node == parent]
          tree_dat$clust[tree_dat$node == tip] <- cluster_parent
        }

      }

    } # loop through tips
  } # suppress singletons


  ### Doubles
  # Option to merge singletons with a nearby node if stricter criteria not met
  if (suppress_doubles == TRUE) {
    t_double <- t#*2
    for (i in tree_dat$node[tree_dat$isTip == F]) {
      # id nodes with two tips as children


      node <- tree_dat$node[i]
      parent <- tree_dat$parent[i]
      descendants <- node_descendants(tree_dat, node)

      # Doubles will have 2 descendant nodes only
      if (length(descendants) == 2) {
        cluster_node <- tree_dat$clust[tree_dat$node == node]
        cluster_parent <- tree_dat$clust[tree_dat$node == parent]

        # ID singletons - tips that don't match parent cluster
        if (cluster_node != cluster_parent) {

          # id sibling
          children <- tree_dat$node[tree_dat$parent == parent]
          children <- setdiff(children, parent) # special case: removes root from children
          sibling <- setdiff(children, node)

          # get distances above parent and from parent to sibling
          dist_child_2 <- tree_dat$branch.length[tree_dat$node == sibling]
          dist_parent <- tree_dat$branch.length[tree_dat$node == parent]
          # if the parent node is a child of the root, replace dist_parent with contrast
          if (parent %in% root_children) {
            dist_parent <- root_contrast
          }

          # either nearby node is close, change singleton
          # if parent-sib distance < t and < parent-gparent dist
          if (dist_child_2 < dist_parent & dist_child_2 < t_double) {
            cluster_sibling <- tree_dat$clust[tree_dat$node == sibling]
            tree_dat$clust[tree_dat$node == node] <- cluster_sibling
            tree_dat$clust[tree_dat$node == parent] <- cluster_sibling
            # also make the two tips below the node the same cluster
            tree_dat$clust[tree_dat$parent == node] <- cluster_sibling
            # if parent-gparent dist < t and < parent-sib dist
          } else if (dist_parent < dist_child_2 & dist_parent < t_double) {
            cluster_parent <- tree_dat$clust[tree_dat$node == parent]
            tree_dat$clust[tree_dat$node == node] <- cluster_parent
            # also make the two tips below the node the same cluster
            tree_dat$clust[tree_dat$node == node] <- cluster_parent
          }

        } # is node cluster != parent cluster
      } # is descendants length 2

    } # loop through non-tip nodes
  } # suppress doubles

  tree_dat
}
