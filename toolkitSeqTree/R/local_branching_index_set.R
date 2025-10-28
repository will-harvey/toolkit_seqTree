#' Local branching index based on subset of tips
#'
#' Calculate the local branching index (LBI, Neher et al. 2014 eLife doi: 10.7554/eLife.03568).
#' This version of the function opperates by calculating the LBI using a user supplied subset
#' of all taxa/tips/terminal nodes. Branches leading towards tips excluded from this set are
#' ignored, as are branching events that only exist due to the samping of taxa outside the
#' set.
#'
#' For each node in a phylogeny, the LBI is the exponentially weighted tree length
#' surrounding a node. Higher LBI indicated rapid branching and clade expansion.
#'
#' The function requires a data frame with column 'node' and 'parent'.
#' Such a dataframe is generated using ggtree::fortify
#'
#' @param tree_dat Data frame describing tree structure
#' @param tree Phylo class object for tree associated with tree_dat
#' @param taxa Vector of taxon labels forming subset
#' @param nodes Optionally subset can be supplied as vector of terminal nodes
#' @param use_nodes Logical should nodes be used instead of taxa labels
#' @param scale Scaling parameter to be passed to local_branching_index function
#'
#' @return tree_dat with additional column for lbi
#' @export
#'
local_branching_index_set <- function(tree_dat = NA, tree = NA, taxa = NA,
                                      nodes = NA, use_nodes = F, scale = NA) {

  if ('branch.length' %in% names(tree_dat) != 1) {
    stop('Error: "branch.length" is not present as a single column in tree_dat dataframe')
  }

  # Check supplied labels/nodes are in dataset/are tip nodes
  if (use_nodes == F) {
    # check all input taxa are in the tree df
    if (sum(taxa %in% tree_dat$label) != length(taxa)) {
      cat('Absent input taxa:\n')
      cat(setdiff(taxa, tree_dat$label), '\n')
      stop('Input taxon label absent from tree_dat')
    }

    # if list of taxa labels are supplied, get nodes
    nodes <- tree_dat$node[tree_dat$label %in% taxa]
  } else {
    # check input 'nodes' are all tips
    if (sum(nodes %in% tree_dat$node[tree_dat$isTip]) != length(nodes)) {
      cat('Supplied node not in tree_dat terminal nodes:\n')
      cat(setdiff(nodes, tree_dat$node[tree_dat$isTip]), '\n')
      stop('Input node absent from tree_dat terminal nodes')
    }
  }


  #### Subtree
  # Get subtree based on supplied set of tips using ape::drop.tip
  all_tip_nodes <- tree_dat$node[tree_dat$isTip == T]
  nodes_to_drop <- setdiff(all_tip_nodes, nodes)

  subtree <- ape::drop.tip(tree, tip = nodes_to_drop, trim.internal = T)
  subtree_dat <- ggtree::fortify(subtree)

  # add lbi to subtree_dat
  subtree_dat <- local_branching_index(subtree_dat, scale = scale)

  # add column with descendants of internal nodes in subtree_dat
  subtree_dat$descendants <- list(NA)
  for (i in 1:nrow(subtree_dat)) {
    if (subtree_dat$isTip[i] == F) {
      # node_descendants gets the node numbers for descendants
      descendants <- as.vector(node_descendants(subtree_dat,
                                                subtree_dat$node[i],
                                                tips_only = T))

      # get matching labels to fill 'descendants' column
      subtree_dat$descendants[[i]] <- subtree_dat$label[subtree_dat$node %in% descendants]
    } # if internal node
  } # loop rows in subtree_dat


  #### Map subtree LBI to tree_dat

  ## Define internal nodes as ancestors of tip set of interest or not
  tip_subset_ancestors <- c()
  for (i in 1:length(nodes)) {
    tip_ancestors <- toolkitSeqTree::node_ancestors(tree_dat,
                                                    node_id = nodes[i],
                                                    keep_target_node = T)
    tip_subset_ancestors <- union(tip_subset_ancestors, tip_ancestors)
  }
  # tip_subset_ancestors used in loop below


  tree_dat$lbi <- NA
  for (i in 1:nrow(tree_dat)) {
    # only get LBI for nodes in ancestors of the input tip subset
    if (tree_dat$node[i] %in% tip_subset_ancestors) {

      # If a tip, get LBI matched by virus label
      if (tree_dat$isTip[i] == T) {
        curr_label <- tree_dat$label[i]
        x <- subtree_dat$lbi[subtree_dat$label %in% c(curr_label)]
        tree_dat$lbi[i] <- x
      }

      # If non-tip, get LBI matched by selection of descendant labels
      if (tree_dat$isTip[i] == F) {
        # ID list of labels descended from internal node
        descendants <- node_descendants(tree_dat, tree_dat$node[i],
                                        tips_only = T)
        descendants_labs <- tree_dat$label[tree_dat$node %in% descendants &
                                             tree_dat$node %in% tip_subset_ancestors]

        # For each set of descendants_labs assoc. a tree_dat node, look
        # through subtree_dat to find matching row
        for (j in 1:nrow(subtree_dat)) {
          if (subtree_dat$isTip[j] == F) {
            subtree_set <- subtree_dat$descendants[[j]][]

            # If the intersect is the same length as descendants_labs its a good match
            if (length(intersect(descendants_labs, subtree_set)) ==
                length(descendants_labs)) {
              # so copy across the LBI from subtree_dat
              tree_dat$lbi[i] <- subtree_dat$lbi[j]
            }
          }
        }

      } # If non-tip, get LBI matched..

    } # only get LBI for nodes in ancestors..
  }

  tree_dat
}

