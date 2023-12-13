#' @title Utility functions
#' 
#' @description
#' Functions to support other functions.
#' @family utils_ 
#' @returns Merged data.
#' 
#' @name utils_
NULL

#' @title Query functions
#' 
#' @description
#' Functions to query specific subset of data via dedicated APIs.
#' @family query_ 
#' @returns Queried data.
#' 
#' @name query_
NULL


#' @title Plot functions
#' 
#' @description
#' Functions to plot data.
#' @param ont An ontology of class \link[simona]{ontology_DAG}.
#' @param types Types of graph to produce. Can be one or more.
#' @param ... Additional arguments passed to plot-specific functions.
#' @import simona
#' @family plot_ 
#' @returns A named list containing the plot and the data.
#' 
#' @name plot_
NULL

#' @title Get functions
#' 
#' @param dat Input \link[data.table]{data.table}.
#' @param force_new Create a new file instead of using any cached files.
#' @param save_dir Directory to save a file to.
#' @param filters A named list, where each element in the list is the name of 
#' a column in the data, and the vector within each element represents the 
#' values to include in the final data.
#' @param maps A list of paired to/from types to filter Monarch association 
#' files by. For example, \code{list(c("gene","disease"))} will return any 
#'  files that contains gene-disease associations.
#' Passes to \link{get_monarch_files}.
#' @param queries A list of free-form substring queries to filter files by 
#' (using any column in the metadata). 
#' For example, \code{list("gene_disease","variant_disease")} will return any 
#'  files that contain either of the substrings 
#'  "gene_disease" or "variant_disease".
#' Passes to \link{get_monarch_files}.
#' @param domain Web domain to search for Monarch files.
#' @param subdir Subdirectory path to search for Monarch files within
#'  \code{domain}.
#' @param rbind If \code{TRUE}, rbinds all \link[data.table]{data.table}s 
#' together. Otherwise, returns a named list of separated 
#' \link[data.table]{data.table}s. 
#' 
#' @inheritParams data.table::merge.data.table
#' @description
#' Functions to get data resources.
#' @family get_ 
#' @returns Data..
#' 
#' @name get_
#' @import data.table
NULL


#' @title Map functions
#' 
#' @description
#' Functions to map IDs across ontologies/databases.
#' @param top_n Top number of mappings to return per \code{top_by} grouping.
#' Set to \code{NULL} to skip this step.
#' @param top_by Grouping columns when selecting \code{top_n} rows per grouping.
#' Can be a character vector of one or more column names. 
#' @param map_types Mapping types to include.
#' @param map_to Mapping outputs to include
#'  (from Mondo IDs to another database's IDs).
#' @param map_type_order The order in which \code{map_types} will be prioritised
#' when filtering the \code{top_n} rows by groupings.
#' @param input_col Column name of input IDs.
#' @param output_col Column name of output IDs.
#' @param to Character vector of database(s) to map IDs to.
#' When not \code{"MONDO"}, can supply multiple alternative databases to map to
#'  (e.g. \code{c("OMIM","Orphanet","DECIPHER")}).
#' @param add_name Logical, if TRUE, add MONDO name column.
#' @param add_definitions logical, if TRUE, add MONDO definition column.  
#' @param map_orthologs Add gene-level data. 
#' @inheritParams data.table::merge.data.table
#' @family map_ 
#' @returns Mapped data.
#' 
#' @name map_
#' @import data.table
NULL

#' @title Link functions
#' 
#' @description
#' Functions to merge data resources.
#' @inheritParams get_
#' @inheritParams map_
#' @inheritParams get_monarch_
#' @family link_ 
#' @returns Merged data.
#' 
#' @name link_
NULL

#' @title Map uPheno functions
#' 
#' @description
#' Functions to map IDs across uPheno data resources. 
#' 
#' @family map_ 
#' @returns Mapped data.
#' 
#' @name map_upheno_
NULL


#' @title Filter functions
#' 
#' @description
#' Functions to filter objects
#' @param terms Term IDs to include. Can alternatively be an integer, 
#' which will be used to randomly sample N terms from the data.
#' @param remove_terms Character vector of term IDs to exclude.
#' @param use_simona Use \link[simona]{dag_filter} to filter terms.
#' @import simona
#' @family filter_ 
#' @returns Converted data. 
#' @name filter_
NULL

#' @title Converter functions
#' 
#' @description
#' Functions to convert one object type to another. 
#' @param to A character string specifying the format to convert to.
#' @param as_sparse If TRUE, return a sparse matrix where possible.
#' @inheritParams plot_
#' @inheritParams filter_
#' @import tidygraph
#' @import simona
#' @family convert_ 
#' @returns Converted data. 
#' @name convert_
NULL


#' @title Add functions
#' 
#' @description
#' Functions to add extra metadata to an ontology or data.table object. 
#' @import simona
#' @family add_ 
#' @returns Added data. 
#' @name add_
NULL




