# KGExplorer 0.99.04

## Bug fixes
* `test-get_ontology_levels`
  - Check for range rather than fixed values.
* `filter_ontology`
  - Move `terms` processing block to after check for character, as appropriate.
* `get_ontology_dict`
  - Add error handling for missing `alternative_terms` when
  `include_alternative_terms=TRUE`.


# KGExplorer 0.99.03

## New features

* `map_ontology_terms`: 
  - Can now recognize `alternative_terms` slot in simona object (https://github.com/jokergoo/simona/issues/6)
  - Automatically uses "short_id" when "id" is set, as the "id" column can sometimes contain long ID formats (http://purl.obolibrary.org/obo/MONDO_0100229 vs. MONDO:0100229)

# KGExplorer 0.99.01

## Bug fixes

- Fix `get_ontology_dict` data.table construction.

# KGExplorer 0.99.0

## New features
 
* Added a `NEWS.md` file to track changes to the package.

## Bug fixes

* Removed Dockerfile and switched to using `rworkflows`.
