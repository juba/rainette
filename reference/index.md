# Package index

## Simple clustering

Simple clustering computation and visualisation

- [`rainette()`](https://juba.github.io/rainette/reference/rainette.md)
  : Corpus clustering based on the Reinert method - Simple clustering
- [`cutree_rainette()`](https://juba.github.io/rainette/reference/cutree_rainette.md)
  : Cut a rainette result tree into groups of documents
- [`rainette_plot()`](https://juba.github.io/rainette/reference/rainette_plot.md)
  : Generate a clustering description plot from a rainette result
- [`rainette_explor()`](https://juba.github.io/rainette/reference/rainette_explor.md)
  : Shiny gadget for rainette clustering exploration

## Double clustering

Double clustering computation and visualisation

- [`rainette2()`](https://juba.github.io/rainette/reference/rainette2.md)
  : Corpus clustering based on the Reinert method - Double clustering
- [`cutree_rainette2()`](https://juba.github.io/rainette/reference/cutree_rainette2.md)
  : Cut a rainette2 result object into groups of documents
- [`rainette2_complete_groups()`](https://juba.github.io/rainette/reference/rainette2_complete_groups.md)
  : Complete groups membership with knn classification
- [`rainette2_plot()`](https://juba.github.io/rainette/reference/rainette2_plot.md)
  : Generate a clustering description plot from a rainette2 result
- [`rainette2_explor()`](https://juba.github.io/rainette/reference/rainette2_explor.md)
  : Shiny gadget for rainette2 clustering exploration

## Clustering statistics

Functions used to get clustering statistics

- [`rainette_stats()`](https://juba.github.io/rainette/reference/rainette_stats.md)
  : Generate cluster keyness statistics from a rainette result

## Corpus importation and segmentation

Functions to import or segment a textual corpus

- [`import_corpus_iramuteq()`](https://juba.github.io/rainette/reference/import_corpus_iramuteq.md)
  : Import a corpus in Iramuteq format
- [`split_segments()`](https://juba.github.io/rainette/reference/split_segments.md)
  : Split a character string or corpus into segments
- [`merge_segments()`](https://juba.github.io/rainette/reference/merge_segments.md)
  : Merges segments according to minimum segment size

## Documents-clusters tables

Functions to describe relationships between documents and clusters when
clustering on a segmented corpus

- [`clusters_by_doc_table()`](https://juba.github.io/rainette/reference/clusters_by_doc_table.md)
  : Returns the number of segment of each cluster for each source
  document
- [`docs_by_cluster_table()`](https://juba.github.io/rainette/reference/docs_by_cluster_table.md)
  : Returns, for each cluster, the number of source documents with at
  least n segments of this cluster

## Utility functions

Functions used internally for computation

- [`cluster_tab()`](https://juba.github.io/rainette/reference/cluster_tab.md)
  : Split a dtm into two clusters with reinert algorithm
- [`cutree()`](https://juba.github.io/rainette/reference/cutree.md) :
  Cut a tree into groups
- [`order_docs()`](https://juba.github.io/rainette/reference/order_docs.md)
  : return documents indices ordered by CA first axis coordinates
- [`select_features()`](https://juba.github.io/rainette/reference/select_features.md)
  : Remove features from dtm of each group base don cc_test and tsj
- [`switch_docs()`](https://juba.github.io/rainette/reference/switch_docs.md)
  : Switch documents between two groups to maximize chi-square value
