library(quanteda)
context("plot functions")

mini_corpus <- head(data_corpus_inaugural, n = 10)
mini_corpus <- split_segments(mini_corpus)
dtm <- dfm(mini_corpus, remove = stopwords("en"), tolower = TRUE, remove_punct = TRUE)
dtm <- dfm_trim(dtm, min_termfreq = 3)

res <- rainette(dtm, k = 5, min_uc_size = 5, min_members = 2, verbose = TRUE)

plot1 <- rainette_plot(res, dtm)
plot2 <- rainette_plot(res, dtm, free_x = TRUE)
plot3 <- rainette_plot(res, dtm, measure = "lr")
plot4 <- rainette_plot(res, dtm, k = 3)
plot5 <- rainette_plot(res, dtm, k = 4, n_terms = 20, font_size = 9)


vdiffr::expect_doppelganger("Base rainette_plot", plot1)
vdiffr::expect_doppelganger("Base rainette_plot with free_x", plot2)
vdiffr::expect_doppelganger("Base rainette_plot measure='lr'", plot3)
vdiffr::expect_doppelganger("Base rainette_plot with k", plot4)
vdiffr::expect_doppelganger("Base rainette_plot with k, n_terms and font_size", plot5)
