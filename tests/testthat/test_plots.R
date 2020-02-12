library(quanteda)
context("plot functions")

skip("Skipping plots")
skip_on_ci()
skip_on_cran()

mini_corpus <- head(data_corpus_inaugural, n = 10)
mini_corpus <- split_segments(mini_corpus)
dtm <- dfm(mini_corpus, remove = stopwords("en"), tolower = TRUE, remove_punct = TRUE)
dtm <- dfm_trim(dtm, min_termfreq = 3)

res <- rainette(dtm, k = 5, min_uc_size = 5, min_split_members = 2, verbose = TRUE)

plot1 <- rainette_plot(res, dtm)
plot2 <- rainette_plot(res, dtm, free_scales = TRUE)
plot3 <- rainette_plot(res, dtm, measure = "lr")
plot4 <- rainette_plot(res, dtm, k = 3, show_negative = FALSE)
plot5 <- rainette_plot(res, dtm, k = 4, n_terms = 20, text_size = 9)
plot6 <- rainette_plot(res, dtm, type = "cloud", text_size = 6)
plot7 <- rainette_plot(res, dtm, k = 4, type = "cloud", n_terms = 10, text_size = 9, measure = "lr")

vdiffr::expect_doppelganger("Base rainette_plot", plot1)
vdiffr::expect_doppelganger("Base rainette_plot with free_scales", plot2)
vdiffr::expect_doppelganger("Base rainette_plot measure='lr'", plot3)
vdiffr::expect_doppelganger("Base rainette_plot with k and without negative", plot4)
vdiffr::expect_doppelganger("Base rainette_plot with k, n_terms and font_size", plot5)
vdiffr::expect_doppelganger("Base rainette_plot of type 'cloud'", plot6)
vdiffr::expect_doppelganger("Base rainette_plot of type 'cloud' and arguments", plot7)