context("import Iramuteq corpus")

text <- "**** *id_01 *sexe_m *age_18 *prof_ouvrier
-*t1
Donec placerat quis quam ac interdum. Cras tempus eros et laoreet dictum. Morbi ac dapibus magna.
-*t2
Vivamus dictum, nisi et suscipit fermentum, tortor nulla tempor quam, a ullamcorper elit erat nec arcu.
**** *id_02 *sexe_f  *prof_agent_dentretien   *age_
-*t1
Vestibulum vitae ante eget justo maximus luctus. Nulla facilisi. 

Integer tincidunt sed sapien mattis euismod. Proin sed massa neque. 
-*t2
Nam nunc neque, fringilla vel pharetra sit amet, posuere a nibh. 

**** *id_03 *sexe_f *age_26
-*t3
Maecenas semper dictum nisi. 

Sed dolor urna, fringilla sed justo vel, fermentum dictum magna. 

-*t4
Vestibulum mollis, ligula in semper vulputate, mauris diam aliquam est, ut placerat sem felis a nibh."

con <- textConnection(text)
corpus <- import_corpus_iramuteq(con, id_var = "id", thematics = "remove")
close(con)
con <- textConnection(text)
corpus_split_id <- import_corpus_iramuteq(con, id_var = "id", thematics = "split")
close(con)
con <- textConnection(text)
corpus_split <- import_corpus_iramuteq(con, thematics = "split")
close(con)

test_that("metadata imported correctly", {
  expect_equal(docvars(corpus, "id"), c("01", "02", "03"))
  expect_equal(docvars(corpus, "sexe"), c("m", "f", "f"))
  expect_equal(docvars(corpus, "age"), c("18", "", "26"))
  expect_equal(docvars(corpus, "prof"), c("ouvrier", "agent_dentretien", NA))
  expect_equal(docnames(corpus), docvars(corpus, "id"))
})

test_that("texts imported correctly", {
  expect_equal(ndoc(corpus), 3)
  expect_equal(texts(corpus)[["02"]], "Vestibulum vitae ante eget justo maximus luctus. Nulla facilisi. \nInteger tincidunt sed sapien mattis euismod. Proin sed massa neque. \nNam nunc neque, fringilla vel pharetra sit amet, posuere a nibh. ")
})


test_that("thematics splitted correctly", {
  expect_equal(ndoc(corpus_split), 6)
  expect_equal(ndoc(corpus_split_id), 6)
  expect_equal(texts(corpus_split_id)[["02_2"]], "Nam nunc neque, fringilla vel pharetra sit amet, posuere a nibh. ")
  expect_equal(docnames(corpus_split), paste0("text", 1:6))
  expect_equal(docnames(corpus_split_id), c("01_1", "01_2", "02_1", "02_2", "03_1", "03_2"))
  expect_equal(names(docvars(corpus_split)), names(docvars(corpus_split_id)))
  expect_equal(docvars(corpus_split, "age"), c("18", "18", "", "", "26", "26"))
  expect_equal(docvars(corpus_split, "age"), docvars(corpus_split_id, "age"))
  expect_equal(docvars(corpus_split, "thematics"), docvars(corpus_split_id, "thematics"))
  expect_equal(docvars(corpus_split, "thematics"), c("t1", "t2", "t1", "t2", "t3", "t4"))
})

text <- "****
-*t1
Donec placerat quis quam ac interdum.
-*t2
Vivamus dictum, nisi et suscipit fermentum, tortor nulla tempor quam, a ullamcorper elit erat nec arcu.
****
-*t1
Vestibulum vitae ante eget justo maximus luctus. 
-*t2
Nam nunc neque, fringilla vel pharetra sit amet, posuere a nibh."

con <- textConnection(text)
corpus_without_meta <- import_corpus_iramuteq(con, id_var = "id", thematics = "remove")
close(con)
con <- textConnection(text)
corpus_without_meta_split <- import_corpus_iramuteq(con, id_var = "id", thematics = "split")
close(con)


test_that("corpus without meta is ok", {
  expect_equal(ndoc(corpus_without_meta), 2)
  expect_equal(texts(corpus_without_meta)[[2]], c("Vestibulum vitae ante eget justo maximus luctus. \nNam nunc neque, fringilla vel pharetra sit amet, posuere a nibh."))
  expect_equal(dim(docvars(corpus_without_meta)), c(2, 0))
})

test_that("splitted corpus without meta is ok", {
  expect_equal(dim(docvars(corpus_without_meta_split)), c(4, 1))
  expect_equal(docvars(corpus_without_meta_split, "thematics"), c("t1","t2","t1","t2"))
  expect_equal(ndoc(corpus_without_meta_split), 4)
  expect_equal(texts(corpus_without_meta_split)[[3]], "Vestibulum vitae ante eget justo maximus luctus. ")
})

text <- "****
Donec placerat quis quam ac interdum.
**** *foo_1
Vestibulum vitae ante eget justo maximus luctus.
****
Donec placerat quis quam ac interdum.
**** *bar_bar
Vestibulum vitae ante eget justo maximus luctus."

con <- textConnection(text)
corpus_with_partial_meta <- import_corpus_iramuteq(con, id_var = "id", thematics = "remove")
close(con)

test_that("corpus with partial meta is ok", {
  expect_equal(texts(corpus_with_partial_meta)[[2]], "Vestibulum vitae ante eget justo maximus luctus.")
  expect_equal(ndoc(corpus_with_partial_meta), 4)
  expect_equal(dim(docvars(corpus_with_partial_meta)), c(4, 2))
  expect_equal(docvars(corpus_with_partial_meta, "foo"), c(NA, "1", NA, NA))
  expect_equal(docvars(corpus_with_partial_meta, "bar"), c(NA, NA, NA, "bar"))
})

test_that("error if f is not a valid file path", {
  skip_on_ci()
  expect_error(expect_warning(import_corpus_iramuteq("doesnt/exist.txtt")))
  expect_error(import_corpus_iramuteq(mtcars))
})

