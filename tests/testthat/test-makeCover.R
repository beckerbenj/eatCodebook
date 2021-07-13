test_that("empty cover", {
  out <- makeCover()
  expect_equal(out[1], "\\thispagestyle{empty}")
  expect_equal(out[2], "\\vspace*{75mm}")
  expect_equal(out[3], "\\bigskip\n")
  expect_equal(out[4], "\\vfill")
  expect_equal(out[5], "Stand: \\today \\par")
  expect_equal(out[9], "\\quad")
  expect_equal(out[10], "\\vfill\n")
})

test_that("standard cover", {
  out <- makeCover(maintitle = "Study of Achievement", subtitle = "Codebook of Study of Achievement",
            authors = "Some Person", addAuthors = "With the help of some other persons",
            schriftenreihe = "Book 9 of Studies of Achievement")

  expect_equal(out[3], "\\begin{Huge}")
  expect_equal(out[4], "\\color{iqbrot} \\textbf{Study of Achievement} \\par \\medskip")
  expect_equal(out[7], "\\textbf{Codebook of Study of Achievement}\\par \\bigskip")
  expect_equal(out[10], "Some Person")
  expect_equal(out[15], "With the help of some other persons\\par"  )
})


#test_that("empty cover", {
#  out <- makeCover("q:/BT2016/BT/02_Organisation/81_StuMi_Arbeitsordner/Skalenhandbuch/Vorlagen/Grafiken/iqb-logo.jpg",
#                   maintitle = "IQB-Bildungstrend~2016",
#                   subtitle = "Skalenhandbuch zur Dokumentation der Erhebungsinstrumente in den Fächern Deutsch und Mathematik",
#                   authors = "Stefan Schipolowski, Johanna Busse, Camilla Rjosk, Nicole Mahler, Benjamin Becker & Petra Stanat",
#                   addAuthors = "Unter Mitarbeit von Felix Milles, Hannes Baukmann, Stefanie Pietz und Christin Rüdiger",
#                   schriftenreihe = "Schriftenreihe des Institutes zur Qualitätsentwicklung im Bildungswesen - Band~10",
#                   bibinfo = "Band~10 \\par \\medskip\nSchipolowski, S., Busse, J., Rjosk, C., Mahler, N., Becker, B. & Stanat, P. (2019). \textit{IQB-Bildungstrend~2016. Skalenhandbuch zur Dokumentation der Erhebungsinstrumente in den Fächern Deutsch und Mathematik.} Berlin: Humboldt-Universität zu Berlin, Institut zur Qualitätsentwicklung im Bildungswesen.")

#  varue.file <- "other_code/Varue.xlsx"
#  sheets.deckblatt <- "Deckblatt"
#  library(openxlsx)
#  deckblatt <- makeDeckblatt(varue.file=varue.file , sheet=sheets.deckblatt )
#  expect_equal(out, deckblatt)
#})
