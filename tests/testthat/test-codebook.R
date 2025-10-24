

file <- system.file("extdata", "example1_clean.sav", package = "eatCodebook")
gd   <- eatGADS::import_spss(file)

## Descriptives
inputForDescr <- createInputForDescriptives(gd)
descr  <- calculateDescriptives(gd, inputForDescr)

miss_final <- getMissings(system.file("extdata", "example_miss.xlsx", package = "eatCodebook"))
varInfo_final <- getVarInfo(system.file("extdata", "example_varInfo.xlsx", package = "eatCodebook"))
varInfo_final2 <- inferLayout(varInfo_final, GADSdat = gd, inputForDescriptives = inputForDescr)
struc_final <- getStructure(system.file("extdata", "example_struc.xlsx", package = "eatCodebook"))
scaleInfo_final <- getScaleInfo(system.file("extdata", "example_scaleInfo.xlsx", package = "eatCodebook"))

register_final <- getRegister(system.file("extdata", "example_register.xlsx", package = "eatCodebook"))
litInfo_final <- getLitInfo(system.file("extdata", "example_litInfo.xlsx", package = "eatCodebook"))
chapters_final <- getChapters(system.file("extdata", "example_chapters.xlsx", package = "eatCodebook"))
cover <- makeCover(logoFile = NULL,
                   maintitle = "Study of Achievement",
                   subtitle = "Codebook of Study of Achievement",
                   authors = "Some Person",
                   addAuthors = "With the help of some other persons",
                   schriftenreihe = "Book 9 of Studies of Achievement",
                   bibinfo = "test")
# --------------------------------------------------
abbr <- makeAbbrList(system.file("extdata", "example_abbrList.xlsx", package = "eatCodebook"))
lit <- makeLit(litInfo_final)
suppressMessages(hint <- makeBGM(varInfo_final))
meta_final <- makeMetadata(system.file("extdata", "example_meta.xlsx", package = "eatCodebook"))

# --------------------------------------------------
# try to hotfix (maybe kennwerte needs a data.frame sometimes?) -> fix this after JB has fixed the example with Felix's syntax
descr2 <- descr
descr2$skala1[[2]] <- as.data.frame(descr2$skala1[[2]])


test_that("normal codebook", {
  suppressMessages(latex_skript <- codebook(varInfo = varInfo_final2, missings = miss_final, struc = struc_final,
                           scaleInfo = scaleInfo_final, register = register_final, dat = eatGADS::extractData(gd),
                           Kennwertedatensatz = descr2, chapters = chapters_final,
                           deckblatt = cover, intro = "", literatur = lit, abkuerzverz = abbr, hintmod = hint,
                           lastpage = ""))

  expect_equal(latex_skript[1], "\\documentclass[paper=a4, hidelinks, twoside=false, numbers=noenddot]{scrbook}")
  expect_equal(latex_skript[574], "\\end{document}")
})

test_that("codebook without register", {
  suppressMessages(latex_skript <- codebook(varInfo = varInfo_final2, missings = miss_final, struc = struc_final,
                                            scaleInfo = scaleInfo_final, dat = eatGADS::extractData(gd),
                                            Kennwertedatensatz = descr2, chapters = chapters_final,
                                            deckblatt = cover, intro = "", literatur = lit, abkuerzverz = abbr, hintmod = hint,
                                            lastpage = ""))

  expect_equal(latex_skript[1], "\\documentclass[paper=a4, hidelinks, twoside=false, numbers=noenddot]{scrbook}")
  expect_equal(latex_skript[537], "\\end{document}")
})
