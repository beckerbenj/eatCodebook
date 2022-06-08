


test_that("simple test", {
  out <- embedLatexSnippet("test")
  expect_equal(out[1], "\\documentclass[paper=a4, hidelinks, twoside=false, numbers=noenddot]{scrbook}")
  expect_equal(out[length(out)], "\\end{document}")
})
