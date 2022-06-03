

test_that("check lit info", {
  inp2 <- inp1 <- inp <- createLitInfo()
  expect_silent(check_litInfo(inp))

  inp1[1, 1:2] <- c("la", "la")
  expect_error(check_litInfo(inp1),
               "There are missing entries in 'in_Literaturverzeichnis' in 'litInfo'.")

  inp2[1, 1:3] <- c("la", "la", "jes")
  expect_error(check_litInfo(inp2),
               "There are invalid entries in 'in_Literaturverzeichnis' in 'litInfo'. Valid entries are 'yes/ja' or 'nein/no'.")
})
