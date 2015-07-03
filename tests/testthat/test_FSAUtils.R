context("Tests changesPos() utility")

# ############################################################
# changesPos
# ############################################################
test_that("changesPos() error messages",{
  ## check error messages
  expect_error(changesPos(numeric(0)),"length")
  expect_error(changesPos(1,include.first=FALSE),"include.first")
  expect_error(changesPos(matrix(1:4)),"vector")
  expect_error(changesPos(data.frame(x=1:4)),"vector")
})

test_that("changesPos() calculations",{
  expect_equal(changesPos(1:4),1:4)
  expect_equal(changesPos(c(1:2,1:2)),1:4)
  expect_equal(changesPos(c(1,2,2,1)),c(1,2,4))
  expect_equal(changesPos(c(1,2,2,1),include.first=FALSE),c(2,4))
  expect_equal(changesPos(c(1,1,1,1)),1)
  expect_equal(changesPos(c(1,1,1,1),include.first=FALSE),numeric(0))
})
