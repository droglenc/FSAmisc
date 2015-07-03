context("Walford and Chapman Plots")
test_that("walfordPlot() and chapmanPlot() errors and warnings",{
  ## Get some data for the following attempts
  if (require(fishmethods)) {
    data(Kimura)
    ## Two variables on LHS
    expect_error(walfordPlot(length+age~age,data=Kimura))
    expect_error(chapmanPlot(length+age~age,data=Kimura))
    ## Two variables on RHS
    expect_error(walfordPlot(length~age+sex,data=Kimura))
    expect_error(chapmanPlot(length~age+sex,data=Kimura))
    ## LHS is a factor
    expect_error(walfordPlot(sex~age,data=Kimura))
    expect_error(chapmanPlot(sex~age,data=Kimura))
    ## RHS is a factor
    expect_error(walfordPlot(length~sex,data=Kimura))
    expect_error(chapmanPlot(length~sex,data=Kimura))
  }
})
