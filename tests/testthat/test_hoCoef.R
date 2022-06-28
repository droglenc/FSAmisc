test_that("hoCoef() messages",{
  data(Mirex,package="FSA")
  ## fit some linear regression results
  lm1 <- lm(mirex~weight,data=Mirex)
  lm2 <- lm(mirex~weight+year,data=Mirex)
  # bad alt=
  expect_error(hoCoef(lm1,term=2,bo=0.1,alt="derek"),"should be one of")
  # bad term
  expect_error(hoCoef(lm1,term=-1,bo=0.1),"positive")
  expect_error(hoCoef(lm1,term=5,bo=0.1),"greater")
  expect_error(hoCoef(lm2,term=5,bo=0.1),"greater")

  ## fit some non-linear regression results
  data(Ecoli,package="FSA")
  fnx <- function(days,B1,B2,B3) {
    if (length(B1) > 1) {
      B2 <- B1[2]
      B3 <- B1[3]
      B1 <- B1[1]
    }
    B1/(1+exp(B2+B3*days))
  }
  nl1 <- nls(cells~fnx(days,B1,B2,B3),data=Ecoli,start=list(B1=6,B2=7.2,B3=-1.45))
  # bad model type
  expect_error(hoCoef(nl1,term=-1,bo=0.1),"lm")
})

