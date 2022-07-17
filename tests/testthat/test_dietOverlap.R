context("Tests of dietOverlap")

## Create testing data
set.seed(123)
test.names <- c("Ephemeroptera", "Odonata", "Diptera")
speciesA <- sample(0:250, 3, replace=TRUE) #158,206,178
speciesB <- sample(0:250, 3, replace=TRUE) #13,194,169

N.speciesA <- 75
N.speciesB <- 90
num.speciesA <- sample(0:N.speciesA, 3, replace=TRUE) #49,42,13
num.speciesB <- sample(0:N.speciesB, 3, replace=TRUE) #24,89,90

## Morisita's index ... by hand
propA <- speciesA/sum(speciesA)
propB <- speciesB/sum(speciesB)

morind1 <- (2*sum(propA*propB))/(sum(propA*((num.speciesA-1)/(N.speciesA-1)))+sum(propB*((num.speciesB-1)/(N.speciesB-1))))  # 0.4977763


test_that("dietOverlap() messages",{
})

test_that("dietOverlap() results",{
  ## Test Morisita's index result
  morind2 <- dietOverlap(speciesA, speciesB, prey=test.names, type="Morisita",
                         num1=num.speciesA,num2=num.speciesB,
                         N1=N.speciesA,N2=N.speciesB)
  expect_equal(morind1,morind2$doi)
})
