context("Tests of gReshape")

## Get some data
if (require(FSA)) {
  data(SMBassWB)
  SMBassWB <- subset(SMBassWB,agecap<3,select=c(names(SMBassWB)[1:9],"radcap"))
}

test_that("gReshape() messages",{
  if (require(FSA)) {
    ## No in.pre=
    expect_error(gReshape(SMBassWB),"You must have a prefix string")
    ## Variable does not exist
    expect_error(gReshape(SMBassWB,in.pre="derek"),"No variables start with")
    expect_error(gReshape(SMBassWB,in.pre="anu",last.plus="derek"),"'last.plus=' variable not found")
  }
})

test_that("gReshape() output",{
  if (require(FSA)) {
    ## Change "radcap" to "ttlanu" to make sure that it does not get treated with in.pre=
    tmp <- SMBassWB
    names(tmp)[which(names(tmp)=="radcap")] <- "ttlanu"
    tmp <- gReshape(tmp,in.pre="anu")
    expect_true(any(names(tmp)=="ttlanu"))
  }
})
