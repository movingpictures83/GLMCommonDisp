
###################################################
### code chunk number 1: cqn.Rnw:7-8
###################################################
options(width=70)


###################################################
### code chunk number 2: load
###################################################
library(cqn)
library(scales)
library(edgeR)

dyn.load(paste("RPluMA", .Platform$dynlib.ext, sep=""))
source("RPluMA.R")

input <- function(inputfile) {
  parameters <<- read.table(inputfile, as.is=T);
  rownames(parameters) <<- parameters[,1];
    pfix = prefix()
  if (length(pfix) != 0) {
     pfix <<- paste(pfix, "/", sep="")
  }
}

run <- function() {}

output <- function(outputfile) {

d.mont <- readRDS(paste(pfix,parameters["dmont",2],sep="/"))
design <- readRDS(paste(pfix,parameters["design",2],sep="/"))

d.mont.cqn <- estimateGLMCommonDisp(d.mont, design = design)
efit.cqn <- glmFit(d.mont.cqn, design = design)
elrt.cqn <- glmLRT(efit.cqn, coef = 2)
print(summary(decideTestsDGE(elrt.cqn)))
#topTags(elrt.cqn, n = 2)

### code chunk number 20: edgeRstd
d.mont.std <- estimateGLMCommonDisp(d.mont, design = design)
efit.std <- glmFit(d.mont.std, design = design)
elrt.std <- glmLRT(efit.std, coef = 2)
print(summary(decideTestsDGE(elrt.std)))

}
