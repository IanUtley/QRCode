#!/usr/local/bin/Rscript
args <- commandArgs(TRUE)
if (length(args)==0) stop("Please supply byte data to encode")
cwd <- getwd()
setwd("..")
source("mask.R",keep.source=TRUE)
source("data.R",keep.source=TRUE)
source("error_encoding.R",keep.source=TRUE)

#Rprof("./tests/prof.out",line.profiling=TRUE)

QRPlot(mode="byte", text=args[1], qrversion="4-L",
       file=TRUE, filename=paste(cwd, "/qr.png", sep=""))

#Rprof(NULL)
#summaryRprof("./tests/prof.out",lines="show")
