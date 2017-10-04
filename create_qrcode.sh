#!/usr/local/bin/Rscript
args<-commandArgs(TRUE)
if (length(args)==0) stop("Please supply arguments")
cwd <- getwd()
source("mask.R")
source("data.R")
source("error_encoding.R")
qrplot(mode="byte", text=args[1], qrversion="4-L",
       file=TRUE, filename=paste(cwd, "/sample.png", sep=""))
