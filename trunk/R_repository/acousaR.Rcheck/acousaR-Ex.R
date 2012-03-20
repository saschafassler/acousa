pkgname <- "acousaR"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
library('acousaR')

assign(".oldSearch", search(), pos = 'CheckExEnv')
cleanEx()
nameEx("acousaR-package")
### * acousaR-package

flush(stderr()); flush(stdout())

### Name: acousaR-package
### Title: What the package does (short line) ~~ package title ~~
### Aliases: acousaR-package acousaR
### Keywords: package

### ** Examples

~~ simple examples of the most important functions ~~



### * <FOOTER>
###
cat("Time elapsed: ", proc.time() - get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
