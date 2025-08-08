library(shinylive)
library(httpuv)

shinylive::export(appdir = ".", destdir = "docs")

httpuv::runStaticServer("docs")
