library(httpuv) # >= 1.6.12
pkgload::load_all()

# Host the local directory
httpuv::runStaticServer("local/shiny-apps-out/")
