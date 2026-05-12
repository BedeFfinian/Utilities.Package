.onLoad <- function(lib, pkg) {

  sysfonts::font_add_google(name = "Poppins", family = "poppins")
  showtext::showtext_auto()

}

.onUnload <- function(libpath) {

}
