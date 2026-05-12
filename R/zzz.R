.onLoad <- function(lib, pkg) {
  run_on_load()
}

on_load({

  sysfonts::font_add_google(name = "Poppins", family = "poppins")
  showtext::showtext_auto()

})

.onUnload <- function(libpath) {

}
