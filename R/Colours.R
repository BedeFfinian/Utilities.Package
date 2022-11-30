#' Colour Functions
#'
#' These functions are helper functions to create palettes and colours for scale_colour_Bede and scale_fill_Bede
#' @keywords Colours Palettes
#' @export
#' @examples
#' Bede_colours()
#' Bede_cols()
#' Bede_Palettes()
#' Bede_pal()
#' colorRampPaletteAlpha()
#' addalpha()

Bede_colours <- c(
  `red`        = "#bd1c1c",
  `green`      = "#38ab07",
  `blue`       = "#0058db",
  `orange`     = "#e3950e",
  `yellow`     = "#fce517",
  `light grey` = "#cccccc",
  `dark grey`  = "#545454",
  `dark cyan`  = "#008b8b",
  `dark purple`= "#3e0269",
  `light blue` = "#b5f6ff",
  `dark blue`  = "#0e1296",
  `dark gold`  = "#e86a28",
  `dark sea blue` = "#0f3d64",
  `Mussels Rope` = "#002673",
  `Mussels Control` = "#a90000",
  `Orkney RN` = "#56bda2",
  `Orkney RS` = "#dbb13b",
  `Orkney WC` = "#2494a2",
  `BathyBlue0`="#c6edec",
  `BathyBlue1`="#9ddee7",
  `BathyBlue2`="#72d0e1",
  `BathyBlue3`="#48b5d2",
  `BathyBlue4`="#218eb7",
  `BathyBlue5`="#1074a6",
  `BathyBlue6`="#054780",
  `BathyBlue7`="#042f66",
  `BathyBlue8`="#22496d",
  `BathyBlue9`="#5e24d6",
  `SAS_1`="#dbb13b",
  `SAS_2`="#2494a2",
  `AnnePort1`="#f5f8fd",
  `AnnePort2`="#bec3c5",
  `AnnePort3`="#aaccd6",
  `AnnePort4`="#a8c7d9",
  `AnnePort5`="#74a9ad",
  `CarbonViridis0`="#553e8d",
  `CarbonViridis1`="#482879",
  `CarbonViridis2`="#3d4a89",
  `CarbonViridis3`="#30688f",
  `CarbonViridis4`="#25838e",
  `CarbonViridis5`="#1f9f89",
  `CarbonViridis6`="#36b779",
  `CarbonViridis7`="#6ece59",
  `CarbonViridis8`="#b4de2c",
  `CarbonViridis9`="#fde825",
  `CarbonViridis10`="#fd6a32",
  `CarbonViridis11`="#e4494f",
  `CarbonDouble1`="#DAA520",
  `CarbonDouble2`="#008B8B",
  `LymeOC`="#cccccc",
  `LymePVC`="#008b8b",
  `LymeSA`="#DAA520",
  `LymeSI`="#e86a28",
  `Macro_1`="#DAA520",
  `Macro_2`="#389318",
  `Macro_3`="#873e23",
  `Macro_4`="#768807",
  `Macro_5`="#e0e809",
  `Macro_6`="#70543e",
  `Pilar_Old`="#e4ca75",
  `Pilar_Young`="#56a9b4",
  `Low`="#2d85c5",
  `LowMiddle`="#61bdee",
  `Middle`="#e2e2e2",
  `HighMiddle`="#dc6e55",
  `High`="#c33726"
)

Bede_cols <- function(...) {
  cols <- c(...)

  if (is.null(cols))
    return (Bede_colours)

  Bede_colours[cols]
}

Bede_palettes <- list(
  `main`  = Bede_cols("dark blue","dark cyan", "yellow"),

  `cool`  = Bede_cols("blue", "green"),

  `hot`   = Bede_cols("yellow", "orange", "red"),

  `mixed` = Bede_cols("dark cyan", "green", "yellow", "orange", "red"),

  `grey`  = Bede_cols("light grey", "dark grey"),

  `Mussels`= Bede_cols("Mussels Rope","Mussels Control"),

  `bathy` = Bede_cols("light blue","dark cyan", "dark blue", "dark purple"),

  `PhD` = Bede_cols("dark cyan", "light grey", "dark gold"),

  `Lyme` = Bede_cols("LymeOC", "LymePVC", "LymeSA", "LymeSI"),

  `Orkney` = Bede_cols("Orkney RN", "Orkney RS", "Orkney WC"),

  `Bathy_Blues`= Bede_cols("BathyBlue0","BathyBlue1","BathyBlue2",
                           "BathyBlue3","BathyBlue4","BathyBlue5",
                           "BathyBlue6","BathyBlue7","BathyBlue8",
                           "BathyBlue9"),
  `SAS`=Bede_cols("SAS_1","SAS_2"),

  `AnnePort` = Bede_cols("AnnePort1", "AnnePort2","AnnePort3",
                         "AnnePort4", "AnnePort5"),
  `Carbon_Viridis`= Bede_cols("CarbonViridis0","CarbonViridis1","CarbonViridis2",
                              "CarbonViridis3","CarbonViridis4","CarbonViridis5",
                              "CarbonViridis6","CarbonViridis7","CarbonViridis8",
                              "CarbonViridis9"),

  `CarbonDouble`=Bede_cols("CarbonDouble1","CarbonDouble2"),

  `Macro5`=Bede_cols("Macro_1","Macro_2","Macro_3","Macro_4","Macro_6"),

  `Macro5_noBS`=Bede_cols("Macro_1","Macro_2","Macro_3","Macro_4","Macro_5"),

  `Macro5_Pub`=Bede_cols("Macro_1","Macro_2","Macro_3","Macro_4","Macro_5"),

  `Macro6`=Bede_cols("Macro_1","Macro_2","Macro_3","Macro_4",
                     "Macro_5","Macro_6"),


  `Pilar`=Bede_cols("Pilar_Old","Pilar_Young"),

  `Climate`=Bede_cols("Low","LowMiddle","HighMiddle","High")
)

addalpha <- function(colors, alpha=1.0) {
  r <- col2rgb(colors, alpha=T)
  # Apply alpha
  r[4,] <- alpha*255
  r <- r/255.0
  return(rgb(r[1,], r[2,], r[3,], r[4,]))
}


Bede_pal <- function(palette = "main", reverse = FALSE, alpha=TRUE,...) {
  pal <- Bede_palettes[[palette]]

  if (reverse) pal <- rev(pal)

  grDevices::colorRampPalette(pal, alpha=TRUE,...)


}







