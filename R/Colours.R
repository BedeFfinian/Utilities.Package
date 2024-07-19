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
  `High`="#c33726",
  `Air_Ganges`="#c0c0c0",
  `Sed_Ganges`="#bf982d",
  `Wat_Ganges`="#3e999b",
  `Drone_BareSed`="#70543e",
  `Drone_BareSand`="#fde825",
  `Drone_Chlorophyta`="#b3ff1a",
  `Drone_DeepSed`="#000000",
  `Drone_LowZos`="#389350",
  `Drone_MPB`="#DAA520",
  `Drone_Magnoliopsida`="#389318",
  `Drone_Phaeohyta`="#873e23",
  `Drone_Rhodophyta`="#b3002d",
  `Drone_SunGlint`="#FFFFFF",
  `Drone_Water`="#42c9bc",
  `Drone_Xantho`="#cccc00",
  `Drone_NAN`="#c6edec",
  `Drone_SunGlint_Silver`="#c0c0c0",
  `Arctic_Basin_01`="#3d26ab",
  `Arctic_Baffin_02`="#4542e7",
  `Arctic_Canadian_03`="#4765ff",
  `Arctic_Beaufort_04`="#2d91ea",
  `Arctic_Chukchi_05`="#30a9d9",
  `Arctic_EastSib_06`="#29bfb5",
  `Arctic_Laptev_07`="#4ccc86",
  `Arctic_Kara_08`="#9fcb41",
  `Arctic_Barents_09`="#eaba33",
  `Arctic_North_10`="#fcd32c",
  `Jersey_Coarse_Sediments`="#a6cee3",
  `Jersey_Fine_Sediments`="#1f78b4",
  `Jersey_Cobbles_and_Pebbles`="#b2df8a",
  `Jersey_Rocky_Reef`="#cab2d6",
  `Jersey_Kelp_Forest`="#ff7f00",
  `Jersey_Hard_Ground`="#e31a1c",
  `Jersey_Slipper_Limpets`="#fdbf6f",
  `Jersey_Maerl_Beds`="#6a3d9a",
  `Jersey_Sand_Mason_Worms`="#eaba33",
  `Jersey_Offshore_Mixed_Sediments`="#fb9a99",
  `Jersey_Seagrass`="#33a02c"

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

  `CarbonDouble`=Bede_cols("CarbonDouble1","CarbonDouble2","CarbonViridis0"),

  `CarbonTriple`=Bede_cols("CarbonDouble1","CarbonDouble2","CarbonDouble3"),

  `Macro5`=Bede_cols("Macro_1","Macro_2","Macro_3","Macro_4","Macro_6"),

  `Macro5_noBS`=Bede_cols("Macro_1","Macro_2","Macro_3","Macro_4","Macro_5"),

  `Macro5_Pub`=Bede_cols("Macro_1","Macro_2","Macro_3","Macro_4","Macro_5"),

  `Macro6`=Bede_cols("Macro_1","Macro_2","Macro_3","Macro_4",
                     "Macro_5","Macro_6"),

  `InvaSeaSpectra`=Bede_cols("Macro_1","Macro_2","Macro_3",
                             "Drone_Rhodophyta","Macro_4",
                            "Macro_5"),

  `Ganges`=Bede_cols("Air_Ganges","Sed_Ganges","Wat_Ganges"),

  `Ganges_Ordered`=Bede_cols("Air_Ganges","Wat_Ganges","Sed_Ganges"),

  `Pilar`=Bede_cols("Pilar_Old","Pilar_Young"),

  `Climate`=Bede_cols("Low","LowMiddle","HighMiddle","High"),

  `ClimateGreen`=Bede_cols("Drone_LowZos","Macro_2","HighMiddle","High"),

  `Drone`=Bede_cols("Drone_BareSed","Drone_Chlorophyta","Drone_DeepSed","Drone_MPB",
                    "Drone_Magnoliopsida","Drone_Phaeohyta","Drone_Rhodophyta",
                    "Drone_SunGlint","Drone_Water","Drone_Xantho"),

  `Drone_Nans`=Bede_cols("Drone_NAN","Drone_BareSed","Drone_Chlorophyta","Drone_DeepSed","Drone_MPB",
                    "Drone_Magnoliopsida","Drone_Phaeohyta","Drone_Rhodophyta",
                    "Drone_SunGlint","Drone_Water","Drone_Xantho"),

  `Drone_D2.2`=Bede_cols("Drone_BareSed","Drone_Chlorophyta","Drone_DeepSed","Drone_MPB",
                         "Drone_Magnoliopsida","Drone_Phaeohyta","Drone_Rhodophyta",
                         "Drone_SunGlint_Silver","Drone_Water","Drone_Xantho"),

  `Drone_Paper`=Bede_cols("Drone_BareSed","Drone_Chlorophyta",
                          "Drone_Magnoliopsida","Drone_MPB",
                          "Drone_Phaeohyta","Drone_Rhodophyta",
                          "Drone_Water"),

  `Drone_LowZos`=Bede_cols("Drone_BareSed","Drone_Chlorophyta","Drone_DeepSed","Drone_LowZos",
                           "Drone_MPB","Drone_Magnoliopsida","Drone_Phaeohyta","Drone_Rhodophyta",
                         "Drone_SunGlint_Silver","Drone_Water","Drone_Xantho"),

  `S2_First`=Bede_cols("Drone_BareSed","Drone_Chlorophyta","Drone_Magnoliopsida","Drone_MPB",
                       "Drone_Phaeohyta","Drone_Rhodophyta",
                       "Drone_Water","Drone_Xantho"),

  `S2_Fourth`=Bede_cols("Drone_BareSand","Drone_BareSed","Drone_Chlorophyta","Drone_Magnoliopsida",
                        "Drone_MPB",
                       "Drone_Phaeohyta","Drone_Rhodophyta",
                       "Drone_Water","Drone_Xantho"),

  `S2_Sixth`=Bede_cols("Drone_BareSand","Drone_BareSed","Drone_Chlorophyta","Drone_Magnoliopsida",
                        "Drone_MPB",
                        "Drone_Phaeohyta","Drone_Rhodophyta",
                        "Drone_Water","Drone_Xantho","Drone_SunGlint_Silver"),

  `Arctic_Regions`=Bede_cols("Arctic_Basin_01",
                             "Arctic_Baffin_02",
                             "Arctic_Canadian_03",
                             "Arctic_Beaufort_04",
                             "Arctic_Chukchi_05",
                             "Arctic_EastSib_06",
                             "Arctic_Laptev_07",
                             "Arctic_Kara_08",
                             "Arctic_Barents_09",
                             "Arctic_North_10"),

  `JerseyHabs_All`=Bede_cols("Jersey_Coarse_Sediments",
                             "Jersey_Cobbles_and_Pebbles",
                             "Jersey_Fine_Sediments",
                             "Jersey_Hard_Ground",
                             "Jersey_Kelp_Forest",
                             "Jersey_Maerl_Beds",
                             "Jersey_Offshore_Mixed_Sediments",
                             "Jersey_Rocky_Reef",
                             "Jersey_Sand_Mason_Worms",
                             "Jersey_Seagrass",
                             "Jersey_Slipper_Limpets"),

  `JerseyHabs_Short_habs`=Bede_cols("Jersey_Seagrass",
                               "Jersey_Sand_Mason_Worms",
                               "Jersey_Fine_Sediments",
                               "Jersey_Coarse_Sediments",
                               "Jersey_Offshore_Mixed_Sediments",
                               "Jersey_Maerl_Beds"),

  `JerseyHabs_Short_depth`=Bede_cols("Jersey_Seagrass",
                                    "Jersey_Fine_Sediments",
                                    "Jersey_Coarse_Sediments",
                                    "Jersey_Sand_Mason_Worms")
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







