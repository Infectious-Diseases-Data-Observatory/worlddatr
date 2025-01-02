library(hexSticker)

iddo_fig = system.file("man/figures/IDDO globe.png", package = "worlddatr")

sticker(iddo_fig,
        package = "worlddatr",
        s_x = 1,
        p_size = 20,
        p_color = "#FFFFFF",
        p_family = "sans",
        p_fontface = "bold",
        h_fill = "#435c6d",
        h_color = "#e31b23",
        filename = "man/figures/worlddatr_logo.png",
        dpi = 300)
