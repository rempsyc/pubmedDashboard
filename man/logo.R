# logo

library(hexSticker) # Create Hexagon Sticker in R
library(showtext)   # Using Fonts More Easily in R Graphs

## Loading Google fonts (http://www.google.com/fonts)
font_add_google("Inconsolata", "incon")

font_add_google("Shizuru", "cursive", db_cache = FALSE)

font_add_google("Jura", "sans-serif")

font_add_google("Gochi Hand", "cursive")

font_add_google("Megrim", "cursive")

font_add_google("Oxanium", "cursive")

# Current font!***
font_add_google("Gluten", "cursive")

font_add_google("Comfortaa", "cursive")

font_add_google("Baloo 2", "cursive")

font_add_google("Saira", "sans-serif")


sticker(
  # Subplot (image)
  subplot = "man/figures/logo_image.png",       # Image name
  s_y = 0.88,                       # Position of the sub plot (y)
  s_x = 1.0,                        # Position of the sub plot (x)
  s_width = 0.91,                   # Width of the sub plot
  s_height=0.002,                   # Height of the sub plot
  # Font
  package = "Dashboard",              # Package name (will be printed on the sticker)
  # p_color = "#ccffcc",            # Colour of the text #ccffff
  p_size = 115,                     # Font size of the text
  p_y = 0.8,                        # Position of the font (y)
  p_x=.95,                          # Position of the font (x)
  p_family = "incon",                # Defines font
  # Spotlight
  spotlight = TRUE,                 # Enables spotlight
  l_y=0.8,                          # Position of spotlight (y)
  l_x=0.7,                          # Position of spotlight (x)
  # Sticker colors
  h_fill = "#EC6D64",               # Color for background
  h_color = "#7E5287",              # Color for border
  # Resolution
  dpi=1200,                         # Sets DPI
  # Save
  filename="man/figures/logo_temp.png"               # Sets file name and location where to store the sticker
) -> my.logo
plot(my.logo)

