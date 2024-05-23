## Install package rsthemes
install.packages("devtools")

devtools::install_github("gadenbuie/rsthemes")


## Install themes
rsthemes::install_rsthemes()

## Usage

# list installed themes
rsthemes::list_rsthemes()

# Try all themes
rsthemes::try_rsthemes()

# Try just the light, dark, or base16 themes, e.g.
rsthemes::try_rsthemes("light")

# Use a theme
rstudioapi::applyTheme("One Dark {rsthemes}")


