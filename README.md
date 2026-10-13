[![rtemis.dplot3 status badge](https://rtemis-org.r-universe.dev/rtemis.dplot3/badges/version)](https://rtemis-org.r-universe.dev/rtemis.dplot3)

# rtemis.dplot3: Interactive visualization from rtemis legacy

`rtemis.dplot3` holds the `draw_*()` functions that began in the
[rtemis](https://github.com/rtemis-org/rtemis) package: interactive 'plotly',
'networkD3', 'threejs' and 'leaflet' graphics, with rtemis themes
(`theme_*()`, `choose_theme()`) and palettes (`get_palette()`).
`available_draw()` lists the functions.

[rtemis.draw](https://github.com/rtemis-org/draw) provides their successors,
and the rtemis model plots (`plot_*()`, `present()`) render through it.

## Installation

```r
install.packages(
  "rtemis.dplot3",
  repos = c("https://rtemis-org.r-universe.dev", "https://cloud.r-project.org")
)
```

## Options

Set these in `.Rprofile` to change the defaults of every `draw_*()` call:

- `rtemis_theme`: theme name, e.g. `"whitegrid"` or `"darkgraygrid"`
- `rtemis_palette`: palette name, e.g. `"rtms"`
- `rtemis_font`: font family
