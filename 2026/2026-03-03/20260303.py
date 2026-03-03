import pandas as pd
import plotnine as gg
import matplotlib.pyplot as plt
import highlight_text as ht
import os
import textwrap


# Load data ---------------------------------------------------------------

clutch_size_cleaned = pd.read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-03-03/clutch_size_cleaned.csv')
tortoise_body_condition_cleaned = pd.read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-03-03/tortoise_body_condition_cleaned.csv')


# Define colours ----------------------------------------------------------

bg_col = "#EBDDF8"
text_col = "#290D44"
m_col = "#B8B8B8"
f_col = "#5C1E99"


# Data wrangling ----------------------------------------------------------

plot_data = (
    tortoise_body_condition_cleaned
    .query("year >= 2010")
    .assign(year=lambda x: x["year"].astype(str))
    .groupby(["year", "sex", "locality"])
    .size()
    .reset_index(name="n")
)

plot_data_labelled = (
    plot_data
    .assign(total=lambda x: x.groupby(["year", "locality"])["n"].transform("sum"))
    .assign(pct=lambda x: x["n"] / x["total"])
    .assign(pct_label=lambda x: (x["pct"] * 100).round(0).astype(int).astype(str) + "%")
    .sort_values(["year", "locality", "sex"], ascending=[True, True, False])
    .assign(pos=lambda x: 1 - x.groupby(["year", "locality"])["pct"].cumsum() + x["pct"] / 2)
    .assign(pct_label=lambda x: x["pct_label"].where(x["pct"] >= 0.05, ""))
)
years = sorted(plot_data["year"].unique(), reverse=True)


# Define text -------------------------------------------------------------

title = 'Male Hermann\'s tortoises dramatically outnumber\n<female::{"color": "#5C1E99"}> tortoises on island plateau'
st = "In an dense island population of Hermann's tortoises in Lake Prespa in North Macedonia, males dramatically outnumber females. Males inflict injuries on females and put them at risk of fatal falls from the plateau's sheer rock faces. Sixteen years of capture-recapture data reveal an ongoing extinction event and predict that the last island female will die in 2083."
wrapped_subtitle = '\n'.join(textwrap.wrap(st, width=80))
cap = '<Data::{"fontweight": "bold"}>: Sex Ratio Bias Triggers Demographic Suicide in a Dense Tortoise Population. Ecology Letters. 2026.\n<Graphic::{"fontweight": "bold"}>: Nicola Rennie (@nrennie)'


# Plot --------------------------------------------------------------------

p = (
  gg.ggplot(plot_data_labelled, gg.aes(y = "n", x = "year", fill = "sex"))
  + gg.geom_col(position=gg.position_fill(reverse=True), width=0.8)
  + gg.geom_text(gg.aes(y="pos", label="pct_label", colour = "sex"), size=7, fontweight='bold')
  + gg.facet_wrap("locality", ncol = 1) 
  + gg.coord_flip()
  + gg.scale_x_discrete(limits=years)
  + gg.scale_y_continuous(expand=(0, 0.005, 0, 0))
  + gg.scale_fill_manual(values = [f_col, m_col])
  + gg.scale_colour_manual(values = ["#FFFFFF", "#000000"])
  + gg.theme_minimal()
  + gg.theme(
      text = gg.element_text(colour = text_col),
      axis_ticks = gg.element_blank(),
      panel_grid = gg.element_blank(),
      axis_title = gg.element_blank(),
      axis_text_x = gg.element_blank(),
      legend_position = "none",
      strip_background = gg.element_blank(),
      strip_text_x=gg.element_text(hjust=0, margin={"l": 0}, weight="bold"),
      plot_margin_left=0.03,
      plot_margin_right=0.03,
      plot_margin_top=0.28,
      plot_margin_bottom=0.08,
      plot_subtitle=gg.element_text(ha='left', x=-0.5),
      figure_size=(6, 9.5)
))

fig = p.draw()
fig.set_size_inches(6, 9.5, forward=True)

plt.figure(fig)

# add caption
ht.fig_text(0.02, 0.04, cap, color=text_col, fontsize=7.5, va='top', fontfamily='sans-serif', fig=fig)
# add title
ht.fig_text(0.02, 0.98, title, color=text_col, fontsize=14, va='top', fontweight='bold', fontfamily='sans-serif', fig=fig)
# add subtitle
ht.fig_text(0.02, 0.93, wrapped_subtitle, color=text_col, fontsize=10, va='top', fontfamily='sans-serif', fig=fig, linespacing=0.9)

# Save --------------------------------------------------------------------

fpath = os.path.join("2026", "2026-03-03", f'{"20260303"}.png')
fig.savefig(fpath, dpi=300)
