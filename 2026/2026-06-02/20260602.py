import pandas as pd
import numpy as np
import country_converter as coco
import plotnine as gg
import highlight_text as ht
import os
import matplotlib.pyplot as plt
import matplotlib.font_manager as fm


# Load data ---------------------------------------------------------------
 
eplp = pd.read_csv(
    "https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-06-02/eplp.csv"
)
 
 
# Define colours and fonts ------------------------------------------------
 
bg_col = "#F2F4F8"
text_col = "#151C28"
m_col = "#7F055F"
c_col = "#F26419"


# Load fonts --------------------------------------------------------------

    
oswald_paths = [p for p in fm.findSystemFonts() if 'oswald' in p.lower()]
for p in oswald_paths:
    fm.fontManager.addfont(p)


nunito_paths = [p for p in fm.findSystemFonts() if 'nunito' in p.lower()]
for p in nunito_paths:
    fm.fontManager.addfont(p)
    
    
available_names = [f.name for f in fm.fontManager.ttflist]
if 'Nunito' in available_names:
    body_font = 'Nunito'
else:
    body_font = 'sans-serif'

if 'Oswald' in available_names:
    title_font = 'Oswald'
else:
    title_font = 'sans-serif'
    

# Data wrangling ----------------------------------------------------------
 
plot_data = eplp[["country", "year", "jp_ld_m", "jp_ld_co"]].copy()
 
cc = coco.CountryConverter()
plot_data["country_name"] = cc.convert(
    plot_data["country"].tolist(), to="name_short", not_found=None
)
plot_data.loc[plot_data["country"] == "UK", "country_name"] = "United Kingdom"
 
plot_data = (
    plot_data
    .rename(columns={"jp_ld_m": "mother", "jp_ld_co": "coparent"})
    [["country_name", "year", "mother", "coparent"]]
    .melt(id_vars=["country_name", "year"], var_name="name", value_name="value")
)
 
plot_data["value"] = plot_data["value"].replace(-98, np.nan)
 
tmp = plot_data.copy()
tmp["total"] = tmp.groupby(["country_name", "year"])["value"].transform("sum")
 
most_recent = (
    tmp
    .sort_values("year", ascending=False)
    .groupby("country_name", group_keys=False)
    .apply(lambda g: g[g["year"] == g["year"].iloc[0]])
    .reset_index(drop=True)
)
 
country_levels = (
    tmp[tmp["name"] == "mother"]
    .sort_values("year", ascending=False)
    .groupby("country_name", group_keys=False)
    .first()
    .reset_index()
    .sort_values("total", ascending=False)
    ["country_name"]
    .tolist()
)
 
plot_data["country_name"] = pd.Categorical(
    plot_data["country_name"], categories=country_levels, ordered=True
)
 
miss_data = plot_data[plot_data["value"].isna()].copy()
miss_data["xmin"] = miss_data["year"] - 0.5
miss_data["xmax"] = miss_data["year"] + 0.5
miss_data["ymin"] = 0
miss_data["ymax"] = 320
miss_data["fill_group"] = "missing"
 
plot_data_filled = plot_data.copy()
plot_data_filled["value"] = plot_data_filled["value"].fillna(0)
 
 
# Define text -------------------------------------------------------------
 
title = "Several countries offer up to 3 years of job protected leave for each parent"
st = "Maximum duration of job protected leave for <birth mothers> and <co-parents> in weeks. 1970 - 2024."
cap = '<Data::{"fontweight": "bold"}>: S. Spitzer et al. The European Parenting Leave Policies (EPLP) Dataset. Zenodo. Nov 19, 2025. doi: 10.5281/zenodo.17648712\n<Graphic::{"fontweight": "bold"}>: Nicola Rennie (@nrennie)'


# Plot --------------------------------------------------------------------

p = (
    gg.ggplot()
    + gg.geom_area(
        data=plot_data_filled,
        mapping=gg.aes(x="year", y="value", fill="name"),
    )
    + gg.geom_rect(
        data=miss_data,
        mapping=gg.aes(
            xmin="xmin", xmax="xmax",
            ymin="ymin", ymax="ymax",
            fill="fill_group",
        ),
        alpha=0.25,
    )
    + gg.facet_wrap("~country_name", nrow=3)
    + gg.scale_fill_manual(
        values={
            "mother": m_col,
            "coparent": c_col,
            "missing": "grey",
        },
        breaks=["missing"],
        labels=["Data on job protected leave for co-parents unavailable"],
        name="",
    )
    + gg.scale_x_continuous(breaks=[1978, 2016], labels=["1970", "2024"])
    + gg.labs(
        x=None,
        y=None,
        title=title,
        caption=" \n ",
    )
    + gg.coord_cartesian(expand=False)
    + gg.guides(fill=gg.guide_legend(override_aes={"alpha": 0.25}))
    + gg.theme_minimal(base_size=10, base_family='Nunito')
    + gg.theme(
        plot_background=gg.element_rect(fill=bg_col, color=bg_col),
        panel_background=gg.element_rect(fill=bg_col, color=bg_col),
        plot_title=gg.element_text(
            color=text_col,
            ha="left",
            weight="bold",
            size=14,
            margin={"b": 30, "t": 5},
            family='Oswald',
        ),
        plot_caption=gg.element_text(
            margin={"b": 10},
        ),
        legend_title=gg.element_blank(),
        legend_text=gg.element_text(size=10),
        legend_position=(-0.03, 1.09),
        legend_justification="left",
        legend_key_size=8,
        axis_title_x=gg.element_blank(),
        axis_title_y=gg.element_blank(),
        strip_text=gg.element_text(
            weight="bold",
            margin={"t": 5, "b": 3},
            size=7,
            ha="left",
            family='Nunito',
        ),
        plot_title_position="plot",
        axis_text_x=gg.element_text(size=8),
        panel_grid_minor=gg.element_blank(),
        panel_grid_major_x=gg.element_blank(),
        panel_spacing_x=0.02,
        panel_spacing_y=0.035
    )
)


# Highlight text ----------------------------------------------------------

fig = p.draw()
fig.set_size_inches(7, 5, forward=True)

plt.figure(fig)

ht.fig_text(
    x=0.01, y=0.06, s=cap,
    fontsize=7.5, va='top',
    fontfamily=body_font,
    color=text_col,
    linespacing=0.6,
    fig=fig)
    
ht.fig_text(
    x=0.01, y=0.92,
    s=st,
    highlight_textprops=[
        {"color": m_col, "fontweight": "bold"},
        {"color": c_col, "fontweight": "bold"},
    ],
    size = 10,
    fontfamily=body_font,
    color=text_col,
    fig=fig,
)


# Save --------------------------------------------------------------------

fpath = os.path.join("2026", "2026-06-02", f'{"20260602"}.png')
fig.savefig(fpath, dpi=300)
