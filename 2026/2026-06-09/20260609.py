import pandas as pd
import plotnine as gg
import matplotlib.pyplot as plt
import matplotlib.font_manager as fm
import os
import highlight_text as ht
import re
from ninejs import interactive, css, save, show


# Load data ---------------------------------------------------------------

game_films = pd.read_csv(
    "https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-06-09/game_films.csv"
)


# Define colours ----------------------------------------------------------

bg_col = "#07090D"
text_col = "#dbdafb"


# Load fonts --------------------------------------------------------------

jersey_paths = [p for p in fm.findSystemFonts() if 'jersey' in p.lower()]
for p in jersey_paths:
    fm.fontManager.addfont(p)
dm_paths = [p for p in fm.findSystemFonts() if 'dm' in p.lower()]
for p in dm_paths:
    fm.fontManager.addfont(p)
    
available_names = [f.name for f in fm.fontManager.ttflist]
if 'DM Mono' in available_names:
    body_font = 'DM Mono'
else:
    body_font = 'sans-serif'

if 'Jersey 10' in available_names:
    title_font = 'Jersey 10'
else:
    title_font = 'sans-serif'


# Functions ---------------------------------------------------------------

def make_year_groups(years, year_group_size):
    years = list(years)

    # Trim from the start so remaining length is divisible by group size
    trim = len(years) % year_group_size
    yrs2 = years[trim:] if trim > 0 else years[:]

    yrs2 = np.array(yrs2)
    n = len(yrs2)

    # Group index for each year
    group_index = np.arange(n) // year_group_size

    # Start and end years for each group (using ONLY yrs2)
    start_years = yrs2[group_index * year_group_size]
    end_years   = yrs2[(group_index + 1) * year_group_size - 1]

    return pd.DataFrame({
        "year": yrs2,
        "year_group": [f"{s}\n to \n{e}" for s, e in zip(start_years, end_years)]
    })


# Data wrangling ----------------------------------------------------------

score_data = (
    game_films
    .assign(
        release_date=lambda d: pd.to_datetime(d["release_date"], errors="coerce"),
        year=lambda d: d["release_date"].dt.year
    )
    .dropna(subset=["year"])
    .assign(year=lambda d: d["year"].astype(int))
    .assign(
        cinema_score=lambda d: (
            d["cinema_score"]
            .replace("N/A", np.nan)
            .fillna("Unknown")
        )
    )
    .query("cinema_score != 'Unknown'")
    [["year", "cinema_score"]]
)

years = list(range(score_data["year"].min(), score_data["year"].max() + 1))
year_groups = make_year_groups(years, year_group_size=3)

plot_data = (
    score_data
    .merge(year_groups, on="year", how="left")
    .assign(
        cinema_score=lambda d: (
            d["cinema_score"]
            .replace("N/A", np.nan)
            .fillna("Unknown")
            .apply(
                lambda x: (
                    "A" if re.search("A", x) else
                    "B" if re.search("B", x) else
                    "C" if re.search("C", x) else
                    "D" if re.search("D", x) else
                    x
                )
            )
        )
    )
    .query("cinema_score != 'Unknown'")
    .groupby(["year_group", "cinema_score"])
    .size()
    .reset_index(name="n")
)

plot_data["total"] = plot_data.groupby("year_group")["n"].transform("sum")
plot_data["pct"] = 100 * plot_data["n"] / plot_data["total"]


# Define text -------------------------------------------------------------

title = "A retro masterpiece? Or game over?"
st = "Percentage of films released each period rated A-F (%)"
cap = '<Note::{"fontweight": "bold"}>: Data excludes films where either the release date or CinemaScore rating are unknown. \n<Data::{"fontweight": "bold"}>: Wikipedia (List of films based on video games)\n<Graphic::{"fontweight": "bold"}>: Nicola Rennie (@nrennie)'
desc_st = "Many video games have been adapted into films. Earlier video game movies tended to\nbe either disasters or cult hits, with all of the films with the <lowest rating (F)>\nfrom CinemaScore released before 2005. In recent years, video game inspired films\nare again becoming popular, with the <highest rating (A)> films released in the\nearly 2020s."

plot_data['summary'] = (
    plot_data['year_group'].astype(str)
    + ': '
    + plot_data['pct'].round().astype(int).astype(str)
    + '% rated '
    + plot_data['cinema_score'].astype(str)
)


# Plot --------------------------------------------------------------------

p = (
    gg.ggplot(
        plot_data,
        gg.aes(x="year_group", y="pct", fill="cinema_score", group="cinema_score",)
    )
    + gg.geom_col(color=bg_col, size=3)
    + gg.scale_fill_manual(
    values={
        "A": "#07fdf5",
        "B": "#5C5C5C",
        "C": "#474747",
        "D": "#333333",
        "F": "#f2020d",
    }
  )
  + gg.labs(
        title=title,
        subtitle=st,
        caption=" \n\n ",
    )
  + gg.coord_cartesian(expand=False)
  + gg.theme_minimal(base_size=10, base_family=body_font)
  + gg.theme(
        plot_background=gg.element_rect(fill=bg_col, color=bg_col),
        panel_background=gg.element_rect(fill=bg_col, color=bg_col),
        plot_title=gg.element_text(
            color=text_col,
            ha="left",
            weight="bold",
            size=20,
            margin={"b": 45, "t": 5},
            family=title_font,
        ),
        plot_subtitle=gg.element_text(
            color=text_col,
            ha="left",
            size=8.5,
            family=body_font,
        ),
        plot_caption=gg.element_text(
            margin={"b": 15},
        ),
        legend_position="none",
        axis_title_x=gg.element_blank(),
        axis_title_y=gg.element_blank(),
        plot_title_position="plot",
        axis_text_x=gg.element_text(size=7.5, lineheight=1, color=text_col),
        axis_text_y=gg.element_text(color=text_col),
        panel_grid_minor=gg.element_blank(),
        panel_grid_major=gg.element_blank()
    )
)

fig = p.draw()
fig.set_size_inches(7, 7, forward=True)

plt.figure(fig)

ht.fig_text(
    x=0.01, y=0.08, s=cap,
    fontsize=7.5, va='top',
    fontfamily=body_font,
    color=text_col,
    linespacing=0.3,
    fig=fig)
  
ht.fig_text(
    x=0.01, y=0.92,
    s=desc_st,
    highlight_textprops=[
        {"color": "#f2020d", "fontweight": "bold"},
        {"color": "#07fdf5", "fontweight": "bold"}
    ],
    size = 10,
    fontfamily=body_font,
    color=text_col,
    fig=fig,
)

    
# Save --------------------------------------------------------------------

fpath = os.path.join("2026", "2026-06-09", f'{"20260609"}.png')
fig.savefig(fpath, dpi=300)


# Interactive -------------------------------------------------------------

# Mostly works out of the box with original 'p' but 
# needs to tweak position of text

p = (
    gg.ggplot(
        plot_data,
        gg.aes(x="year_group", y="pct", fill="cinema_score", group="cinema_score", tooltip="summary", data_id="year_group")
    )
    + gg.geom_col(color=bg_col, size=3)
    + gg.scale_fill_manual(
    values={
        "A": "#07fdf5",
        "B": "#5C5C5C",
        "C": "#474747",
        "D": "#333333",
        "F": "#f2020d",
    }
  )
  + gg.labs(
        title=title,
        subtitle=st,
        caption=" \n\n ",
    )
  + gg.coord_cartesian(expand=False)
  + gg.theme_minimal(base_size=10, base_family=body_font)
  + gg.theme(
        plot_background=gg.element_rect(fill=bg_col, color=bg_col),
        panel_background=gg.element_rect(fill=bg_col, color=bg_col),
        plot_title=gg.element_text(
            color=text_col,
            ha="left",
            weight="bold",
            size=20,
            margin={"b": 50, "t": 5},
            family=title_font,
        ),
        plot_subtitle=gg.element_text(
            color=text_col,
            ha="left",
            size=8.5,
            family=body_font,
        ),
        plot_caption=gg.element_text(
            margin={"b": 15},
        ),
        legend_position="none",
        axis_title_x=gg.element_blank(),
        axis_title_y=gg.element_blank(),
        plot_title_position="plot",
        axis_text_x=gg.element_text(size=7.5, lineheight=1, color=text_col),
        axis_text_y=gg.element_text(color=text_col),
        panel_grid_minor=gg.element_blank(),
        panel_grid_major=gg.element_blank()
    )
)

fig = p.draw()
fig.set_size_inches(7, 7, forward=True)

plt.figure(fig)

ht.fig_text(
    x=0.01, y=0.10, s=cap,
    fontsize=7.5, va='top',
    fontfamily=body_font,
    color=text_col,
    linespacing=0.3,
    fig=fig)
  
ht.fig_text(
    x=0.01, y=0.92,
    s=desc_st,
    highlight_textprops=[
        {"color": "#f2020d", "fontweight": "bold"},
        {"color": "#07fdf5", "fontweight": "bold"}
    ],
    size = 9,
    fontfamily=body_font,
    color=text_col,
    linespacing=0.3,
    fig=fig,
)

(
    interactive(p, hover_nearest=True)
    + css(from_dict={".tooltip": {"font-size": "2em"}})
    + css(from_file="2026/2026-06-09/style.css")
    + show()
)

# For some reason, it draws subtitle text twice
# Use CSS to hide one
(
    interactive(p, hover_nearest=True)
    + css(from_dict={".tooltip": {"font-size": "2em"}})
    + css(from_file="style.css")
    + save("2026/2026-06-09/20260609.html")
)

