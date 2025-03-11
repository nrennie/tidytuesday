import pandas as pd
import plotnine as gg
import matplotlib.pyplot as plt
import PyDyTuesday
import os
import highlight_text as ht
from pyfonts import load_font


# Load data ---------------------------------------------------------------

#PyDyTuesday.get_date("2025-03-11")
pixar_films = pd.read_csv("pixar_films.csv")
public_response = pd.read_csv("public_response.csv")


# Define colours ----------------------------------------------------------

bg_col = "#D6F9FF"
text_col = "#002329"
colors = ["#D1495B", "#EDAE49", "#00798C"]


# Load fonts --------------------------------------------------------------

title_font = load_font(
   font_url="https://github.com/google/fonts/blob/main/apache/slackey/Slackey-Regular.ttf?raw=true"
)
body_font = load_font(
   font_url="https://github.com/google/fonts/blob/main/ofl/notosans/NotoSans%5Bwdth%2Cwght%5D.ttf?raw=true"
)


# Data wrangling ----------------------------------------------------------

plot_data = (pixar_films
  .query("film_rating == 'G'")
  .drop(columns = ["run_time", "number", "film_rating"])
  .merge(public_response, left_on='film', right_on='film')
  .drop(columns = ["cinema_score"])
  .melt(id_vars=['film', 'release_date'], var_name='critic', value_name='score')
  .sort_values("release_date")
  )
plot_data['release_date'] = pd.to_datetime(plot_data.release_date)
plot_data['release_date'] = plot_data['release_date'].dt.strftime('%Y-%m-%d')
plot_data["year"] = pd.DatetimeIndex(plot_data['release_date']).year
plot_data['film'] = plot_data['film'].str.replace(r' ', ' \\: ')
plot_data["label"] = r"$\bf{" + plot_data["film"] +"}$" +"\n(Released "+ plot_data["year"].astype(str) +")"
plot_data['label'] = pd.Categorical(plot_data.label, categories=pd.unique((plot_data.label)))


# Define text -------------------------------------------------------------

title = '<Are Pixar films getting better or worse?::{"fontweight": "bold"}>'
st = 'Pixar released a total of 13 G-rated movies between 1995 and 2019. Ratings from\nall three critic sites (<Rotten Tomatoes::{"color": "#00798C", "fontweight": "bold"}>, <Metacritic::{"color": "#EDAE49", "fontweight": "bold"}>, and <Critics Choice::{"color": "#D1495B", "fontweight": "bold"}>) were below\naverage for <Cars 2::{"fontweight": "bold"}>, before slowly increasing for subsequent films, returning to average\nratings with the release of <Toy Story 4 ::{"fontweight": "bold"}>. There are no Critics Choice ratings for <Toy Story::{"fontweight": "bold"}>\nor <A Bug\'s Life::{"fontweight": "bold"}>. Both <Toy Story::{"fontweight": "bold"}> and <Toy Story 2::{"fontweight": "bold"}> have the maximum score on Rotten\nTomatoes.'
cap = '<Data::{"fontweight": "bold"}>: {pixarfilms}\n<Graphic::{"fontweight": "bold"}>: Nicola Rennie (@nrennie)'


# Plot --------------------------------------------------------------------

g = (gg.ggplot(data=plot_data, mapping = gg.aes(x="label", y="score", fill="critic"))
    + gg.geom_col(stat="identity", position="dodge", width=0.6)
    + gg.scale_fill_manual(values=colors)
    + gg.scale_x_discrete(limits=reversed)
    + gg.labs(x = "", y = "Score (Maximum 100)") 
    + gg.coord_flip(expand=False)
    + gg.theme_bw(base_size = 6, base_family = 'Noto Sans')
    + gg.theme(
        legend_position='none',
        plot_margin_top=0.25,
        plot_margin_bottom=0.03,
        plot_margin_left=0.03,
        plot_margin_right=0.03,
        figure_size=(10, 10),
        panel_grid_minor=gg.element_blank(),
        panel_grid_major_y=gg.element_blank(),
        panel_grid_major_x=gg.element_line(color = text_col, linewidth=0.3),
        text = gg.element_text(color = text_col),
        plot_background=gg.element_rect(fill=bg_col, color=bg_col),
        panel_background=gg.element_rect(fill=bg_col, color=bg_col))
)

g.draw(True)


# Annotations
fig = g.draw()
fig.set_size_inches(8, 8, forward=True)
ax = plt.gca()

# title
ht.fig_text(
    0.03,
    0.97,
    title,
    vsep=3,
    color=text_col,
    font=title_font,
    fontsize=12,
    va='top')
# subtitle
ht.fig_text(
    0.03,
    0.92,
    st,
    vsep=2.5,
    color=text_col,
    font='Noto Sans',
    fontsize=8,
    va='top')
# caption
ht.fig_text(
    0.03,
    0.005,
    cap,
    vsep=3,
    color=text_col,
    font='Noto Sans',
    fontsize=6,
    va='bottom')


# Save --------------------------------------------------------------------

fpath = os.path.join("2025", "2025-03-11", f'{"20250311"}.png')
plt.savefig(fpath, dpi=300, bbox_inches='tight', format="png")

plt.show()
plt.close()

