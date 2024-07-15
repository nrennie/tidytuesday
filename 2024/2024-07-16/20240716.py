import pandas as pd
import plotnine as gg
import matplotlib.font_manager
import matplotlib.pyplot as plt
from itertools import chain
import highlight_text as ht


# Load data ---------------------------------------------------------------

url = 'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-07-16/ewf_standings.csv'
ewf_standings = pd.read_csv(url)


# Load fonts --------------------------------------------------------------

flist = matplotlib.font_manager.findSystemFonts()
flist = ''.join(flist).lower()
if 'gadugi' in flist:
    body_font = 'Gadugi'
else:
    body_font = 'sans'
    
    
# Define colours and fonts-------------------------------------------------

bg_col = '#A9A9A9'
text_col = '#333333'
col_palette = list(chain(*[['#656565']*5, ['#001489'], ['#656565']*10]))

# title, subtitle
title_text = "FA Women's Super League"
st = "The Women's Super League is the highest league of women's football in England. The 2018-2019 season was the first\nafter a rebranding of the four highest levels in English women's football, where eleven teams competed. <Chelsea Women::{'color': '#001489'}>\nhave won every FA Women's Super League since 2019-2020.\n<Data::{'fontweight': 'bold'}>: English Women's Football (EWF) Database | <Graphic::{'fontweight': 'bold'}>: Nicola Rennie (@nrennie)"


# Data wrangling ----------------------------------------------------------

# Prep data for plotting
plot_data = ewf_standings[
   ewf_standings['division'].isin(["FA Women's Super League (WSL)"])
].copy()

plot_data = plot_data[
   plot_data['season'].isin(['2018-2019', '2019-2020', '2020-2021', '2021-2022'])
]

# Keep only relevant columns
plot_data = plot_data[['team_name', 'season', 'position']]



# Plot --------------------------------------------------------------------

p = (gg.ggplot(plot_data) +
     gg.geom_line(mapping=gg.aes(x='season', y='position', colour='team_name', group='team_name')) +
     gg.geom_label(mapping=gg.aes(x='season', y='position', label='team_name', colour='team_name'), size=6.5, family=body_font, label_padding=0.3) +
     gg.scale_y_reverse(breaks=[12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1], limits=[12, -2.5]) +
     gg.scale_color_manual(values=col_palette) +
     gg.scale_x_discrete(expand=(0,0.5,0,0.5)) +
     gg.labs(x='', y='') +
     gg.annotate('text', x=0.5, y=-2.5, label=title_text, color=text_col, 
                 family=body_font, ha='left', va='top', size=14, fontweight='bold') +
     gg.coord_cartesian(expand=True) +
     gg.theme_void(base_size=9) +
     gg.theme(legend_position='none',
              axis_text=gg.element_text(color=text_col),
              plot_background=gg.element_rect(fill=bg_col, color=bg_col),
              panel_background=gg.element_rect(fill=bg_col, color=bg_col))
)

# Set up plot options
fig = p.draw()
fig.set_size_inches(8.5, 6, forward=True)
fig.set_dpi(300)
ax = plt.gca()

# text
ht.ax_text(
    0.5,
    1.8,
    st,
    vsep=3,
    color=text_col,
    fontname=body_font,
    fontsize=8.5,
    va='top')

# Save image
plt.savefig('2024/2024-07-16/20240716.png', dpi=300, bbox_inches='tight')
