import pandas as pd
import plotnine as gg

# Read CSV data
url = 'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-01-29/clean_cheese.csv'
cheese = pd.read_csv(url)

# Select columns and compute differences
plot_data = cheese[['Year', 'Cheddar']].copy()
plot_data['diff'] = plot_data['Cheddar'].diff()
plot_data.loc[plot_data.index[0], 'diff'] = plot_data['Cheddar'].iloc[0]
plot_data['start'] = plot_data['Cheddar'] - plot_data['diff']
plot_data['fill'] = plot_data['diff'] > 0

# Define text for annotation
annot_text = 'Data from the United States\nDepartment of Agriculture shows\nthat pounds of cheese consumed\nper person has been steadily rising\nsince the 1970s.\n\nGraphic: Nicola Rennie'

# Create the plot
p = (
    gg.ggplot(plot_data) +
    gg.geom_rect(mapping=gg.aes(xmin='Year-0.45', xmax='Year+0.45', ymin='start', ymax='Cheddar', fill='fill')) +
    gg.annotate('text', x=2002, y = 4, label=annot_text, size=8) +
    gg.labs(title='Who ate all the cheese?',
            x='',
            y='Pounds of cheese per person') +
    gg.scale_fill_manual(values=['#CC3F0C', '#09814A']) +
    gg.theme_xkcd() +
    gg.theme(legend_position='none',
             plot_title=gg.element_text(face='bold'))
)

# Save the plot
gg.ggsave(p, filename="2024/viz/05_diverging.png", height=4, width=6, dpi=300)
