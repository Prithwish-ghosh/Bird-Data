#!/usr/bin/env python
# coding: utf-8

# In[7]:


import pandas as pd
import matplotlib.pyplot as plt

import numpy as np
data = pd.read_csv("D:/Desktop/research codes/bird_tracking.csv")
data = data.dropna()
species =(data['bird_name'])
# Sample data for three bird species with longitude, latitude, direction, and species
lon = np.array(data['longitude'])
lat = np.array(data['latitude'])
direction = np.array(data['direction'])


# In[12]:


get_ipython().system('pip install geopandas')


# In[13]:


import geopandas as gpd
# Create a GeoDataFrame from the data
data = {'Longitude': lon.flatten(), 'Latitude': lat.flatten(), 'Direction': direction.flatten(),
        'Species': species}
df = gpd.GeoDataFrame(data, geometry=gpd.points_from_xy(lon.flatten(), lat.flatten()))

# Define the latitude and longitude limits for the region of interest
lon_min, lon_max = -25, 25
lat_min, lat_max = 0, 70

# Filter the data within the specified region
region_df = df[(df['Longitude'] >= lon_min) & (df['Longitude'] <= lon_max) &
               (df['Latitude'] >= lat_min) & (df['Latitude'] <= lat_max)]

# Retrieve the world map data
world = gpd.read_file(gpd.datasets.get_path('naturalearth_lowres'))

# Create a figure and axes
fig, ax = plt.subplots(figsize=(10, 6))

# Plot the world map
world.plot(ax=ax, color='lightblue')

species_colors = {'Eric': 'blue', 'Nico': 'orange', 'Sanne': 'green'}  # Add more colors if needed


# Plot paths and directions for each species in the region
for species_name, species_data in region_df.groupby('Species'):
    line_coords = np.column_stack((species_data['Longitude'], species_data['Latitude']))
    ax.plot(line_coords[:, 0], line_coords[:, 1], label=species_name, linewidth=1)

    arrow_start = line_coords[0]
    arrow_direction = np.deg2rad(np.mean( species_data['Direction'].iloc[0]))
    arrow_length = 10  # Adjust arrow length as desired
    arrow_end = [
        arrow_start[0] + arrow_length * np.sin(arrow_direction),
        arrow_start[1] + arrow_length * np.cos(arrow_direction)
    ]
    arrow_color = species_colors.get(species_name, 'black') 
    ax.annotate("", xy=arrow_end, xytext=arrow_start,
                arrowprops=dict(facecolor=arrow_color, edgecolor=arrow_color, arrowstyle='->'))
# Set the aspect ratio and limits
ax.set_aspect('equal')
ax.set_xlim([lon_min, lon_max])
ax.set_ylim([lat_min, lat_max])

# Add a legend
ax.legend()

# Show the plot
plt.show()

