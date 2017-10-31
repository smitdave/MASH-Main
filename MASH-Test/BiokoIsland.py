# -*- coding: utf-8 -*-
#==============================================================================
# 
# MASH
# Analysis of Bioko Island Geographical Data
# Sean Wu
# July 19, 2017
# 
#==============================================================================
import osmnx as ox
from IPython.display import Image
%matplotlib inline

# import all movement paths
biokoGraph = ox.graph_from_place('Bioko', network_type='all')
ox.plot_graph(biokoGraph,fig_height=20)

# import buildings & save to .shp
biokoBldg = ox.buildings_from_place('Bioko')
biokoBldgProj = ox.project_gdf(biokoBldg)
ox.plot_buildings(gdf=biokoBldgProj,bgcolor='#333333',color='w',save=True, show=False, close=True, filename='biokoBldg', dpi=90)
Image('{}/{}.{}'.format('images', 'biokoBldg', 'png'), height=800, width=800)

biokoBldgShp = biokoBldg.drop(labels='nodes',axis=1)
biokoBldgShp.to_file('Desktop/BiokoBldg')

biokoAreas = biokoBldgProj.area

# basic analysis of road network
biokoGraphLength = ox.get_edge_colors_by_attr(biokoGraph, attr='length')
ox.plot_graph(biokoGraph, edge_color=biokoGraphLength,fig_height=30)


# helper funcion to get one-square-mile street networks, building footprints, and plot them
def make_plot(place, point, network_type='drive', bldg_color='orange', dpi=90,
              dist=805, default_width=4, street_widths=None):
    gdf = ox.buildings_from_point(point=point, distance=dist)
    gdf_proj = ox.project_gdf(gdf)
    fig, ax = ox.plot_figure_ground(point=point, dist=dist, network_type=network_type, default_width=default_width,
                                    street_widths=street_widths, save=False, show=False, close=True)
    fig, ax = ox.plot_buildings(gdf_proj, fig=fig, ax=ax, color=bldg_color, set_bounds=False,
                                save=True, show=False, close=True, filename=place, dpi=dpi)
    
    
#place = 'portland_buildings'
#point = (45.517309, -122.682138)
#make_plot(place, point)
#Image('{}/{}.{}'.format('images', place, 'png'), height=500, width=500)


place = 'bioko_buildings'
point = (3.742836,8.780056)
make_plot(place, point,'all',dist=4000)
Image('{}/{}.{}'.format('images', place, 'png'), height=500, width=500)
