# https://overpass-turbo.eu/?template=key-value&key=amenity&value=fast_food

# /*
#   This query looks for nodes, ways and relations 
# with the given key/value combination.
# Choose your region and hit the Run button above!
#   */
#   [out:json][timeout:25];
# // gather results
# nwr["amenity"="fast_food"]({{bbox}});
# // print results
# out geom;


################################

# [out:json][timeout:25];
# // Define the area for Belfast
# area["name"="Belfast"]["boundary"="administrative"]->.searchArea;
# 
# // Find all parks in this area
# (
#   node["leisure"="park"](area.searchArea);
#   way["leisure"="park"](area.searchArea);
#   relation["leisure"="park"](area.searchArea);
# );
# out body;
# >;
# out skel qt;

###############################

# [out:json][timeout:60];
# {{geocodeArea:Northern Ireland}}->.searchArea;
# 
# nwr["amenity"="fast_food"](area.searchArea);
# out body;
# >;
# out skel qt;