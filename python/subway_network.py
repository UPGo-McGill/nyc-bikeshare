import csv
import os
import sys
import networkx as nx
import osmnx as ox
import argparse

def parseLong(input):
    clean   = input.replace('c', '')
    clean   = clean.replace('(', '')
    long     = float(clean)
    return long

def parseLat(input):
    clean   = input.replace(')', '')
    clean   = clean.replace(' ', '')
    lat    = float(clean)
    return lat

def cityNetwork(file):
    with open(os.path.join(sys.path[0],"../data/" + file), newline='\n') as file:
        entries = csv.reader(file)
        lines = list(entries)
        i = 1
        graphs = []
        for i in range(1, len(lines)):
            lat  = float(lines[i][3])
            long = float(lines[i][4])
            nextG = ox.graph_from_point((lat,long), distance=960, distance_type = 'network', network_type='all')
            graphs.append(nextG)
        G = nx.compose_all(graphs)
        ox.save_graph_shapefile(G, filename='entire_city', folder = 'whole_city')

def neighborhoodsSmall(file):
    with open(os.path.join(sys.path[0],"../data/" + file), newline='\n') as file:
        entries = csv.reader(file)
        lines = list(entries)

        br   = []
        cb   = []
        chb  = []
        eb   = []
        enyc = []
        fr   = []
        jhf  = []
        j    = []
        sb   = []
        spbr = []
        um   = []
        wb   = []

        for i in range(1, len(lines)):
            print(i, "of", (len(lines)-1))
            lat = parseLat(lines[i][10])
            long = parseLong(lines[i][9])
            G = ox.graph_from_point((lat,long), distance=960, distance_type = 'network', network_type='all')
            if(lines[i][6] == "Bushwick/Ridgewood"):
                br.append(G)
            elif(lines[i][6] == "Central Bronx"):
                cb.append(G)
            elif(lines[i][6] == "Crown Heights/Brownsville"):
                chb.append(G)
            elif(lines[i][6] == "East Bronx"):
                eb.append(G)
            elif(lines[i][6] == "East New York/Canarsie"):
                enyc.append(G)
            elif(lines[i][6] == "Far Rockaway"):
                fr.append(G)
            elif(lines[i][6] == "Jackson Heights/Flushing"):
                jhf.append(G)
            elif(lines[i][6] == "Jamaica"):
                j.append(G)
            elif(lines[i][6] == "South Bronx"):
                sb.append(G)
            elif(lines[i][6] == "Sunset Park/Bay Ridge"):
                spbr.append(G)
            elif(lines[i][6] == "Upper Manhattan"):
                um.append(G)
            elif(lines[i][6] == "West Bronx"):
                wb.append(G)
            else:
                continue

        print("Composing neighborhood graphs from indivdual graphs.")
        ox.save_graph_shapefile(nx.compose_all(br), filename='Bushwick_Ridgewood', folder="subway_service_network")
        print("Composed Bushwick/Ridgewood graph.")
        ox.save_graph_shapefile(nx.compose_all(cb), filename='Central_Bronx', folder="subway_service_network")
        print("Composed Central Bronx graph.")
        ox.save_graph_shapefile(nx.compose_all(chb), filename='Crown_Heights_Brownsville', folder="subway_service_network")
        print("Composed Crown Heights/Brownsville graph.")
        ox.save_graph_shapefile(nx.compose_all(eb), filename='East_Bronx', folder="subway_service_network")
        print("Composed East Bronx graph.")
        ox.save_graph_shapefile(nx.compose_all(enyc), filename='East_New_York_Canarsie', folder="subway_service_network")
        print("Composed East New York/Canarsie graph.")
        ox.save_graph_shapefile(nx.compose_all(fr), filename='Far_Rockaway', folder="subway_service_network")
        print("Composed Far Rockaway graph.")
        ox.save_graph_shapefile(nx.compose_all(jhf), filename='Jackson_Heights_Flushing', folder="subway_service_network")
        print("Composed Jackson Heights/Flushing graph.")
        ox.save_graph_shapefile(nx.compose_all(j), filename='Jamaica', folder="subway_service_network")
        print("Composed Jamaica graph.")
        ox.save_graph_shapefile(nx.compose_all(sb), filename='South_Bronx', folder="subway_service_network")
        print("Composed South Bronx graph.")
        ox.save_graph_shapefile(nx.compose_all(spbr), filename='Sunset_Park_Bay_Ridge', folder="subway_service_network")
        print("Composed Sunset Park/Bay Ridge graph.")
        ox.save_graph_shapefile(nx.compose_all(um), filename='Upper_Manhattan', folder="subway_service_network")
        print("Composed Upper Manhattan graph.")
        ox.save_graph_shapefile(nx.compose_all(wb), filename='West_Bronx', folder="subway_service_network")
        print("Composed West Bronx graph.")
        print("Composed all graphs.")

def neighborhoodsLarge(file):
    with open(os.path.join(sys.path[0],"../data/" + file), newline='\n') as file:
        entries = csv.reader(file)
        lines = list(entries)

        br   = []
        cb   = []
        chb  = []
        eb   = []
        enyc = []
        fr   = []
        jhf  = []
        j    = []
        sb   = []
        spbr = []
        um   = []
        wb   = []

        for i in range(1, len(lines)):
            print(i, "of", (len(lines)-1))
            lat = parseLat(lines[i][10])
            long = parseLong(lines[i][9])
            G = ox.graph_from_point((lat,long), distance=2400, distance_type = 'network', network_type='all')
            if(lines[i][6] == "Bushwick/Ridgewood"):
                br.append(G)
            elif(lines[i][6] == "Central Bronx"):
                cb.append(G)
            elif(lines[i][6] == "Crown Heights/Brownsville"):
                chb.append(G)
            elif(lines[i][6] == "East Bronx"):
                eb.append(G)
            elif(lines[i][6] == "East New York/Canarsie"):
                enyc.append(G)
            elif(lines[i][6] == "Far Rockaway"):
                fr.append(G)
            elif(lines[i][6] == "Jackson Heights/Flushing"):
                jhf.append(G)
            elif(lines[i][6] == "Jamaica"):
                j.append(G)
            elif(lines[i][6] == "South Bronx"):
                sb.append(G)
            elif(lines[i][6] == "Sunset Park/Bay Ridge"):
                spbr.append(G)
            elif(lines[i][6] == "Upper Manhattan"):
                um.append(G)
            elif(lines[i][6] == "West Bronx"):
                wb.append(G)
            else:
                continue

        print("Composing neighborhood graphs from indivdual graphs.")
        ox.save_graph_shapefile(nx.compose_all(br), filename='Bushwick_Ridgewood', folder="bike_service_network")
        print("Composed Bushwick/Ridgewood graph.")
        ox.save_graph_shapefile(nx.compose_all(cb), filename='Central_Bronx', folder="bike_service_network")
        print("Composed Central Bronx graph.")
        ox.save_graph_shapefile(nx.compose_all(chb), filename='Crown_Heights_Brownsville', folder="bike_service_network")
        print("Composed Crown Heights/Brownsville graph.")
        ox.save_graph_shapefile(nx.compose_all(eb), filename='East_Bronx', folder="bike_service_network")
        print("Composed East Bronx graph.")
        ox.save_graph_shapefile(nx.compose_all(enyc), filename='East_New_York_Canarsie', folder="bike_service_network")
        print("Composed East New York/Canarsie graph.")
        ox.save_graph_shapefile(nx.compose_all(fr), filename='Far_Rockaway', folder="bike_service_network")
        print("Composed Far Rockaway graph.")
        ox.save_graph_shapefile(nx.compose_all(jhf), filename='Jackson_Heights_Flushing', folder="bike_service_network")
        print("Composed Jackson Heights/Flushing graph.")
        ox.save_graph_shapefile(nx.compose_all(j), filename='Jamaica', folder="bike_service_network")
        print("Composed Jamaica graph.")
        ox.save_graph_shapefile(nx.compose_all(sb), filename='South_Bronx', folder="bike_service_network")
        print("Composed South Bronx graph.")
        ox.save_graph_shapefile(nx.compose_all(spbr), filename='Sunset_Park_Bay_Ridge', folder="bike_service_network")
        print("Composed Sunset Park/Bay Ridge graph.")
        ox.save_graph_shapefile(nx.compose_all(um), filename='Upper_Manhattan', folder="bike_service_network")
        print("Composed Upper Manhattan graph.")
        ox.save_graph_shapefile(nx.compose_all(wb), filename='West_Bronx', folder="bike_service_network")
        print("Composed West Bronx graph.")
        print("Composed all graphs.")


#def main():
    #parser = argparse.ArgumentParser()
    #parser.add_argument("type", help = "Choose either c for the city network or n for the neighborhood network")
    #args = parser.parse_args()
#    nb = input("Choose either c for the city network or n for the neighborhood network")
#    if(nb == 'c'):
#        with open(os.path.join(sys.path[0],"../data/nbhd_buffer_subway_stations.csv"), newline='\n') as file:
#            cityNetwork(file)
#    elif(args.type == 'n'):
#        with open(os.path.join(sys.path[0],"../data/nbhd_subway_stations.csv"), newline='\n') as file:
#            neighborhoods(file)
#    else:
#        print("You didn't enter either 'c' or 'n' - please run the program and try again.")
#
#if __name__ == "__main__":
#    main()
