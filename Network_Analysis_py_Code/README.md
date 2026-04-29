# 🗺️ Urban Mobility Accessibility Analysis
**Geospatial analysis of walking & cycling accessibility to public transport and urban facilities**

> Study area: Schwabing, Munich, Germany  
> Tools: Python · GeoPandas · OSMnx · Folium · Matplotlib  
> Data source: OpenStreetMap (via OSMnx)

---

## 📌 Project Overview

This project analyses **urban transport accessibility** using open geospatial data.
It quantifies how well residents can reach public transport stops and key urban
facilities on foot or by bicycle — a core metric in sustainable mobility planning.

### Key questions addressed
- What share of the study area falls within a 5-minute walk of a PT stop?
- Which hospitals, schools, and supermarkets are poorly connected to transit?
- How do walking and cycling catchments compare across the network?

---

## 📂 Project Structure

```
urban_mobility/
│
├── 01_data_collection.py        # Download OSM data (networks, PT stops, facilities)
├── 02_accessibility_analysis.py # Catchment polygons, nearest-stop distances, coverage stats
├── 03_static_maps.py            # Publication-quality maps (GeoPandas + Matplotlib)
├── 04_interactive_map.py        # Interactive HTML dashboard (Folium)
│
├── data/                        # Generated GeoJSON layers
│   ├── walk_network.geojson
│   ├── bike_network.geojson
│   ├── pt_stops.geojson
│   ├── facilities.geojson
│   ├── catchments.geojson
│   ├── accessibility_results.geojson
│   ├── coverage_stats.csv
│   └── facility_accessibility_summary.csv
│
└── outputs/                     # Maps and charts
    ├── map1_network_overview.png
    ├── map2_walk_isochrones.png
    ├── map3_facility_accessibility.png
    ├── chart4_coverage_stats.png
    └── interactive_map.html     ← open in browser!
```

---

## 🔍 Methods

### 1. Data Collection (`01_data_collection.py`)
- Downloads pedestrian and cycling networks from OpenStreetMap via **OSMnx**
- Retrieves all public transport stops (bus, tram, U-Bahn, S-Bahn)
- Collects urban facility locations (hospitals, schools, supermarkets, parks, etc.)
- Reprojects all layers to **EPSG:25832** (UTM Zone 32N) for metric calculations

### 2. Accessibility Analysis (`02_accessibility_analysis.py`)
- Builds **isochrone catchment polygons** for walking (300/500/800 m) and
  cycling (500/1000/2000 m) around all PT stops
- Calculates **nearest PT stop distance** for every facility using a KD-tree
- Computes **coverage statistics**: % of study area within each catchment
- Summarises accessibility rates by facility type

### 3. Static Maps (`03_static_maps.py`)
| Map | Description |
|-----|-------------|
| Network overview | Walk + cycle network + PT stops by mode |
| Isochrone map | Walking catchment rings (300/500/800 m) |
| Facility dot map | Facilities coloured by distance to nearest PT stop |
| Coverage chart | Bar chart of % study-area coverage by threshold |

### 4. Interactive Dashboard (`04_interactive_map.py`)
- Fully interactive HTML map built with **Folium**
- Toggle layers on/off: networks, catchments, stops, facilities
- Click any marker for popup details (mode, distance to PT, accessibility flag)
- Built-in measure tool and fullscreen mode

---

## ⚙️ Installation

```bash
# Clone the repo
git clone https://github.com/YOUR_USERNAME/urban-mobility-accessibility.git
cd urban-mobility-accessibility

# Install dependencies
pip install geopandas osmnx folium matplotlib pandas numpy scipy shapely
```

---

## 🚀 Usage

Run the scripts in order:

```bash
python 01_data_collection.py      # ~1–2 min (downloads OSM data)
python 02_accessibility_analysis.py
python 03_static_maps.py
python 04_interactive_map.py
```

Then open `outputs/interactive_map.html` in any browser.

To analyse a different city, change the `PLACE` variable in `01_data_collection.py`:
```python
PLACE = "Prenzlauer Berg, Berlin, Germany"
```

---

## 📊 Key Results (Schwabing, Munich)

| Mode  | Threshold | Study area covered |
|-------|-----------|--------------------|
| Walk  | 300 m     | 93.1 %             |
| Walk  | 500 m     | 99.8 %             |
| Walk  | 800 m     | 100.0 %            |
| Cycle | 500 m     | 99.8 %             |
| Cycle | 1000 m    | 100.0 %            |

> Schwabing shows excellent PT coverage — virtually the entire district is within
> a 500 m walk of a public transport stop, consistent with Munich's dense transit network.

---

## 🛠️ Tech Stack

| Library     | Purpose                                  |
|-------------|------------------------------------------|
| GeoPandas   | Spatial data processing & analysis       |
| OSMnx       | OpenStreetMap network & feature download |
| Folium      | Interactive web maps                     |
| Matplotlib  | Static publication-quality maps          |
| Shapely     | Geometric operations (buffers, unions)   |
| SciPy       | KD-tree nearest-neighbour queries        |
| Pandas      | Tabular data & statistics                |

---

## 📄 Data Sources

All geospatial data is sourced from **OpenStreetMap** © OpenStreetMap contributors,
available under the [ODbL licence](https://www.openstreetmap.org/copyright).

---

## 👤 Author

**[Your Name]** — MSc Transport and Mobility Planning, TU Munich  
Course: Traffic Data Collection and Analysis (TDCA), SoSe 2026
