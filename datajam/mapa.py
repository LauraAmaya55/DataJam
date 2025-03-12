import pandas as pd
import folium
import pgeocode

data = pd.read_csv('encuesta.csv')

nomi = pgeocode.Nominatim('co')  
data['coords'] = data['codigo_postal'].apply(lambda x: nomi.query_postal_code(x)[['latitude', 'longitude']])
data[['lat', 'lon']] = pd.DataFrame(data['coords'].tolist(), index=data.index)

data['proporcion_inseguridad'] = data.groupby('codigo_postal')['inseguro'].transform('mean')

mapa = folium.Map(location=[4.6097, -74.0817], zoom_start=12)  

for idx, row in data.drop_duplicates('codigo_postal').iterrows():
    folium.CircleMarker(
        location=(row['lat'], row['lon']),
        radius=row['proporcion_inseguridad'] * 10,  
        color='red',
        fill=True,
        fill_color='red',
        fill_opacity=0.6,
        popup=f"Código Postal: {row['codigo_postal']}<br>Proporción Inseguridad: {row['proporcion_inseguridad']:.2f}"
    ).add_to(mapa)


mapa.save('mapa_inseguridad_bogota.html')
