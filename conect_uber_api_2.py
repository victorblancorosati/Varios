# -*- coding: utf-8 -*-
"""
Created on Tue Jul  5 15:15:19 2022

@author: vblanco
"""

import geopy as ge
import requests
import json
import datetime
import pandas as pd

service = ge.Nominatim(user_agent = "paracredicorp@gmail.com")

origen = service.geocode("Calle 50, Panama")

print(origen.latitude)
print(origen.longitude)


destino = service.geocode("Multiplaza, Panama")

print(destino.latitude)
print(destino.longitude)


def get_ride_price(origin_latitude, origin_longitude, destination_latitude, destination_longitude):
    result1 = pd.read_csv('result1.csv')
    url = "https://www.uber.com/api/loadFEEstimates?localeCode=en"

    payload = json.dumps({
      "origin": {
        "latitude": origin_latitude,
        "longitude": origin_longitude
      },
      "destination": {
        "latitude": destination_latitude,
        "longitude": destination_longitude
      },
      "locale": "en"
    })
    headers = {
      'content-type': 'application/json',
      'user-agent': 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/103.0.0.0 Safari/537.36',
      'x-csrf-token': 'x'
    }

    response = requests.request("POST", url, headers=headers, data=payload)
    result = [[ datetime.datetime.now(), x['vehicleViewDisplayName'], x['fareString']] for x in response.json()['data']['prices']]
    result = pd.DataFrame(result)
    result.rename(columns= { 0: 'fecha', 1: 'tipo', 2:'valor'}, inplace=True)
    result1 = pd.DataFrame(result)   ## 1) primera vez
    #result1 = result1.append(result, ignore_index=True)
    result1.rename(columns= { 0: 'fecha', 1: 'tipo', 2:'valor'}, inplace=True)
    result1.to_csv('result1.csv', header=True, index=False)
    return result1


print(get_ride_price(origen.latitude, origen.longitude, destino.latitude, destino.longitude))
