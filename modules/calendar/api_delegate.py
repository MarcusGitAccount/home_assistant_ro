
from __future__ import print_function

import datetime
import pickle
import os.path as path
import json
import requests as req

from googleapiclient.discovery import build
from google_auth_oauthlib.flow import InstalledAppFlow
from google.auth.transport.requests import Request

from flask import Flask, jsonify, request
from flask_cors import CORS, cross_origin

from datetime import datetime

# If modifying these scopes, delete the file token.pickle.
SCOPES = [
  'https://www.googleapis.com/auth/calendar.events',
  'https://www.googleapis.com/auth/calendar'
]
CALENDAR_ID = '7p6iutdbluv6bqbssbulkv0hug@group.calendar.google.com'
CREDENTIALS_FILENAME = 'service-account.json'

service = None

WEATHER_KEY = '042cfb53b0542daeac4dcffb8bc01246'
DIRNAME = path.dirname(__file__)
WEATHER_CACHE =  path.join(DIRNAME, '.cache')

def get_credentials():
  global service

  creds = None
  # The file token.pickle stores the user's access and refresh tokens, and is
  # created automatically when the authorization flow completes for the first
  # time.
  if path.exists('token.pickle'):
    with open('token.pickle', 'rb') as token:
      creds = pickle.load(token)
  # If there are no (valid) credentials available, let the user log in.
  if not creds or not creds.valid:
      if creds and creds.expired and creds.refresh_token:
          creds.refresh(Request())
      else:
          flow = InstalledAppFlow.from_client_secrets_file(CREDENTIALS_FILENAME, SCOPES)
          creds = flow.run_local_server(port=0)
      # Save the credentials for the next run
      with open('token.pickle', 'wb') as token:
          pickle.dump(creds, token)
  return creds

def get_events(start, end):
  global service

  events_result = service.events().list(calendarId=CALENDAR_ID, timeMin=start, timeMax=end,
                                      maxResults=10, singleEvents=True,
                                      orderBy='startTime').execute()
  events = events_result.get('items', [])
  return events

def insert_event(event_data):
  global service

  data = {
    'summary': event_data['summary'],
    'location': event_data['location'],
    'description': 'Event added by the Home Assistant',
    'start': {
      'dateTime': event_data['start'],
      # 'timeZone': 'Europe/Athens'
    },
    'end': {
      'dateTime': event_data['end'],
      # 'timeZone': 'Europe/Athens'
    },
    'reminders': {
      'useDefault': False,
      'overrides': [
        {'method': 'email', 'minutes': 24 * 60},
        {'method': 'popup', 'minutes': 10},
      ],
    },
  }
  
  if 'location' in event_data:
    data['location'] = event_data['location']
  
  event = service.events().insert(calendarId=CALENDAR_ID, body=data).execute()
  return event

def update_event(event_id, event_data):
  global service

  body = service.events().get(calendarId=CALENDAR_ID, eventId=event_id).execute()
  for key, val in event_data.items():
    if 'start' == key:
      body['start']['dateTime'] = val
    elif 'end' == key:
      body['end']['dateTime'] = val
    else:
      body[key] = val

  event = service.events().update(calendarId=CALENDAR_ID, eventId=event_id, body=body).execute()
  return event

app = Flask(__name__)
cors = CORS(app)
app.config['CORS_HEADERS'] = 'Content-Type'

@app.route('/api/calendar/', methods = ['GET', 'POST']) 
def calendar_api():
  if request.method == 'GET':
    start = request.args.get('start', type=str)
    end   = request.args.get('end',   type=str)

    events = get_events(start, end)
    return json.dumps({'items': events})
  
  elif request.method == 'POST':
    body = request.get_json()
    print('Body received:', body)
    event = insert_event(body)
    return json.dumps(event)

  return json.dumps({'error': 'Method not supported'}), 400

# Did not find a way to do put requests from Prolog...... bummer
@app.route('/api/calendar/update/', methods = ['POST']) 
def calendar_api_update():
  if request.method == 'POST':
    event_id = request.args.get('id', type=str)
    body = request.get_json()
    event = update_event(event_id, event_data=body)
    return json.dumps(event)

  return json.dumps({'error': 'Method not supported'}), 400

@app.route('/api/weather', methods=['GET'])
def weather_api():
  if request.method == 'GET':
    lat_lon = request.args.get('lat_lon', type=str, default=[46.770439, 23.591423])
    time = request.args.get('time', type=str)

    # Transform list represented as a string into real list
    lat_lon = lat_lon[1:-1:].split(',')
    time = time[1:-1:].split(',')  
    lat = float(lat_lon[0])
    lon = float(lat_lon[1])

    data = get_weather(lat, lon)
    response = {}

    if time[0] == 'daily':
      offset = int(time[1])
      time_of_day = time[2]
      data = data['daily'][offset]
      response['temp'] = data['temp'][time_of_day]
      response['feels_like'] = data['feels_like'][time_of_day]
      response['humidity'] = data['humidity']
      response['description'] = data['weather'][0]['description']
    else:
      if time[0] == 'current':
        data = data['current']
      elif time[0] == 'hourly':
        offset = int(time[1])
        data = data['hourly'][offset]

      response['temp'] = data['temp']
      response['feels_like'] = data['feels_like']
      response['humidity'] = data['humidity']
      response['description'] = data['weather'][0]['description']

    return json.dumps(response, indent=2, ensure_ascii=False)

def get_weather(lat, lon):
  cache = {}
  
  key = str((lat, lon))
  with open(WEATHER_CACHE, 'rb') as fd:
    content = fd.read()
    if content:
      cache = json.loads(content, encoding='utf-8')
      if key in cache:
        print('Found cached data')
        keys = list(cache[key])
        if len(keys) != 1:
          print('Invalid cache data')
        else:
          timestamp = keys[0]

          now = datetime.utcnow()
          then = datetime.fromtimestamp(int(timestamp))
          diff = now - then
          total = diff.total_seconds()
          minutes = total / 60
          if minutes > 60:
            print('Cached data too old')
          else:
            d = cache[key]
            return d[list(d.keys())[0]]

  print('Issuing request to weather api.')
  url = f'https://api.openweathermap.org/data/2.5/onecall?lat={lat}&lon={lon}&exclude=minutely&appid={WEATHER_KEY}&lang=ro&units=metric'
  response = req.get(url)
  utf_content = response.content.decode('utf-8')
  data = json.loads(utf_content, encoding='utf-8')
  
  with open(WEATHER_CACHE, 'wb') as fd:
    timestamp = datetime.utcnow().timestamp()
    cache[key] = {
      f'{round(timestamp)}': data
    }
    print('Writing response to cache')
    to_write = json.dumps(cache, ensure_ascii=False).encode('utf-8')
    fd.write(to_write)
    
  return data

def main():
  global service

  creds = get_credentials()
  service = build('calendar', 'v3', credentials=creds)

  # events = get_events("2020-06-02T21:32:52+03:00", "2020-06-20T21:32:52+03:00")
  # print(json.dumps(events, indent=2))
  # # inserted = insert_event(service, {'summay': 'API test 1', 'start': "2020-06-12T21:30:00+03:00", 'end': "2020-06-12T22:30:00+03:00"})
  # # print(json.dumps(inserted, indent=2))
  # id = "ek0v1i7u9gq5jm8uhrhek9jmok"
  # updated = update_event(id, {'summary': 'Modified title ayy'})
  # print(json.dumps(updated, indent=2))
  
  app.run(port=5001, host='127.0.0.1', debug=True)

  # get_weather('Cluj-Napoca')

if __name__ == '__main__':
  main()