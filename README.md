This project focuses on the implementation of a dialogue manager module inside a currently in-development minimalistic voice assistant application. Users of this application will be able to communicate with it in Romanian and issue simple voice commands (current iteration focuses only on the first three), such as:
* querying the weather
* adding events/appointments to a calendar
* querying the aforementioned calendar
* playing music
* turning the lights on/off

The dialogue manager is a component inside the project which is responsible for the interpretation of given commands and their parameters, deciding whether they are correctly specified or not, retrieving information from APIs (weather, calendar, etc.), and issuing a meaningful response based on given input.
# Modules

![sytem diagram](https://github.com/MarcusGitAccount/home_assistant_ro/blob/master/images/system%20diagram.png)

<!-- - idea behind each module
- modules connection
- flow example? -->

The above diagram illustrates the system's structure and how it is divided in several interacting modules, each of which is going to be detailed in the following sub-sections of this document.

The dialogue manager is essentially based on two intercommunicating servers, one written in Prolog, which employs a finite state machine logic in order to handle user communication and requests, and the other written in Python, which handles actuall API calls and authentications.

## Knowledge base representation

The knowledge base is written using Prolog and divided in several files:
* a main file, ```kb.pl```
* additional files, one for each intent: ```calendar_kb.pl``` and ```weather_kb.pl```

### ***kb<span>.pl***
*Intents* are our chosen representation of user-given commands; they can be specified using the ```intent/1``` predicate. Intents contain several *entities*, each corresponding to one of their parameters, which can be specified using the ```entity/3``` predicate. The three arguments are: the intent to which the entity corresponds, the name of the entity, and its value. For example, an *add to calendar* intent may contain entities such as: event title, date, starting time, ending time etc.

* Some entities have default values which are stored using the ```default/4``` predicate, such as:
```Prolog
default(calendarAsk, ora_final, [time(23, 59, 59)], 'sfarsitul zilei').
```

* Most entities' values require some transformation, as they do not contain information that can be handled by the application. For example, a value of _tomorrow_ for the _time_ entity inside a _queryWeather_ intent might be easily understood by a human, but it is of no use to our weather module that expects timestamps. We tackled this matter by defining a ```relative/5``` predicate which transforms humanly readable entity values so that they can be further processed by other modules. For example:
```Prolog
relative(calendarAsk, ora_inceput_relativ, 'dupa-masa', R, Mesaj) :-
    R = [time(12, 0, 0), time(18, 0, 0)],
    Mesaj = 'dup\u0103-mas\u0103'.
```

* Some entities might be missing because they have not been specified by the user.

Finally, the `getEntitiesValues/3` predicate performs transformations using ```relative``` predicates for each entity in the list of entities given as parameter. Additionally, if the third parameter is set to _true_, it will replace missing entities with their default values. The transformed values are asserted using ```finalEntity/4``` and ```missingEntity/2``` predicates. In the case of a _queryWeather_ intent with a specified time of tomorrow and a missing location parameter, the flow of execution would look like this:

![entity replacement](https://github.com/MarcusGitAccount/home_assistant_ro/blob/master/images/entity_replacement.jpg)

### ***calendar_kb<span>.pl*** and ***weather_kb<span>.pl***

These files contain definitions for `relative` predicates in the case of multiple possible values of different entities, such as:
```Prolog
relative(calendarAsk, data, 'maine', R, Mesaj) :-
    date_get(tomorrow, Tomorrow),
    R = [Tomorrow],
    Mesaj = 'm\u00e2ine'.
```
```Prolog
relative(queryWeather, timp, 'peste o ora', R, Mesaj) :-
    R = ['hourly', 1],
    Mesaj = 'peste o or\u0103'.
```
It is worth mentioning the extensive use of the [Prolog date_time library](https://github.com/fnogatz/date_time) in order to compute time intervals, date differences and so on.

### ***calendar.<span>pl*** and ***weather.<span>pl***

These additional files define predicates that wrap the http calls made by the Prolog server to the Python service module and handle the received responses.

```Prolog
getWeatherCall(LatLon, Time, R) :-
    % Build URL for call
    getWeatherURL(LatLon, Time, Url),

    % Perform http call and receive JSON response
    setup_call_cleanup(
      http_open(Url, In, [request_header('Accept'='application/json')]),
      json_read_dict(In, WeatherData),
      close(In)
    ),

    % Reformat the response as a string
    format(string(R), ...).
```

## Finite automata

![fsm diagram](https://github.com/MarcusGitAccount/home_assistant_ro/blob/master/images/fsm.PNG)

<!-- - states
- states switch
- intent observer -->

The finite state machine is written using Prolog and is running under a server that handles http requests which pass JSON data containing intents and their entities. Predicates such as `currentState/1`, `performState/1`, and `switchState/1` are used to design the execution of the FSM.

* `currentState` simply asserts which state the automata is in at a given moment.
* `performState` has a separate definition for each state and contains various actions that have to be executed during that particular state. It also switches the current state the FSM is in.
* `switchState` is used to switch between states and simultaneously log messages to the console and perform clean-up actions.

For example, the actions for a _weather call_ state would look like this:
```Prolog
performState(weatherApiCall) :-
    % Assert the current FSM state
    currentState(weatherApiCall),

    % Check the existence of entities generated using the knowledge base
    finalEntity(queryWeather, loc, LatLon, Location),
    finalEntity(queryWeather, timp, Time, T),

    % Call the weather Prolog module and log messages
    log('Weather api call'),
    getWeatherCall(LatLon, Time, Desc),
    format(string(Message), 'Vremea ~s ~s. ~s.', [T, Location, Desc]),
    assertz(message(Message)),

    % Switch to the next state
    switchState(respond).
```

## Service module

<!-- - apis adapter, authentication
- caching
- flask server -->

The Python module is running as a server written using the Flask framework. This server handles incoming requests from Prolog modules and issues calls to [Google API](https://developers.google.com/calendar) (calendar add/update requests) and [OpenWeatherMap API](https://openweathermap.org/) (weather querying requests). The [OAuth](https://developers.google.com/calendar/auth) standard is used in order to access the Google API.

In the case of weather API calls, the Python module employs a locally stored cache file that is accessed whenever weather querying requests are received from the Prolog server. Actual calls to the API are made to update the cache data only if it is older than 60 minutes, thus providing quicker responses in the event of consecutive weather querying requests from the user.

```python
def get_weather(lat, lon):
    # ...
    if key in cache:
        print('Found cached data')
        keys = list(cache[key])
        # ...
        if minutes > 60:
            print('Cached data too old')
        else:
            # Return cached data
    
    # Issuse an API request...
    # url = ...
    response = req.get(url)
```

The Python module sends http responses back to the Prolog server in the form of JSON data.

```python
@app.route('/api/calendar/update/', methods = ['POST']) 
def calendar_api_update():
  if request.method == 'POST':
    # ...
    return json.dumps(event)

# ...
# ...
# ...

@app.route('/api/weather', methods=['GET'])
def weather_api():
if request.method == 'GET':
    # ...
    if time[0] == 'daily':
      # ...
    else:
      # ...
      response['temp'] = data['temp']
      response['feels_like'] = data['feels_like']
      response['humidity'] = data['humidity']
      response['description'] = data['weather'][0]['description']

    return json.dumps(response, indent=2, ensure_ascii=False)
```

## Existing modules

<!-- - NLU(intents representation and generation)
- text to speech
- speech to text -->

Other modules include the speech-to-text module, which is developed using the [RASA NLU framework](https://rasa.com/docs/rasa/nlu/about/). The training datasets containing sentences used for learning are generated using [Chatito](https://rodrigopivi.github.io/Chatito/). Example content of one of the .chatito files:
```C#
%[answer]('training': '90', 'testing': '30')
    @[positive]
    @[negative]

@[positive]
    Da
    Da, este ok
    Sigur
    Desigur
    Așa facem
    Bine
    Continuă tot așa
    Continuă
    Okay
    Ok
    Îmi place

@[negative]
    Nu
    Nu așa
    Nu este ok
    Deloc
    Nu sunt de acord