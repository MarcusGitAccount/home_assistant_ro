:- use_module(library(http/json)).
:- use_module(library(http/http_open)).

api_key("042cfb53b0542daeac4dcffb8bc01246").
api_weather_call_url(URL, City) :- api_key(API_Key), format(string(URL), 'http://api.openweathermap.org/data/2.5/weather?q=~s&appid=~s&units=metric&lang=ro', [City, API_Key]).
api_forecast_call_url(URL, City) :- api_key(API_Key), format(string(URL), 'http://api.openweathermap.org/data/2.5/forecast?q=~s&appid=~s&units=metric&lang=ro', [City, API_Key]).


%! get_weather_data(-Data, +City) is det.
%  get JSON response from OpenWeatherMap API for corresponding City
get_weather_data(Data, City) :-
    api_weather_call_url(URL, City), % Build URL for API call
    setup_call_cleanup(
        http_open(URL, In, [request_header('Accept'='application/json')]),
        json_read_dict(In, Data),
        close(In)
    ).

%! get_forecast_data(-Data, +City, +Offset)
%  get JSON response from API for weather forecast at Offset * 3 hours from now
get_forecast_data(Data, City, Offset) :-
    integer(Offset),
    api_forecast_call_url(URL, City), 
    setup_call_cleanup(
        http_open(URL, In, [request_header('Accept'='application/json')]),
        json_read_dict(In, ForecastData),
        close(In)
    ),
    nth0(Offset, ForecastData.get(list), Data).


%! extract_info(+Data, -Desc, -Temp, -FeelsLike, -Humidity) is det.
%  extract information from json returned by weather API call
extract_info(Data, Desc, Temp, FeelsLike, Humidity) :-
    nth0(0, Data.get(weather), Weather),
    Desc = Weather.get(description),
    Main = Data.get(main),
    Temp = Main.get(temp),
    FeelsLike = Main.get(feels_like),
    Humidity = Main.get(humidity).

%! weather_now(-City) is det.
%  print out information about current weather in given city
weather_now(City) :-
    get_weather_data(Data, City),
    extract_info(Data, Desc, Temp, FeelsLike, Humidity),
    format('Vremea acum in ~s: ~s. Temperatura este de ~2f C, dar se simte ca ~2f C.\nNivelul de umiditate este de ~d%.', [City, Desc, Temp, FeelsLike, Humidity]).


%! weather_tomorrow(-City) is det.
%  print out information about tomorrow's weather (24h from now) in given city 
weather_tomorrow(City) :-
    get_forecast_data(Data, City, 8),
    extract_info(Data, Desc, Temp, FeelsLike, Humidity),
    format('Vremea peste 24h in ~s: ~a. Temperatura va fi de ~2f C, dar se va simti ca ~2f C.\nNivelul de umiditate va fi de ~d%.', [City, Desc, Temp, FeelsLike, Humidity]).