:- use_module(library(http/json)).
:- use_module(library(http/http_open)).
:- use_module(library(url)).
:- use_module(library(http/http_server)).

weatherApiUrl('http://127.0.0.1:5001/api/weather').

getWeatherURL(LatLon, Time, Url) :-
    weatherApiUrl(ApiUrl),
    term_string(LatLon, LatLonString),
    term_string(Time, TimeString),
    format(string(Decoded), '~s?lat_lon=~s&time=~s', [ApiUrl, LatLonString, TimeString]),
    url_iri(Url, Decoded).

getWeatherCall(LatLon, Time, R) :-
    getWeatherURL(LatLon, Time, Url),
    setup_call_cleanup(
      http_open(Url, In, [request_header('Accept'='application/json')]),
      json_read_dict(In, WeatherData),
      close(In)
    ),
    format(string(R), 'Descriere: ~s. Temperatura: ~2f C. Se simte ca: ~2f C. Umiditate: ~d%', [ WeatherData.description, WeatherData.temp, WeatherData.feels_like, WeatherData.humidity]).
