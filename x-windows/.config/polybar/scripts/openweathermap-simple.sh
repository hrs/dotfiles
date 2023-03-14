#!/bin/sh

get_icon() {
  case $1 in
    # Icons from Font Awesome 6

    # clear sky
    01d) icon="";;
    01n) icon="";;

    # few clouds
    02d) icon="";;
    02n) icon="";;

    # scattered clouds
    03d) icon="";;
    03n) icon="";;

    # broken clouds
    04d) icon="";;
    04n) icon="";;

    # shower rain
    09d) icon="";;
    09n) icon="";;

    # rain
    10d) icon="";;
    10n) icon="";;

    # thunderstorm
    11*) icon="";;

    # snow 
    13*) icon="";;

    # mist
    50*) icon="";;

    *) icon="?";
  esac

  echo $icon
}

KEY=$(~/.bin/netrc password api.openweathermap.com)
CITY="Vancouver,CA"
UNITS="imperial"
SYMBOL="°"

API="https://api.openweathermap.org/data/2.5"

if [ -n "$CITY" ]; then
  if [ "$CITY" -eq "$CITY" ] 2>/dev/null; then
    CITY_PARAM="id=$CITY"
  else
    CITY_PARAM="q=$CITY"
  fi

  weather=$(curl -sf "$API/weather?appid=$KEY&$CITY_PARAM&units=$UNITS")
else
  location=$(curl -sf "https://location.services.mozilla.com/v1/geolocate?key=geoclue")

  if [ -n "$location" ]; then
    location_lat="$(echo "$location" | jq '.location.lat')"
    location_lon="$(echo "$location" | jq '.location.lng')"

    weather=$(curl -sf "$API/weather?appid=$KEY&lat=$location_lat&lon=$location_lon&units=$UNITS")
  fi
fi

if [ -n "$weather" ]; then
  weather_temp=$(echo "$weather" | jq ".main.temp" | cut -d "." -f 1)
  weather_icon=$(echo "$weather" | jq -r ".weather[0].icon")

  echo "$(get_icon "$weather_icon")" "$weather_temp$SYMBOL"
fi
