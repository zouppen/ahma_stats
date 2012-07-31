# CouchDB view for temperature readings

Install this with couchapp. See README.md in root directory for more
information.

## Supported features

The collector has 4 temperature sources: one for device, one for room
temperature and two for air temperature. Air temperature sensors are
placed in the opposite sides of the building, north and south. Because
of the sunlight, the measurement on the sunny side is skewed. To
overcome this problem, minimum of these two sensors is picked.

That approach will give somewhat too small temperatures because the
sensors have about 1 centigrade accuracy. That results circa 0.5
centigrade temperature drop compared to arithmetic mean when the sky
is cloudy. That's a very small drawback compared to a case where one
sensor is in direct sunlight. In that case it's possible to get 10
centigrade skew if that algorithm were used.

Features per measurement source:

* Minimum and maximum temperature
* Timestamps of minimum and maximum points
* Sum of measurements
* Number of measurements collected

Note there's no arithmetic mean (=average) of the temperature. To get
that we calculate that in user application by dividing sum of
measurements by number of measurements.
