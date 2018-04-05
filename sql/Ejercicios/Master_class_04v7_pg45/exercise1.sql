/*
• Import optd_aircraft.csv and optd_airlines.csv in postgres
( /Data/opentraveldata/)

• Which airplane has the highest number of engines?
(optd_aircraft)
*/

SELECT * FROM optd_aircraft
WHERE nb_engines IS NOT NULL
ORDER BY nb_engines
DESC LIMIT 1;
