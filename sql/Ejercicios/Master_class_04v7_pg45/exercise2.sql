/*
• Import optd_aircraft.csv and optd_airlines.csv in postgres
( /Data/opentraveldata/)

What number of engines is most common on airplanes?
(optd_aircraft)
*/

SELECT nb_engines, COUNT(*)
FROM optd_aircraft
GROUP BY nb_engines
ORDER BY count desc
LIMIT 1;
