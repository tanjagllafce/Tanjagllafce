--Selecting all the relevant refrences from the dataset
SELECT 
  unique_key, 
  fare, 
  tips, 
  tolls, 
  payment_type, 
  extras, 
  trip_total, 
  taxi_id, 
  company, 
  pickup_location, 
  dropoff_community_area,
  pickup_community_area,
  dropoff_location, 
  trip_start_timestamp, 
  trip_end_timestamp, 
  trip_seconds, 
  trip_miles 

--From the city taxi trips 
 FROM `bigquery-public-data.chicago_taxi_trips.taxi_trips` 

--Filtering the values to ensure we have no null values 
 WHERE unique_key IS NOT NULL 
 AND taxi_id IS NOT NULL
 AND trip_start_timestamp IS NOT NULL
 AND trip_end_timestamp IS NOT NULL
 AND trip_seconds IS NOT NULL
 AND trip_miles IS NOT NULL
 AND fare IS NOT NULL 
 AND dropoff_community_area IS NOT NULL
 AND pickup_community_area IS NOT NULL
 AND tips IS NOT NULL
 AND tolls IS NOT NULL
 AND extras IS NOT NULL
 AND trip_total IS NOT NULL 
 AND payment_type IS NOT NULL 
 AND company IS NOT NULL
 AND pickup_location IS NOT NULL
 AND dropoff_location IS NOT NULL 
 
--Select only dropoffs from chicago airports (Ohare and Midway)
AND 
   dropoff_community_area IN (56, 76, 64)  -- Dropoffs from Ohare and Midway

--filtering for rides that are at least one mile in distance 
AND trip_miles >= 1

--Filtering for rides that are between 3 minutes- 3 hours 
AND trip_seconds BETWEEN 180 AND 10800

--Filtering for the year 2023
AND EXTRACT(YEAR FROM trip_start_timestamp) = 2023 --ensure that the ride starts in 2023
AND EXTRACT(YEAR FROM trip_end_timestamp) = 2023; --and ends in 2023



