module Orbit where

import Graphics.Gloss
import Equations

type Altitude = Float -- in km
type Velocity = Float -- in km/s
type Eccentricity = Float 

-- Planet data to hold necessary values 
data Planet = 
    MkPlanet {
        planetId :: Int ,  -- corresponds with closeness to sun
        planetMass :: Float , -- in kg
        planetRadius :: Float  -- in km
    } 


animateOrbit :: Planet -> Altitude -> Velocity -> Eccentricity -> IO ()
animateOrbit planet alt vel ecc = 
    let
      scaleFactor = 1e5 -- scale to screen --> may change later

      -- convert to meter units
      radiO_meters = kmToMeters(planetRadius planet) + kmToMeters(alt) 
      v = kmToMeters vel
      
      -- get period and angular frequency
      period = orbitPeriod radiO_meters v
      w = 2 * pi / period

      -- set semi-minor axis 
      b = semiMinorAxis radiO_meters ecc


      -- make display pane 
      display = InWindow "Orbit Simulation" (800, 800) (100, 100)

      satellitePos t =  (realToFrac x / scaleFactor, realToFrac y / scaleFactor)
        where
            t' = realToFrac t :: Float
            x = radiO_meters * cos (t' * w)
            y = b * sin (t' * w)

        
      render t = pictures [ color blue (translate 0 0 (circleSolid ((kmToMeters(planetRadius planet)) / scaleFactor)))  -- planet
                          , color red (translate x y (circleSolid 5))    -- satellite
                          , color white (scale 1 (b / radiO_meters) (circle (realToFrac (radiO_meters / scaleFactor))))]
        where 
            (x, y) = satellitePos t

  in 
    animate display black render