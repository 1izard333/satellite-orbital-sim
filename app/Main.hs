module Main where

import Graphics.Gloss
import Equations
import Linear 

-- Planet data to hold necessary values 
data Planet = 
    MkPlanet {
        planetId :: Int ,  -- corresponds with closeness to sun
        planetMass :: Double , -- in kg
        planetRadius :: Double  -- in km
    } 

-- planets list ( https://nssdc.gsfc.nasa.gov/planetary/factsheet/planet_table_ratio.html )

mercury :: Planet
mercury = MkPlanet 1 0.33010e24 2440.5

venus :: Planet
venus = MkPlanet 2 4.8673e24 6051.8

earth :: Planet
earth = MkPlanet 3 5.9722e24 6378.137

mars :: Planet
mars = MkPlanet 4 0.64169e24 3396.2

jupiter :: Planet
jupiter = MkPlanet 5 1898.13e24 71492

saturn :: Planet
saturn = MkPlanet 6 568.32e24 60268

uranus :: Planet
uranus = MkPlanet 7 86.811e24 25559

neptune :: Planet
neptune = MkPlanet 8 102.409e24 24764

moon :: Planet
moon = MkPlanet 0 0.07346e24 1738.1


-- ... (keep your existing Planet data and planet definitions)

-- Convert meters to screen coordinates
scaleToScreen :: Double -> V2 Double -> Point
scaleToScreen scale (V2 x y) =
  (realToFrac x / realToFrac scale, realToFrac y / realToFrac scale)

-- Render the planet and orbit
renderOrbit :: Planet -> [OrbitalInfo] -> Picture
renderOrbit planet states =
  let scaleFactor = 1e6  -- Adjust this for proper scaling
      planetRadiusScreen = realToFrac (planetRadius planet * 1000) / realToFrac scaleFactor
      orbitPath = line $ map (scaleToScreen scaleFactor . pos) states
      currentPos = scaleToScreen scaleFactor (pos (head states))
  in pictures
     [ color blue (circleSolid planetRadiusScreen)  -- Planet
     , color white orbitPath                      -- Orbit path
     , translate (fst currentPos) (snd currentPos) $
       color red (circleSolid 5)                  -- Satellite
     ]

-- Create initial orbital state
initialState :: Planet -> Double -> Double -> OrbitalInfo
initialState planet altitude initialSpeed =
  let r = planetRadius planet * 1000 + altitude  -- Convert km to m
      position = V2 r 0
      velocity = V2 0 initialSpeed
  in MkOrbitalInfo position velocity

-- Animation function
animateOrbit :: Planet -> Double -> Double -> IO ()
animateOrbit planet altitude speed = do
  let initState = initialState planet altitude speed
      dt = 100  -- Time step in seconds
      duration = 3600 * 2  -- 2 hours in seconds
      orbit = generateOrbit (planetMass planet) initState dt duration
      display = InWindow "Orbit Simulation" (800, 800) (10, 10)
  animate display black (const (renderOrbit planet orbit))


main :: IO ()
main = do
  putStrLn "Select a planet:"
  putStrLn "1. Mercury"
  putStrLn "2. Venus"
  -- ... (add other planets)
  putStrLn "Enter planet number:"
  planetChoice <- readLn
  
  putStrLn "Enter altitude (meters):"
  altitude <- readLn
  
  putStrLn "Enter initial speed (m/s):"
  speed <- readLn
  
  let planet = case planetChoice of
                 1 -> mercury
                 2 -> venus
                 3 -> earth
                 4 -> mars
                 5 -> jupiter
                 6 -> saturn
                 7 -> uranus 
                 8 -> neptune
                 0 -> moon
  animateOrbit planet altitude speed
