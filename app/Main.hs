module Main where

import Brillo 
import Graphics.Gloss
import Equations

-- Planet data to hold necessary values 
data Planet = 
    MkPlanet {
        planetId :: Int ,  -- corresponds with closeness to sun
        planetMass :: Double , -- in kg
        planetRadius :: Double  -- in km
    } 

-- planets list ( https://nssdc.gsfc.nasa.gov/planetary/factsheet/planet_table_ratio.html )

mercury :: Planet
mercury = Planet 1 0.33010e24 2440.5

venus :: Planet
venus = Planet 2 4.8673e24 6051.8

earth :: Planet
earth = Planet 3 5.9722e24 6378.137

mars :: Planet
mars = Planet 4 0.64169e24 	3396.2

jupiter :: Planet
jupiter = Planet 5 1898.13e24 71492

saturn :: Planet
saturn = Planet 6 568.32e24 60268

uranus :: Planet
uranus = Planet 7 86.811e24 25559

neptune :: Planet
neptune = Planet 8 102.409e24 24764

moon :: Planet
moon = Planet 0 0.07346e24 1738.1



main :: IO ()
main = putStrLn "Hello, Haskell!"
