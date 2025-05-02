module Main where

import Graphics.Gloss
import Equations
import Orbit


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


main :: IO ()
main = do

  animateOrbit neptune 2000 7.8  0 -- alt == km ; vel == km/s

  {-

  if planet clicked do some sort of switch case to reinnitiate sim

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
  -}
