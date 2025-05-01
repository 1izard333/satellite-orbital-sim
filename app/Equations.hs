module Equations where

import Linear

{-
Current resources for equations:
** https://www.physicsclassroom.com/class/circles/lesson-4/mathematics-of-satellite-motion ** 
** Space Mission Analysis and Design book ** 
-}

-- Universal gravitational constant (N*m^2/kg^2)
gravConst :: Double 
gravConst = 6.6743e-11

-- Orbital Speed Equation (v = √(G*M/R))
velocity :: Double -> Double -> Double 
velocity massP radiO = sqrt((gravConst * massP) / radiO)

-- Centripetal acceleration (a = G*M/R²)
acceleration :: Double -> Double -> Double 
acceleration massP radiO = (gravConst * massP) / (radiO ** 2.0)

-- Orbital Period (T = 2πR/v)
orbitPeriod :: Double -> Double -> Double 
orbitPeriod radiO v = (2 * pi * radiO) / v

-- Altitude (alt = √(G*M/a) - R_planet)
altitude :: Double -> Double -> Double -> Double 
altitude massP acc radiP = (sqrt ((gravConst * massP) / acc )) - radiP

