module Equations where

import Linear

{-
Current resources for equations:
** https://www.physicsclassroom.com/class/circles/lesson-4/mathematics-of-satellite-motion ** 
** Space Mission Analysis and Design book ** 
-}

type Position = V2 Double
type Velocity = V2 Double
type Acceleration = V2 Double

data OrbitalInfo = MkOrbitalInfo
  { pos :: Position  -- position (m)
  , vel :: Velocity  -- velocity (m/s)
  } deriving (Show)

-- Vector space operations for OrbitalInfo
addOrbital :: OrbitalInfo -> OrbitalInfo -> OrbitalInfo
addOrbital (MkOrbitalInfo p1 v1) (MkOrbitalInfo p2 v2) =
  MkOrbitalInfo (p1 + p2) (v1 + v2)

scaleOrbital :: Double -> OrbitalInfo -> OrbitalInfo
scaleOrbital s (MkOrbitalInfo p v) =
  MkOrbitalInfo (s *^ p) (s *^ v)

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

-- Calculate gravitational acceleration at a position
gravAccel :: Double -> Position -> Acceleration
gravAccel mass (V2 x y) =
  let r = sqrt (x*x + y*y)
      a = -gravConst * mass / (r**3)
  in V2 (a * x) (a * y)

-- State derivative for RK4
stateDerivative :: Double -> OrbitalInfo -> OrbitalInfo
stateDerivative mass (MkOrbitalInfo p v) =
  MkOrbitalInfo v (gravAccel mass p)

-- Single simulation step (Runge-Kutta 4th order)
simulateStep :: Double -> Double -> OrbitalInfo -> OrbitalInfo
simulateStep mass dt state =
  let k1 = stateDerivative mass state
      k2 = stateDerivative mass (addOrbital state (scaleOrbital (dt/2) k1))
      k3 = stateDerivative mass (addOrbital state (scaleOrbital (dt/2) k2))
      k4 = stateDerivative mass (addOrbital state (scaleOrbital dt k3))
      derivative = scaleOrbital (1/6) $
                   addOrbital k1 $
                   addOrbital (scaleOrbital 2 k2) $
                   addOrbital (scaleOrbital 2 k3) k4
  in addOrbital state (scaleOrbital dt derivative)

-- Generate orbit trajectory
generateOrbit :: Double -> OrbitalInfo -> Double -> Double -> [OrbitalInfo]
generateOrbit mass initialState dt duration =
  take (floor (duration / dt)) $
  iterate (simulateStep mass dt) initialState