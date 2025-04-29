module Equations where 

{-

Current resources for equtions : 
** https://www.physicsclassroom.com/class/circles/lesson-4/mathematics-of-satellite-motion ** 
** Space Mission Analysis and Design book ** 

-}


-- G (universal gravitational constant) ~~ 6.6743 * 10^-11 N*m^2/kg^2
gravConst :: double 
gravConst = 6.6743e-11

-- Orbital Speed Equation ( v = ((G * M) / R)^(1/2) )

velocity :: double -> double -> double 
velocity massP radiO = sqrt((gravConst * massP) / radiO)


-- Acceleration ( a = (G * M) / R^2 )

acceleration :: double -> double -> double 
acceleration massP radiO = (gravConst * massP) / (radiO ^ 2)


-- Orbital Period ( T = ((4 * pi * R^3) / (G * M))^(1/2) = (2 * pi * R) / v )

orbitPeriod :: double -> double -> double 
orbitPeriod radiO v = (2 * pi * radiO) / v


-- Alltitude ( (Radius of orbit) - (Radius of planet) -->  Allt. = ((G * M)/a)^(1/2) - Rplanet )

alltitude :: double -> double -> double -> double 
alltitude massP acc radiP = (sqrt ((gravConst * massP) / acc )) - radiP

