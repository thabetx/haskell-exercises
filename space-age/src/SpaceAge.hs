module SpaceAge (Planet(..), ageOn) where

data Planet = Earth | Mercury | Venus | Mars | Jupiter | Saturn | Uranus | Neptune

ageOn :: Planet -> Float -> Float

modifier :: Planet -> Float
modifier Earth   = 1
modifier Mercury = 0.2408467
modifier Venus   = 0.61519726
modifier Mars    = 1.8808158
modifier Jupiter = 11.862615
modifier Saturn  = 29.447498
modifier Uranus  = 84.016846
modifier Neptune = 164.79132

ageOn planet seconds = seconds / 31557600 / modifier planet
