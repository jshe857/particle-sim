module Test where

import Test.QuickCheck
import World
import Simulation
import Physics
import TestSupport

prop_EnergyConservation :: World -> Bool
prop_EnergyConservation  w = isEnergyConserved w


isEnergyConserved :: World -> Bool
isEnergyConserved w@(World _ _ _ _) = abs (realToFrac energyDiff) < World.epsilon
  where
    energyDiff =  (worldEnergy w) - worldEnergy (advanceWorld "" (0.001) w)




