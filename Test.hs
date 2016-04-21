module Test where

import Test.QuickCheck
import World
import Simulation
import Physics
import TestSupport

prop_EnergyConservation :: World -> Bool
prop_EnergyConservation  w = isEnergyConserved w


isEnergyConserved :: World -> Bool
isEnergyConserved w = (realToFrac (energyDiff/oldEnergy)) < World.epsilon
  where
    oldEnergy = worldEnergy w
    energyDiff =  abs (oldEnergy - worldEnergy (advanceWorld "" (0.001) w))




