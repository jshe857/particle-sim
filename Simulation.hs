module Simulation (moveParticle, accelerate, advanceWorld) where
  
import World
import Physics

-- Move a particle according to its velocity for the given number of (simulated) seconds.
--
moveParticle :: Float -> Particle -> Particle
moveParticle (dt) (Particle m (x,y) v@(dx,dy)) = Particle m (x + dx*dt, y +dy*dt) v 

-- Accelerate a particle in dependence on the gravitational force excerted by all other particles for
-- the given number of (simulated) seconds.
--

-- bug here, need to find out all a's before updating
accelerate :: Float -> [Particle] -> [Particle]
accelerate dt ps = map (accelerateParticle dt) $ zip ps (resolveForces ps ps)
  where
    resolveForces :: [Particle] -> [Particle] -> [Accel]
    resolveForces [] _ = []
    resolveForces (p:rest) (allPs) = a : (resolveForces rest allPs)
      where
        a = sumAccels $ map (force p) allPs
    

sumAccels :: [Accel] -> Accel
sumAccels [] = (0,0)
sumAccels ((ax,ay):others) = (ax + otherx, ay + othery)
  where
    (otherx,othery) = sumAccels (others)


accelerateParticle :: Float -> (Particle, Accel) -> Particle
accelerateParticle dt ((Particle m p (dx,dy)), (ax,ay)) = Particle m p (dx + dt*ax, dy + dt*ay)


advanceWorld :: unused -> Float -> World -> World
advanceWorld _ dt (World s1 s2 s3 ps) = World s1 s2 s3 newPs
  where
    newPs = map (moveParticle (dt*s3)) $ accelerate (dt*s3) ps


