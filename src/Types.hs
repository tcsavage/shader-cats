module Types where

import Linear

type Scalar = Double

type Color = V3 Float

data ShaderGlobals = ShaderGlobals { gPosition :: V3 Scalar
                                   , gNormal :: V3 Scalar
                                   , gIncidentRay :: V3 Scalar
                                   , gDepth :: Scalar
                                   }
