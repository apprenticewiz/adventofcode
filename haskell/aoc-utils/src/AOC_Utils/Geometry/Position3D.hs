module AOC_Utils.Geometry.Position3D ( Position3D(..) ) where

data Position3D a = Position3D
    { x :: a
    , y :: a
    , z :: a
    } deriving (Eq, Ord, Show)
