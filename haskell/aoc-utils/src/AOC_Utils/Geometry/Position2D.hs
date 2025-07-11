module AOC_Utils.Geometry.Position2D ( Position2D(..) ) where

data Position2D a = Position2D
    { x :: a
    , y :: a
    } deriving (Eq, Ord, Show)
