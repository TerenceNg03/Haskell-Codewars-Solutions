{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Solutions.Spiral where

import Control.Applicative ((<|>))
import Control.Lens (makeLenses, (%=), (.=))
import Control.Monad.State (MonadState (get), State, execState)
import Data.Set (Set)
import qualified Data.Set as Set

type Point = (Int, Int)

data Direction = U | D | L | R
    deriving (Show)

data Spiral = Spiral
    { _plane :: Set (Int, Int)
    , _direction :: Direction
    , _position :: Point
    , _size :: Int
    }

makeLenses ''Spiral

isOutOfBound :: Int -> Point -> Bool
isOutOfBound limit (x, y) =
    x < 0 || y < 0 || x >= limit || y >= limit

checkCollision :: Set (Int, Int) -> Int -> (Point, Point, Point) -> Bool
checkCollision set limit (a, b, c) =
    let checkCollision' p = not (isOutOfBound limit p) && Set.member p set
     in checkCollision' a || checkCollision' b || checkCollision' c

possibleCollisions :: Point -> Direction -> (Point, Point, Point)
possibleCollisions (x, y) direction' =
    let u = (x - 1, y)
        d = (x + 1, y)
        l = (x, y - 1)
        r = (x, y + 1)
     in case direction' of
            U -> (u, l, r)
            D -> (l, d, r)
            L -> (u, d, l)
            R -> (u, d, r)

nextStep :: State Spiral (Maybe (Point, Direction))
nextStep = do
    Spiral{_position = (x, y), ..} <- get
    let u = (x - 1, y)
        d = (x + 1, y)
        l = (x, y - 1)
        r = (x, y + 1)
        next = case _direction of
            U -> (u, U)
            D -> (d, D)
            L -> (l, L)
            R -> (r, R)
        nextTurn = case _direction of
            U -> (r, R)
            D -> (l, L)
            L -> (u, U)
            R -> (d, D)
        stepOK (p, d') =
            not $
                isOutOfBound _size p
                    || checkCollision _plane _size (possibleCollisions p d')
        stepOK' s = if stepOK s then Just s else Nothing
    return (stepOK' next <|> stepOK' nextTurn)

buildSpiral :: State Spiral ()
buildSpiral = do
    next <- nextStep
    case next of
        Just (p, d) -> do
            plane %= Set.insert p
            position .= p
            direction .= d
            buildSpiral
        Nothing -> return ()

planeToList :: Set (Int, Int) -> Int -> [[Int]]
planeToList s limit =
    [[if Set.member (x, y) s then 1 else 0 | y <- [0 .. limit - 1]] | x <- [0 .. limit - 1]]

spiralize :: Int -> [[Int]]
spiralize limit =
    let initState = Spiral{_plane = Set.fromList [(0, 0)], _direction = R, _position = (0, 0), _size = limit}
        Spiral{_plane = plane'} = execState buildSpiral initState
     in planeToList plane' limit
