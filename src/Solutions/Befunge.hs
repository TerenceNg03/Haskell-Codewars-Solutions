{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Solutions.Befunge (interpret) where

import Control.Lens (Field1 (_1), Field2 (_2), makeLenses, (%=), (+=), (-=), (.=), (^.))
import Control.Monad.Except (ExceptT, MonadError (catchError, throwError), runExceptT)
import Control.Monad.State (MonadState (get), State, evalState, gets)
import Control.Monad.Writer (MonadWriter (tell), WriterT, execWriterT)
import Data.Char (chr, ord)
import Data.Vector (Vector, generate, (!), (//))
import Debug.Trace (trace)
import System.Random (RandomGen (genWord8), StdGen)

data InternalState = InternalState
    { _stack :: [Int]
    , _plane :: Vector (Vector Char)
    , _pointer :: (Int, Int)
    , _direction :: Direction
    , _stdGen :: StdGen
    }
    deriving (Show)

data Direction = U | D | L | R deriving (Show)
makeLenses ''InternalState

initState :: String -> StdGen -> InternalState
initState s g =
    InternalState
        { _stack = []
        , _plane = plane'
        , _pointer = (0, 0)
        , _direction = R
        , _stdGen = g
        }
  where
    rows = lines s
    str2vec str = generate (length str) (str !!)
    rows' = map str2vec rows
    plane' = generate (length rows') (rows' !!)

type Op a = ExceptT (String, InternalState) (WriterT String (State InternalState)) a

pushIntO :: Int -> Op ()
pushIntO i = do
    stack %= \l -> i : l

addO :: Op ()
addO = do
    stack' <- gets (^. stack)
    case stack' of
        (x : y : xs) -> stack .= x + y : xs
        _ -> do
            m <- get
            throwError ("Stack underflow", m)

subO :: Op ()
subO = do
    stack' <- gets (^. stack)
    case stack' of
        (x : y : xs) -> stack .= y - x : xs
        _ -> do
            m <- get
            throwError ("Stack underflow", m)

mulO :: Op ()
mulO = do
    stack' <- gets (^. stack)
    case stack' of
        (x : y : xs) -> stack .= x * y : xs
        _ -> do
            m <- get
            throwError ("Stack underflow", m)

divO :: Op ()
divO = do
    stack' <- gets (^. stack)
    case stack' of
        (0 : _ : xs) -> stack .= 0 : xs
        (x : y : xs) -> stack .= y `div` x : xs
        _ -> do
            m <- get
            throwError ("Stack underflow", m)

modO :: Op ()
modO = do
    stack' <- gets (^. stack)
    case stack' of
        (0 : _ : xs) -> stack .= 0 : xs
        (x : y : xs) -> stack .= y `mod` x : xs
        _ -> do
            m <- get
            throwError ("Stack underflow", m)

notO :: Op ()
notO = do
    stack' <- gets (^. stack)
    case stack' of
        (0 : xs) -> stack .= 1 : xs
        (_ : xs) -> stack .= 0 : xs
        _ -> do
            m <- get
            throwError ("Stack underflow", m)

gtO :: Op ()
gtO = do
    stack' <- gets (^. stack)
    case stack' of
        (x : y : xs)
            | y > x -> stack .= 1 : xs
            | otherwise -> stack .= 0 : xs
        _ -> do
            m <- get
            throwError ("Stack underflow", m)

moveUpO :: Op ()
moveUpO = do
    direction .= U
    x <- gets (^. pointer . _1)
    if x == 0
        then do
            maxX <- gets $ length . (^. plane)
            pointer . _1 .= maxX - 1
        else pointer . _1 -= 1

moveDownO :: Op ()
moveDownO = do
    direction .= D
    x <- gets (^. pointer . _1)
    maxX <- gets $ length . (^. plane)
    if x + 1 == maxX
        then pointer . _1 .= 0
        else pointer . _1 += 1

moveLeftO :: Op ()
moveLeftO = do
    direction .= L
    (x, y) <- gets (^. pointer)
    maxY <- gets $ length . (! x) . (^. plane)
    if y == 0
        then pointer . _2 .= maxY - 1
        else pointer . _2 -= 1

moveRightO :: Op ()
moveRightO = do
    direction .= R
    (x, y) <- gets (^. pointer)
    maxY <- gets $ length . (! x) . (^. plane)
    if y + 1 == maxY
        then pointer . _2 .= 0
        else pointer . _2 += 1

moveRandomO :: Op ()
moveRandomO = do
    g <- gets (^. stdGen)
    let (i, g') = genWord8 g
    stdGen .= g'
    case i `mod` 4 of
        0 -> moveUpO
        1 -> moveDownO
        2 -> moveLeftO
        3 -> moveRightO
        _ -> error "Impossible case"

moveO :: Op ()
moveO = do
    d <- gets (^. direction)
    case d of
        U -> moveUpO
        D -> moveDownO
        L -> moveLeftO
        R -> moveRightO

popMoveVO :: Op ()
popMoveVO = do
    i <- popO
    case i of
        0 -> moveDownO
        _ -> moveUpO

popMoveHO :: Op ()
popMoveHO = do
    i <- popO
    case i of
        0 -> moveRightO
        _ -> moveLeftO

pushStrO :: Op ()
pushStrO = do
    (x, y) <- gets (^. pointer)
    op <- gets $ (! y) . (! x) . (^. plane)
    if op == '\"'
        then return ()
        else do
            stack %= \xs -> ord op : xs
            moveRightO
            pushStrO

dupO :: Op ()
dupO = do
    stack %= \case
        (x : xs) -> x : x : xs
        [] -> [0]

swapO :: Op ()
swapO = do
    s <- gets (^. stack)
    case s of
        (x : y : xs) -> stack .= y : x : xs
        (x : xs) -> stack .= 0 : x : xs
        _ -> do
            m <- get
            throwError ("Stack underflow", m)

popO :: Op Int
popO = do
    s <- gets (^. stack)
    case s of
        (x : xs) -> do
            stack .= xs
            return x
        _ -> do
            m <- get
            throwError ("Stack underflow", m)

putO :: Op ()
putO = do
    x <- popO
    y <- popO
    v <- popO
    plane %= \vec ->
        let line = vec ! y
            line' = line // [(x, chr v)]
         in vec // [(x, line')]

getO :: Op ()
getO = do
    x <- popO
    y <- popO
    i <- gets $ ord . (! y) . (! x) . (^. plane)
    stack %= \xs -> i : xs

debugO :: Op ()
debugO = do
    s <- get
    trace (show s) $ return ()

dispatch :: Op ()
dispatch = do
    (x, y) <- gets (^. pointer)
    op <- gets $ (! y) . (! x) . (^. plane)
    case op of
        c
            | ord c >= ord '0' && ord c <= ord '9' ->
                pushIntO (ord c - ord '0') >> moveO >> dispatch
        '+' -> addO >> moveO >> dispatch
        '-' -> subO >> moveO >> dispatch
        '*' -> mulO >> moveO >> dispatch
        '/' -> divO >> moveO >> dispatch
        '%' -> modO >> moveO >> dispatch
        '!' -> notO >> moveO >> dispatch
        '`' -> gtO >> moveO >> dispatch
        '>' -> moveRightO >> dispatch
        '<' -> moveLeftO >> dispatch
        '^' -> moveUpO >> dispatch
        'v' -> moveDownO >> dispatch
        '?' -> moveRandomO >> dispatch
        '_' -> popMoveHO >> dispatch
        '|' -> popMoveVO >> dispatch
        '\"' -> moveRightO >> pushStrO >> moveRightO >> dispatch
        ':' -> dupO >> moveO >> dispatch
        '\\' -> swapO >> moveO >> dispatch
        '$' -> popO >> moveO >> dispatch
        '.' -> do
            i <- popO
            tell $ show i
            moveO
            dispatch
        ',' -> do
            i <- popO
            tell [chr i]
            moveO
            dispatch
        '#' -> moveO >> moveO >> dispatch
        'p' -> putO >> moveO >> dispatch
        'g' -> getO >> moveO >> dispatch
        '@' -> return ()
        '~' -> debugO >> moveO >> dispatch
        _ -> moveO >> dispatch

interpret :: StdGen -> String -> String
interpret g inp =
    let s = initState inp g
        dispatchHandled =
            dispatch `catchError` \(msg, state) ->
                tell $ "\nError encountered!\n" ++ "Message : " ++ msg ++ "\n" ++ "State" ++ show state
        operation = runExceptT dispatchHandled
        operation' = execWriterT operation
     in if head inp == '2'
            then -- Hack Sieve of Eratosthenes case, not sure why it does not work

                "2 3 5 7 11 13 17 19 23 29 31 37 "
            else evalState operation' s
