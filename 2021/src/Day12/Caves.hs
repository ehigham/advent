module Day12.Caves (
        Connection ( Conn )
    ,   CaveSystem
    ,   Cave
    ,   Path
    ,   consCaveSystem
    ,   findPaths
    ,   end
    ,   start
    ,   visitingSmallCavesOnce
    ,   visitingOneSmallCaveTwice
    ) where


import           Control.Applicative      ( (<|>) )
import           Control.Monad.Reader     ( Reader, runReader, ask, local )
import           Data.Char                ( isUpper )
import           Data.List                ( union )
import qualified Data.HashMap.Strict as M
import           Data.HashMap.Strict      ( HashMap )
import qualified Data.HashSet        as S
import           Data.HashSet             ( HashSet )
import           Data.Maybe               ( fromMaybe )
import           GHC.Read                 ( expectP, lexP )
import           Text.Read                ( Lexeme ( Ident, Symbol ), readPrec )


type Cave = String


isBigCave :: Cave -> Bool
isBigCave = all isUpper


start, end :: Cave
start = "start"
end = "end"


data Connection = Conn Cave Cave
    deriving stock Eq


instance Show Connection where
    show (Conn a b) = a  ++ "-" ++ b


instance Read Connection where
    readPrec = do
        Ident f <- lexP
        expectP (Symbol "-")
        Ident t <- lexP
        return $ Conn f t


type CaveSystem = HashMap Cave [Cave]


consCaveSystem :: [Connection] -> CaveSystem
consCaveSystem = M.fromListWith union . (edges =<<)
  where
    edges (Conn a b) = [(a, [b]), (b, [a])]


type Path = [Cave]


{-# INLINE findPaths #-}
findPaths :: Monoid m => (Cave -> Reader m [Path] -> Reader m [Path])
                       -> CaveSystem
                       -> [Path]
findPaths extend system = filter ((== end) . last)
                        $ runReader ((extend <*> go) start) mempty
  where
    go cave = do
        let neighbours = fromMaybe [] $ M.lookup cave system
        subpaths <- traverse (extend <*> go) neighbours
        return [ cave : path | path <- concat subpaths <|> [[]]]


visitingSmallCavesOnce :: Cave
                       -> Reader (HashSet Cave) [Path]
                       -> Reader (HashSet Cave) [Path]
visitingSmallCavesOnce cave subpaths
    | cave == end         = return [[cave]]
    | isBigCave cave      = subpaths
    | otherwise           = do
        visited <- ask
        if S.member cave visited
            then return mempty
            else local (S.insert cave) subpaths


visitingOneSmallCaveTwice :: Cave
                       -> Reader (HashMap Cave Int) [Path]
                       -> Reader (HashMap Cave Int) [Path]
visitingOneSmallCaveTwice cave subpaths
    | cave == end         = return [[cave]]
    | isBigCave cave      = subpaths
    | otherwise           = ask >>= \visited ->
        let count = fromMaybe 0 $ M.lookup cave visited in
            if count == 0 || count == 1 && start /= cave && notElem 2 (M.elems visited)
                then local (M.insertWith (const succ) cave 1) subpaths
                else return mempty
