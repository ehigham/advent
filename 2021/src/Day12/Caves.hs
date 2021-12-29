module Day12.Caves (
        Connection ( Conn )
    ,   CaveSystem
    ,   Cave
    ,   Path
    ,   consCaveSystem
    ,   findPaths
    ,   end
    ,   start
    ) where


import           Control.Applicative      ( (<|>) )
import           Control.Monad.Reader     ( runReader, ask, local )
import           Data.Char                ( isUpper )
import           Data.Function            ( (&) )
import           Data.Functor             ( (<&>) )
import           Data.List                ( union )
import qualified Data.HashMap.Strict as M
import           Data.HashMap.Strict      ( HashMap )
import qualified Data.HashSet        as S
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


findPaths :: CaveSystem -> [Path]
findPaths system = filter ((end ==) . last) $ runReader (go start) S.empty
  where
    go cave = do
        visited <- ask
        let neighbours = M.lookup cave system
                         <&> filter (not . (`S.member` visited))
                         & fromMaybe []

        subpaths <- traverse (pathsFrom cave) neighbours
        return . map (cave:) $ concat subpaths <|> [[]]


    pathsFrom cave next
        | end == cave    = return mempty
        | isBigCave cave = go next
        | otherwise      = local (S.insert cave) (go next)
