module Garden where
import qualified Data.Map as M
import Data.List (transpose, sort)
import Data.List.Split (chunksOf)

data Plant = Clover | Grass | Radishes | Violets deriving (Eq, Show)

type Plot    = String
type PlotRow = String

lookupPlants :: Ord a => a -> M.Map a [Plant] -> [Plant]
lookupPlants =
    M.findWithDefault []

garden :: Ord a => [a] -> Plot -> M.Map a [Plant]
garden ss ps =
      M.fromListWith (flip (++)) $ concatMap (dig $ sort ss) $ words ps

dig :: [a] -> PlotRow -> [(a, [Plant])]
dig ss ps =
    zip ss . chunksOf 2 $ map toPlant ps

toPlant :: Char -> Plant
toPlant p
    | p == 'C' = Clover
    | p == 'G' = Grass
    | p == 'R' = Radishes
    | p == 'V' = Violets

defaultGarden :: Plot -> M.Map String [Plant]
defaultGarden =
    garden defaultStudents

egGarden :: Plot
egGarden = "VRCGVVRVCGGCCGVRGCVCGCGV\nVRCCCGCRRGVCGCRVVCVGCGCV"

egStudents :: String
egStudents    = ['A'..'Z']

defaultStudents :: [String]
defaultStudents =
    [ "Alice"
    , "Bob"
    , "Charlie"
    , "David"
    , "Eve"
    , "Fred"
    , "Ginny"
    , "Harriet"
    , "Ileana"
    , "Joseph"
    , "Kincaid"
    , "Larry"
    ]

