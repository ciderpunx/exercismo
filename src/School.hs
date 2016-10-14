module School where
import qualified Data.Map.Strict as M
import Data.List (sort)

type School = M.Map Integer [String]

empty :: School
empty = M.empty

add :: Integer -> String -> School -> School
add grade student = M.insertWith (++) grade [student]

grade :: Integer -> School -> [String]
grade g = sort . M.findWithDefault [] g

sorted :: School -> [(Integer,[String])]
sorted = M.toAscList . M.map sort
