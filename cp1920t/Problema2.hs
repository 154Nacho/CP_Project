-- Resolução do Problema 2 doc Trabalho de CP

import Cp
import List  hiding (fac)
import Nat
import BTree
import LTree
import Probability
import ListUtils
import Show
import Data.List hiding (find)
import Test.QuickCheck hiding ((><),choose,collect)
import qualified Test.QuickCheck as QuickCheck
import System.Random  hiding (split)
import System.Process
import GHC.IO.Exception
import Graphics.Gloss
import Control.Monad
import Control.Applicative hiding ((<|>))
import Exp

auxMaisEsq :: (a,(Maybe a,Maybe a)) -> Maybe a
auxMaisEsq (h,(Nothing,r)) = Just h
auxMaisEsq (_,(l,_)) = l  

maisEsq :: BTree a -> Maybe a
maisEsq = cataBTree g
  where g = either (const Nothing) auxMaisEsq

auxMaisDir :: (a,(Maybe a,Maybe a)) -> Maybe a
auxMaisDir (h,(l,Nothing)) = Just h 
auxMaisDir (_,(_,r)) = r

maisDir :: BTree a -> Maybe a
maisDir = cataBTree g
  where g = either (const Nothing) auxMaisDir

infixr 4 .==.
(.==.) :: Eq b => (a -> b) -> (a -> b) -> (a -> Bool)
f .==. g = \a -> f a == g a

prop_inv :: BTree String -> Bool
prop_inv = maisEsq .==. maisDir . invBTree

auxisOrd :: (Ord a) => [a] -> Bool
auxisOrd [] = True
auxisOrd l | l == qSort(l) = True
           | l /= qSort(l) = False

isOrd' = cataBTree g
  where g = inordt

isOrd :: (Ord a) => BTree a -> Bool
isOrd = auxisOrd . isOrd´

isOrd' = cataBTree g
  where g = undefined

isOrd = undefined




insOrd' x = cataBTree g 
  where g = undefined

insOrd a x = undefined


{-Testes 
*Splay> maisDir t1
    Just 16
*Splay> maisEsq t1
    Just 1
*Splay> maisDir t2
    Just 8
*Splay> maisEsq t2
    Just 0
-}

emp x = Node(x,(Empty,Empty))

t7 = emp 7
t16 = emp 16
t7_10_16 = Node(10,(t7,t16))
t1_2_nil = Node(2,(emp 1, Empty)) 
t1 = Node(5,(t1_2_nil, t7_10_16))

t0_2_1 = Node(2, (emp 0, emp 3))
t5_6_8 = Node(6, (emp 5, emp 8))
t2 = Node(4, (t0_2_1, t5_6_8))


