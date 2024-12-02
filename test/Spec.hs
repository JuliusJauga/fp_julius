{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}
import Test.Tasty ( TestTree, defaultMain, testGroup )
import Test.Tasty.HUnit ( testCase, (@?=) )
import Test.Tasty.QuickCheck as QC

import Data.List
import Data.Ord

import Lib1 qualified
import Lib2 qualified
import Lib3 qualified
import Test.QuickCheck (Arbitrary, arbitrary, elements, listOf1, Gen, resize, sized, oneof, frequency, choose)

import Control.Concurrent.STM (newTVarIO, readTVarIO, TVar)
import Control.Concurrent (newChan, Chan)
import Control.Monad.IO.Class (liftIO)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [propertyTests]

propertyTests :: TestTree
propertyTests = testGroup "property tests"
  [
    QC.testProperty "Save then load" $ withMaxSuccess 100 saveThenLoad,
    QC.testProperty "Preserve state" $ withMaxSuccess 100 preserveState
  ]

saveThenLoad :: Property
saveThenLoad = forAll genStatements $ \stmts ->
  let rendered = Lib3.renderStatements stmts
      parsed = Lib3.parseStatements rendered
  in 
  counterexample (showDetails stmts rendered parsed) $
  case parsed of
    Right (parsedStmts, _) -> stmts == parsedStmts
    _ -> False
  where
    showDetails expected rendered actual =
      "Expected: " ++ show expected ++
      "\nRendered: " ++ rendered ++
      "\nActual: " ++ show actual

preserveState :: Property
preserveState = forAll genStatements $ \stmts -> ioProperty $ do
  oldState <- newTVarIO $ Lib2.State [] [] []
  newState <- newTVarIO $ Lib2.State [] [] []
  chan <- newChan :: IO (Chan Lib3.StorageOp)
  _ <- Lib3.stateTransition oldState (Lib3.StatementCommand stmts) chan
  marshalled <- Lib3.marshallState <$> readTVarIO oldState
  let rendered = Lib3.renderStatements marshalled
  case Lib3.parseStatements rendered of
    Left _ -> return False
    Right (parsed, _) -> do
      _ <- Lib3.stateTransition newState (Lib3.StatementCommand parsed) chan
      oldStateVal <- readTVarIO oldState
      newStateVal <- readTVarIO newState
      return $ oldStateVal == newStateVal


-- Generators
maxDepth :: Int
maxDepth = 4

maxLen :: Int
maxLen = 10

limListOf1 :: Gen a -> Gen [a]
limListOf1 genElement = sized $ \n ->
  let size = min n maxLen
  in resize size (listOf1 genElement)

genStatements :: Gen Lib3.Statements
genStatements = oneof [
    Lib3.Single <$> genQuery,
    Lib3.Batch <$> limListOf1 genQuery
  ]

genQuery :: Gen Lib2.Query
genQuery = oneof [
    Lib2.ListCreate <$> genName,
    Lib2.ListAdd <$> genName <*> genName,
    Lib2.ListGet <$> genName,
    Lib2.ListRemove <$> genName,
    Lib2.RouteCreate <$> genRoute maxDepth,
    Lib2.RouteGet <$> genName,
    Lib2.RouteAddRoute <$> genName <*> genName,
    Lib2.RouteAddStop <$> genName <*> genName,
    Lib2.RouteRemoveStop <$> genName <*> genName,
    Lib2.RouteRemove <$> genName,
    Lib2.StopCreate <$> genName,
    Lib2.StopDelete <$> genName,
    Lib2.RoutesFromStop <$> genName
  ]

genName :: Gen Lib2.Name
genName = do
  name <- limListOf1 $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']
  return $ Lib2.StringName name

genRoute :: Int -> Gen Lib2.Route
genRoute 0 = Lib2.Route <$> genName <*> limListOf1 genStop <*> pure []
genRoute depth = Lib2.Route <$> genName <*> limListOf1 genStop <*> limListOf1 (genRoute (depth - 1))

genStop :: Gen Lib2.Stop
genStop = Lib2.Stop <$> genName