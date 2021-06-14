{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative
import Control.Monad.Identity (Identity, runIdentity)
import Data.Aeson (decode)
import Data.ByteString.Lazy.Char8 ()
import Data.Ratio (denominator, numerator)
import Data.Scientific (Scientific)
import Data.Text (Text)
import Test.HUnit ((@?=))
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase)
import Text.Digestive (Form, (.:), Result(..), bool, check, text, listOf, stringRead, validate)
import Text.Digestive.Aeson (digestJSON, jsonErrors)

import qualified Data.Text as T

--------------------------------------------------------------------------------
data Pokemon = Pokemon { pokemonName :: Text, pokemonNumber :: Int }
  deriving (Eq, Show)


pokeForm :: Monad m => Form Text m Pokemon
pokeForm = Pokemon <$> "name" .: nonEmptyText
                   <*> "number" .: parseInteger
  where
    nonEmptyText = check "Name cannot be empty" (not . T.null) $
                     text Nothing


data Pokedex = Pokedex [Pokemon]
  deriving (Eq, Show)


--------------------------------------------------------------------------------
testPokemon :: TestTree
testPokemon = testGroup "Pokemon tests" [ testPokemonOk, testPokemonInvalid ]
  where
    testPokemonOk =
        let (v, r) = runIdentity $ digestJSON pokeForm json
        in testGroup "Submit pokeForm with valid data/check error view"
             [ testCase "Passed validation" $ r @?= Just expected
             , testCase "jsonErrors shows no errors" $ jsonErrors v @?= errors
             ]
      where
        (Just json) = decode "{\"name\":\"Pikachu\", \"number\":\"25\"}"
        (Just errors) = decode "{}"
        expected = Pokemon { pokemonName = "Pikachu", pokemonNumber = 25 }

    testPokemonInvalid =
        let (v, r) = runIdentity $ digestJSON pokeForm json
        in testGroup "Submit pokeForm with invalid data/check error view"
             [ testCase "Failed validation" $ r @?= Nothing
             , testCase "jsonErrors shows correct errors" $ jsonErrors v @?= errors
             ]
      where
        (Just json) = decode "{\"name\":\"\", \"number\":\"25\"}"
        (Just errors) = decode "{\"name\":\"Name cannot be empty\"}"
        expected = Pokemon { pokemonName = "Pikachu", pokemonNumber = 25 }


--------------------------------------------------------------------------------
testPokedex :: TestTree
testPokedex = testGroup "Pokedex tests"
                [ testPokedexSingle
                , testPokedexMany
                , testPokedexFailHead
                , testPokedexFailLater
                ]
  where
    pokedexForm :: Monad m => Form Text m Pokedex
    pokedexForm = Pokedex <$> "pokemon" .: listOf (const pokeForm) Nothing

    testPokedexSingle = testCase "Valid pokedex with one pokemon" $
        (runIdentity $ snd <$> digestJSON pokedexForm json) @?= Just expected
      where
        (Just json) = decode "{\"pokemon\":[{\"name\": \"Pikachu\",\"number\":25}]}"
        expected = Pokedex [Pokemon { pokemonName = "Pikachu", pokemonNumber = 25 }]

    testPokedexMany = testCase "Valid pokedex with many pokemon" $
        (runIdentity $ snd <$> digestJSON pokedexForm json) @?= Just expected
      where
        (Just json) = decode "{\"pokemon\":[\
                             \  {\"name\": \"Pikachu\", \"number\": 25},\
                             \  {\"name\":\"Clefable\", \"number\": 36},\
                             \  {\"name\":\"Gengar\", \"number\": 94}\
                             \]}"
        expected = Pokedex [ Pokemon { pokemonName = "Pikachu", pokemonNumber = 25 }
                           , Pokemon { pokemonName = "Clefable", pokemonNumber = 36 }
                           , Pokemon { pokemonName = "Gengar", pokemonNumber = 94 }
                           ]

    testPokedexFailHead =
        let (v, r) = runIdentity $ digestJSON pokedexForm json
        in testGroup "Submit pokedex with a single invalid item"
             [ testCase "Failed validation" $ r @?= Nothing
             , testCase "jsonErrors shows correct errors" $ jsonErrors v @?= errors
             ]
      where
        (Just json) = decode "{\"pokemon\":[{\"name\":\"\", \"number\": 10}]}"
        (Just errors) = decode "{\"pokemon\":[{\"name\":\"Name cannot be empty\"}]}"

    testPokedexFailLater =
        let (v, r) = runIdentity $ digestJSON pokedexForm json
        in testGroup "Submit pokedex with a later invalid item"
             [ testCase "Failed validation" $ r @?= Nothing
             , testCase "jsonErrors shows correct errors" $ jsonErrors v @?= errors
             ]
      where
        (Just json) = decode "{\"pokemon\":[{\"name\": \"Pikachu\", \"number\": 10}, {\"name\":\"\", \"number\": 10}]}"
        (Just errors) = decode "{\"pokemon\":[null, {\"name\":\"Name cannot be empty\"}]}"


--------------------------------------------------------------------------------
testTopLevelLists :: TestTree
testTopLevelLists = testCase "Top level lists" $ do
  let (Just json) = decode "[\"hello\", \"world\"]"
  (runIdentity $ snd <$> digestJSON (listOf text Nothing :: Form Text Identity [Text]) json)
    @?= Just [ "hello", "world" ]


--------------------------------------------------------------------------------
testBool :: TestTree
testBool = testCase "Booleans work" $ do
  let (Just json) = decode "{\"a\": true, \"b\": false}"
      parser :: Form () Identity (Bool, Bool)
      parser = (,) <$> "a" .: bool Nothing <*> "b" .: bool (Just True)
  (runIdentity $ snd <$> digestJSON parser json)
    @?= Just (True, False)


--------------------------------------------------------------------------------
main :: IO ()
main = defaultMain $ testGroup "Tests" [ testPokemon
                                       , testPokedex
                                       , testTopLevelLists
                                       , testBool
                                       ]

--------------------------------------------------------------------------------
validateInteger :: Num a => Scientific -> Result Text a
validateInteger x =
  let xRat = toRational x
  in if denominator xRat /= 1
       then Error "Number must be an integer"
       else return (fromInteger $ numerator xRat)

--------------------------------------------------------------------------------
parseInteger :: (Monad m, Num a) => Form Text m a
parseInteger =
  validate validateInteger (stringRead "Could not parse number" Nothing)
