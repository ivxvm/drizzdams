module Main where

import Prelude

import Control.Monad.ST (ST)
import Control.Monad.ST as ST
import Data.Array ((:), (..), replicate, foldl)
import Data.Array as A
import Data.Array.ST (STArray, modify, withArray)
import Data.Int as I
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect, foreachE)
import Effect.Console (error)
import Web.DOM.Document (createElement, createElementNS)
import Web.DOM.Element (toNode, setAttribute)
import Web.DOM.Node (appendChild, setTextContent)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toDocument, body)
import Web.HTML.HTMLElement (toElement)
import Web.HTML.Window (document)

nya :: (_ -> _ -> Effect Unit) -> Effect Unit
nya f = window >>= document >>= \doc ->
  body doc >>= case _ of
    Nothing -> error ":C"
    Just bodyElem -> do
      f (toDocument doc) (toNode (toElement bodyElem))

apE :: String -> _ -> _ -> Effect (Tuple _ _)
apE tag doc parN = do
  e <- createElement tag doc
  let n = toNode e
  _ <- appendChild n parN
  pure (Tuple e n)

apENS :: String -> String -> _ -> _ -> Effect (Tuple _ _)
apENS ns tag doc parN = do
  e <- createElementNS (Just ns) tag doc
  let n = toNode e
  _ <- appendChild n parN
  pure (Tuple e n)

apEWT :: String -> String -> _ -> _ -> Effect (Tuple _ _)
apEWT tag t doc parN = do
  e <- createElement tag doc
  let n = toNode e
  setTextContent t n
  _ <- appendChild n parN
  pure (Tuple e n)

apP :: Array Int -> Int -> Int -> Number -> _ -> _ -> Effect _
apP fqs w h fsz doc parN = do
  Tuple svgE svgN <- apENS "http://www.w3.org/2000/svg" "svg" doc parN
  setAttribute "xmlns" "http://www.w3.org/2000/svg" svgE
  let maxWH = I.toNumber (max w h)
  let maxX = I.toNumber w / maxWH
  let maxY = I.toNumber h / maxWH
  setAttribute "viewBox" ("0 0 " <> show maxX <> " " <> show maxY) svgE
  setAttribute "width" (show w) svgE
  setAttribute "height" (show h) svgE
  setAttribute "preserveAspectRatio" "none" svgE
  let maxF = A.foldl max bottom fqs
  let len = A.length fqs
  let dx = maxX / I.toNumber len
  foreachE (A.zip (0 .. (len - 1)) fqs) $ \(Tuple i f) -> do
    Tuple rectE _ <- apENS "http://www.w3.org/2000/svg" "rect" doc svgN
    let rh = maxY * I.toNumber f / I.toNumber maxF
    setAttribute "x" (show (dx * I.toNumber i)) rectE
    setAttribute "y" (show (maxY - rh)) rectE
    setAttribute "width" (show (dx * 0.9)) rectE
    setAttribute "height" (show rh) rectE
    setAttribute "fill" "blue" rectE
    Tuple textE textN <- apENS "http://www.w3.org/2000/svg" "text" doc svgN
    setAttribute "x" (show (dx * I.toNumber i + 0.9 * dx / 2.0)) textE
    setAttribute "y" (show (maxY - dx * 0.25)) textE
    setAttribute "font-size" (show fsz) textE
    setAttribute "text-anchor" "middle" textE
    setAttribute "fill" "red" textE
    setTextContent (show i) textN

digits :: Int -> Array Int
digits = gogogo [] where
  gogogo acc 0 = acc
  gogogo acc x = gogogo (x `mod` 10 : acc) (x `div` 10)

pgf :: (Int -> Int -> Int) -> Int -> Int -> Array Int
pgf op z p = map (foldl op z <<< digits) (A.filter I.odd $ A.range 1 p)

segDistr :: Int -> Array Number -> Array Int
segDistr seg xs = ST.run (withArray gogogo (replicate seg 0)) where
  minVal = foldl min top xs
  maxVal = foldl max bottom xs
  ilen = maxVal - minVal
  slen = ilen / I.toNumber seg
  gogogo :: forall casper . STArray casper Int -> ST casper Unit
  gogogo arr = ST.foreach xs $ \x -> do
    let i = I.floor $ (x - minVal) / slen
    void $ modify i ((+) 1) arr

modDistr :: Int -> Array Int -> Array Int
modDistr m xs = ST.run (withArray gogogo (replicate m 0)) where
  gogogo :: forall casper . STArray casper Int -> ST casper Unit
  gogogo arr = ST.foreach xs $ \x -> do
    void $ modify (x `mod` m) ((+) 1) arr

modDivDistr :: Int -> Int -> Array Int -> Array Int
modDivDistr m d xs = ST.run (withArray gogogo (replicate (m `div` d) 0)) where
  gogogo :: forall casper . STArray casper Int -> ST casper Unit
  gogogo arr = ST.foreach xs $ \x -> do
    void $ modify ((x `mod` m) `div` d) ((+) 1) arr

main :: Effect Unit
main = nya $ \doc body -> do
  let numPages = 953

  void $ apEWT "h1" "Consider a book" doc body
  void $ apEWT "h3" ("It has " <> show numPages <> " pages. You open it randomly and look at the right page's number. It's usually an odd number.") doc body
  void $ apEWT "h3" "Now what is the distribution of page numbers modulo 10?" doc body

  void $ apEWT "h1" "Distribution of odd page numbers % 10" doc body
  let dz = modDistr 10 (pgf (\x y -> x * 10 + y) 0 numPages)
  apP dz 400 400 0.05 doc body

  void $ apEWT "h3" "Now if you perform some operations on its digits, and take the modulo 10 on result, what is the distribution?" doc body

  foreachE (2 .. 30) $ \i -> do
    void $ apEWT "h1" ("Distribution of sums of digits of odd page numbers % " <> show i) doc body
    let d = modDistr i (pgf (+) 0 numPages)
    let fs | i < 11    = 0.05
           | i < 20    = 0.03
           | otherwise = 0.02
    apP d (40 * i) 400 fs doc body

  foreachE (2 .. 30) $ \i -> do
    void $ apEWT "h1" ("Distribution of products of digits of odd page numbers % " <> show i) doc body
    let d = modDistr i (pgf (*) 1 numPages)
    let fs | i < 11    = 0.05
           | i < 20    = 0.03
           | otherwise = 0.02
    apP d (40 * i) 400 fs doc body

  void $ apEWT "h1" "Distribution of odd page numbers % 20 div 2" doc body
  let d = modDivDistr 20 2 (pgf (\x y -> x * 10 + y) 0 numPages)
  apP d 400 400 0.05 doc body
