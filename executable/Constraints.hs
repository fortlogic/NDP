{-# LANGUAGE OverloadedStrings #-}
module Constraints where

import Data.ByteString.Builder
import Data.List as L

data Constraints = Constraints {
  rawConstraints :: [String],
  netConstraints :: [NetConstraint]
  } deriving (Read, Show)

data NetConstraint = SingleNet String String [NetParameter] -- name, location, params
                   | SingleNetLocless String [NetParameter] -- name, params
                   | BusNet String [String] [NetParameter] -- name, locations, params
                   deriving (Read, Show)

data NetParameter = NetKV String String -- Arbitrary key/value pair
                  | NetFlag String      -- unary flag (like PULLUP)
                  deriving (Read, Show)

renderConstraints :: Constraints -> Builder
renderConstraints cs = renderLines (rawB ++ netB)
  where rawB = map stringUtf8 (rawConstraints cs)
        netB = (netConstraints cs) >>= renderNet

renderLines :: [Builder] -> Builder
renderLines lns = mconcat [ ln <> stringUtf8 ";\n" | ln <- lns ]

renderNet :: NetConstraint -> [Builder]
renderNet (SingleNet name loc attrs) = [renderPrimNet (name <> "(0)") (locP : attrs)]
  where locP = NetKV "LOC" loc
renderNet (SingleNetLocless name attrs) = [renderPrimNet (name <> "(0)") attrs]
renderNet (BusNet name locs attrs) = zipWith mkSingle locs ([0..] :: [Int])
  where mkSingle loc idx = renderPrimNet (mkName idx) (NetKV "LOC" loc : attrs)
        mkName idx = name ++ "(" ++ show idx ++ ")"

renderPrimNet :: String -> [NetParameter] -> Builder
renderPrimNet name attrs = mconcat $ intersperse spc ["NET", netName, renderAttribs attrs]
  where netName = stringUtf8 name
        spc = charUtf8 ' '

renderNetParameter :: NetParameter -> Builder
renderNetParameter (NetFlag f)        = stringUtf8 f
renderNetParameter (NetKV l r) = utf8 l <> utf8 " = " <> utf8 r

renderAttribs :: [NetParameter] -> Builder
renderAttribs attr = mconcat $ intersperse sep $ map renderNetParameter attr
  where sep = stringUtf8 " | "

-- I want a shorter name for this
utf8 :: String -> Builder
utf8 = stringUtf8
