{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
module Calculator where


calc :: String -> Double
calc = undefined
type CSS = String

data AttrType = Geometric | Presentation

data Attr (t :: AttrType) where
    Radius, Width 
        :: Float -> Attr Geometric 
    Fill, Stroke 
        :: CSS   -> Attr Presentation
    FillOpacity, StrokeOpacity, StrokeWidth 
        :: Float -> Attr Presentation

data VizSpec 
    = Scatterplot
    | BubbleChart
    | StripPlot
    | BarChart
    | StackedBarChart
    | Histogram
