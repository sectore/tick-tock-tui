module TUI.Utils where

type RgbColor = (Double, Double, Double)

type RgbaColor = (Double, Double, Double, Double)

applyAlpha :: Double -> RgbColor -> RgbaColor
applyAlpha alpha (r, g, b) = (r, g, b, alpha)
