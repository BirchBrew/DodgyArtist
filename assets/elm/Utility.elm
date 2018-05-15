module Utility exposing (calculateDrawingSpaceEdgePx)

import Constant exposing (drawingWindowRatio)


calculateDrawingSpaceEdgePx : Int -> Int -> Float
calculateDrawingSpaceEdgePx windowWidth windowHeight =
    min windowWidth windowHeight
        * drawingWindowRatio
        // 100
        |> toFloat
