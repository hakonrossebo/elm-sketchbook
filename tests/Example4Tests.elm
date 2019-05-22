module Example4Tests exposing (all)

import Expect exposing (..)
import Sketches.Example4 exposing (..)
import Test exposing (..)


all : Test
all =
    describe "Mandelbrot - Example 4 test suite"
        [ test "Addition" <|
            \_ ->
                Expect.equal 10 (3 + 7)
        , test "Complex Addition" <|
            \_ ->
                Expect.equal { r = 4, i = 4 } (addComplex { r = 2, i = 2 } { r = 2, i = 2 })
        , test "Complex sqrt" <|
            \_ ->
                Expect.within (Absolute 0.1) 2.82 (magnitudeComplex { r = 2, i = 2 })
        , test "Complex Iteration Outside" <|
            \_ ->
                Expect.equal 0 (iterateComplexMandelbrot { r = 0, i = 0 } { r = 3, i = 3 } 50 0)
        , test "Complex Iteration Inside" <|
            \_ ->
                Expect.equal 2 (iterateComplexMandelbrot { r = 0, i = 0 } { r = 1, i = 1 } 50 0)
        , test "String.left" <|
            \_ ->
                Expect.equal "a" (String.left 1 "abcdefg")
        ]
