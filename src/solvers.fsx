#load "day01.fsx"
#load "day02.fsx"
#load "day03.fsx"
#load "day04.fsx"
#load "day05.fsx"
#load "day06.fsx"
#load "day07.fsx"
#load "day08.fsx"
#load "day09.fsx"
#load "day10.fsx"
#load "day11.fsx"
#load "day12.fsx"
#load "day13.fsx"
#load "day14.fsx"
#load "day15.fsx"
#load "day16.fsx"
#load "day17.fsx"
#load "day18.fsx"
#load "day19.fsx"
#load "day21.fsx"

open Day01
open Day02
open Day03
open Day04
open Day05
open Day06
open Day07
open Day08
open Day09
open Day10
open Day11
open Day12
open Day13
open Day14
open Day15
open Day16
open Day17
open Day18
open Day19
open Day21

let solvers = Map [
    ( 1, Day01.solve )
    ( 2, Day02.solve )
    ( 3, Day03.solve )
    ( 4, Day04.solve )
    ( 5, Day05.solve )
    ( 6, Day06.solve )
    ( 7, Day07.solve )
    ( 8, Day08.solve )
    ( 9, Day09.solve )
    ( 10, Day10.solve )
    ( 11, Day11.solve )
    ( 12, Day12.solve )
    ( 13, Day13.solve )
    ( 14, Day14.solve )
    ( 15, Day15.solve )
    ( 16, Day16.solve )
    ( 17, Day17.solve )
    ( 18, Day18.solve )
    ( 19, Day19.solve )
    ( 21, Day21.solve )
]

let getSolver dayNumber = solvers |> Map.find dayNumber
