#r "nuget: FsUnit, 4.0.6"

#load "solvers.fsx"

open Solvers
open FsUnit

[<Struct>]
type Test = {
    Day : int
    File : string;
    Part1 : string option
    Part2 : string option
}

let tests = [|
    { Day = 1; File = "../test/day01.txt"; Part1 = Some "514579"; Part2 = Some "241861950" }
    { Day = 2; File = "../test/day02.txt"; Part1 = Some "2"; Part2 = Some "1" }
    { Day = 3; File = "../test/day03.txt"; Part1 = Some "7"; Part2 = Some "336" }
    { Day = 4; File = "../test/day04.txt"; Part1 = Some "2"; Part2 = Some "2" }
    { Day = 5; File = "../test/day05.txt"; Part1 = Some "820"; Part2 = None }  // No example given for part 2
    { Day = 6; File = "../test/day06.txt"; Part1 = Some "11"; Part2 = Some "6" }
    { Day = 7; File = "../test/day07.txt"; Part1 = Some "4"; Part2 = Some "32" }
    { Day = 8; File = "../test/day08.txt"; Part1 = Some "5"; Part2 = Some "8" }
    //{ Day = 9; File = "../test/day09.txt"; Part1 = None; Part2 = None }  // Wrong "preamble" size
    { Day = 10; File = "../test/day10.txt"; Part1 = Some "220"; Part2 = Some "19208" }
    { Day = 11; File = "../test/day11.txt"; Part1 = Some "37"; Part2 = Some "26" }
    { Day = 12; File = "../test/day12.txt"; Part1 = Some "25"; Part2 = Some "286" }
    { Day = 13; File = "../test/day13.txt"; Part1 = Some "295"; Part2 = Some "1068781" }
    //{ Day = 14; File = "../test/day14.txt"; Part1 = Some "165"; Part2 = Some "208" }  // Different examples used for parts 1 and 2
    { Day = 15; File = "../test/day15.txt"; Part1 = Some "436"; Part2 = Some "175594" }
    { Day = 16; File = "../test/day16.txt"; Part1 = Some "71"; Part2 = None }  // No example given for part 2
    { Day = 17; File = "../test/day17.txt"; Part1 = Some "112"; Part2 = Some "848" }
    { Day = 18; File = "../test/day18.txt"; Part1 = Some "26457"; Part2 = Some "694173" }
    { Day = 19; File = "../test/day19.txt"; Part1 = Some "3"; Part2 = Some "12" }
    { Day = 20; File = "../test/day19.txt"; Part1 = None; Part2 = None }
    { Day = 21; File = "../test/day21.txt"; Part1 = Some "5"; Part2 = Some "mxmxvkd,sqjhc,fvjkl" }
|]

let runTest test =
    let solver = getSolver test.Day
    let testFile = System.IO.Path.Combine (__SOURCE_DIRECTORY__, test.File)
    let solution = testFile |> System.IO.File.ReadAllLines |> solver
    test.Part1 |> Option.iter (should equal (solution |> fst))
    test.Part2 |> Option.iter (should equal (solution |> snd))

tests |> Array.map runTest
