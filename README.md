# Advent of Code 2020: F# 
I am using [2020's Advent of Code](https://adventofcode.com/) challenge as an exercise to learn F#.
My solutions are unlikely to be highly optimised, but the algorithms used should be reasonable.
I use a functional style as much as is reasonably possible, aiming to write idiomatic F#.

Some of the loose constraints I placed on myself are:
- No loops (for, while, ...)
- No mutable state
- No classes

## Prerequisites
- [F# Interactive](https://docs.microsoft.com/en-us/dotnet/fsharp/tools/fsharp-interactive/). On Windows this can be obtained through Visual Studio Installer by installing [**.NET desktop development**](https://docs.microsoft.com/en-us/visualstudio/ide/fsharp-visual-studio?view=vs-2019).

## Running
1. Clone the repository and navigate to the root of the project
   ```
   git clone git@github.com:jonnybolton/aoc2020-fsharp.git
   cd aoc2020-fsharp
   ```
2. Download the puzzle input files from the [Advent of Code website](https://adventofcode.com/2020). You will be required to create an account if you do not have one.
3. Run the code for a particular day with the following command
   ```
   dotnet fsi src/day01.fsx day01.txt
   ```
   replacing the fsx script with the corresponding day, and replacing the final argument with the path to your input for that day.

## Progress
| Challenge                                                               | File                                | Part 1 | Part 2 | Time           |
|-------------------------------------------------------------------------|-------------------------------------|:------:|:------:|----------------|
| [Day 1: Report Repair](https://adventofcode.com/2020/day/1)             | [Day01.fs](aoc2020-fsharp/Day01.fs) | ✔️     | ✔️     | `█           ` |
| [Day 2: Password Philosophy](https://adventofcode.com/2020/day/2)       | [Day02.fs](aoc2020-fsharp/Day02.fs) | ✔️     | ✔️     | `█           ` |
| [Day 3: Toboggan Trajectory](https://adventofcode.com/2020/day/3)       | [Day03.fs](aoc2020-fsharp/Day03.fs) | ✔️     | ✔️     | `█           ` |
| [Day 4: Passport Processing](https://adventofcode.com/2020/day/4)       | [Day04.fs](aoc2020-fsharp/Day04.fs) | ✔️     | ✔️     | `█           ` |
| [Day 5: Binary Boarding](https://adventofcode.com/2020/day/5)           | [Day05.fs](aoc2020-fsharp/Day05.fs) | ✔️     | ✔️     | `█           ` |
| [Day 6: Custom Customs](https://adventofcode.com/2020/day/6)            | [Day06.fs](aoc2020-fsharp/Day06.fs) | ✔️     | ✔️     | `█           ` |
| [Day 7: Handy Haversacks](https://adventofcode.com/2020/day/7)          | [Day07.fs](aoc2020-fsharp/Day07.fs) | ✔️     | ✔️     | `█           ` |
| [Day 8: Handheld Halting](https://adventofcode.com/2020/day/8)          | [Day08.fs](aoc2020-fsharp/Day08.fs) | ✔️     | ✔️     | `█           ` |
| [Day 9: Encoding Error](https://adventofcode.com/2020/day/9)            | [Day09.fs](aoc2020-fsharp/Day09.fs) | ✔️     | ✔️     | `█           ` |
| [Day 10: Adapter Array](https://adventofcode.com/2020/day/10)           | [Day10.fs](aoc2020-fsharp/Day10.fs) | ✔️     | ✔️     | `█           ` |
| [Day 11: Seating System](https://adventofcode.com/2020/day/11)          | [Day11.fs](aoc2020-fsharp/Day11.fs) | ✔️     | ✔️     | `██████████  ` |
| [Day 12: Rain Risk](https://adventofcode.com/2020/day/12)               | [Day12.fs](aoc2020-fsharp/Day12.fs) | ✔️     | ✔️     | `█           ` |
| [Day 13: Shuttle Search](https://adventofcode.com/2020/day/13)          | [Day13.fs](aoc2020-fsharp/Day13.fs) | ✔️     | ✔️     | `█           ` |
| [Day 14: Docking Data](https://adventofcode.com/2020/day/14)            | [Day14.fs](aoc2020-fsharp/Day14.fs) | ✔️     | ✔️     | `█           ` |
| [Day 15: Rambunctious Recitation](https://adventofcode.com/2020/day/15) | [Day15.fs](aoc2020-fsharp/Day15.fs) | ✔️     | ✔️     | `████████████` |
| [Day 16: Ticket Translation](https://adventofcode.com/2020/day/16)      | [Day16.fs](aoc2020-fsharp/Day16.fs) | ✔️     |        |                |
| [Day 17: Conway Cubes](https://adventofcode.com/2020/day/17)            |                                     |        |        |                |
| [Day 18: Operation Order](https://adventofcode.com/2020/day/18)         |                                     |        |        |                |
| [Day 19: Monster Messages](https://adventofcode.com/2020/day/19)        |                                     |        |        |                |
| [Day 20: Jurassic Jigsaw](https://adventofcode.com/2020/day/20)         |                                     |        |        |                |
| [Day 21: Allergen Assessment](https://adventofcode.com/2020/day/21)     |                                     |        |        |                |
| [Day 22: Crab Combat](https://adventofcode.com/2020/day/22)             |                                     |        |        |                |
| [Day 23: Crab Cups](https://adventofcode.com/2020/day/23)               |                                     |        |        |                |
| [Day 24: Lobby Layout](https://adventofcode.com/2020/day/24)            |                                     |        |        |                |
| [Day 25: Combo Breaker](https://adventofcode.com/2020/day/25)           |                                     |        |        |                |
