# Advent of Code 2020: F# 
I am using [2020's Advent of Code](https://adventofcode.com/) challenge as an exercise to learn F#.
My solutions are unlikely to be highly optimised, but the algorithms used should be reasonable.
I use a functional style as much as is reasonably possible, aiming to write idiomatic F#.

Some of the loose constraints I placed on myself are:
- No loops (for, while, ...)
- No mutable state
- No classes

## Prerequisites
- [F# Interactive](https://docs.microsoft.com/en-us/dotnet/fsharp/tools/fsharp-interactive/).
  On Windows this can be obtained through Visual Studio Installer by installing [**.NET desktop development**](https://docs.microsoft.com/en-us/visualstudio/ide/fsharp-visual-studio?view=vs-2019).

## Running
1. Clone the repository and navigate to the root of the project
   ```
   git clone git@github.com:jonnybolton/aoc2020-fsharp.git
   cd aoc2020-fsharp
   ```
2. Download the puzzle input files from the [Advent of Code website](https://adventofcode.com/2020). You will be required to create an account if you do not have one.
3. Run the code for a particular day with the following command
   ```
   dotnet fsi aoc2020.fsx [DAY-NUMBER] [PATH-TO-PUZZLE-INPUT-FILE]
   ```
   For example
   ```
   dotnet fsi aoc2020.fsx 1 data/day01.txt
   ```

## Testing
Most puzzle descriptions include a simple example along with a solution.
These have been converted into unit tests which can be executed by running
```
dotnet fsi src/test.fsx
```

## Progress
| Challenge                                                               | File                       | Part 1 | Part 2 |
|-------------------------------------------------------------------------|----------------------------|:------:|:------:|
| [Day 1: Report Repair](https://adventofcode.com/2020/day/1)             | [day01.fsx](src/day01.fsx) | ✔️     | ✔️     |
| [Day 2: Password Philosophy](https://adventofcode.com/2020/day/2)       | [day02.fsx](src/day02.fsx) | ✔️     | ✔️     |
| [Day 3: Toboggan Trajectory](https://adventofcode.com/2020/day/3)       | [day03.fsx](src/day03.fsx) | ✔️     | ✔️     |
| [Day 4: Passport Processing](https://adventofcode.com/2020/day/4)       | [day04.fsx](src/day04.fsx) | ✔️     | ✔️     |
| [Day 5: Binary Boarding](https://adventofcode.com/2020/day/5)           | [day05.fsx](src/day05.fsx) | ✔️     | ✔️     |
| [Day 6: Custom Customs](https://adventofcode.com/2020/day/6)            | [day06.fsx](src/day06.fsx) | ✔️     | ✔️     |
| [Day 7: Handy Haversacks](https://adventofcode.com/2020/day/7)          | [day07.fsx](src/day07.fsx) | ✔️     | ✔️     |
| [Day 8: Handheld Halting](https://adventofcode.com/2020/day/8)          | [day08.fsx](src/day08.fsx) | ✔️     | ✔️     |
| [Day 9: Encoding Error](https://adventofcode.com/2020/day/9)            | [day09.fsx](src/day09.fsx) | ✔️     | ✔️     |
| [Day 10: Adapter Array](https://adventofcode.com/2020/day/10)           | [day10.fsx](src/day10.fsx) | ✔️     | ✔️     |
| [Day 11: Seating System](https://adventofcode.com/2020/day/11)          | [day11.fsx](src/day11.fsx) | ✔️     | ✔️     |
| [Day 12: Rain Risk](https://adventofcode.com/2020/day/12)               | [day12.fsx](src/day12.fsx) | ✔️     | ✔️     |
| [Day 13: Shuttle Search](https://adventofcode.com/2020/day/13)          | [day13.fsx](src/day13.fsx) | ✔️     | ✔️     |
| [Day 14: Docking Data](https://adventofcode.com/2020/day/14)            | [day14.fsx](src/day14.fsx) | ✔️     | ✔️     |
| [Day 15: Rambunctious Recitation](https://adventofcode.com/2020/day/15) | [day15.fsx](src/day15.fsx) | ✔️     | ✔️     |
| [Day 16: Ticket Translation](https://adventofcode.com/2020/day/16)      | [day16.fsx](src/day16.fsx) | ✔️     | ✔️     |
| [Day 17: Conway Cubes](https://adventofcode.com/2020/day/17)            | [day17.fsx](src/day17.fsx) | ✔️     | ✔️     |
| [Day 18: Operation Order](https://adventofcode.com/2020/day/18)         | [day18.fsx](src/day18.fsx) | ✔️     | ✔️     |
| [Day 19: Monster Messages](https://adventofcode.com/2020/day/19)        | [day19.fsx](src/day19.fsx) | ✔️     | ✔️     |
| [Day 20: Jurassic Jigsaw](https://adventofcode.com/2020/day/20)         |                            |        |        |
| [Day 21: Allergen Assessment](https://adventofcode.com/2020/day/21)     | [day21.fsx](src/day21.fsx) | ✔️     | ✔️     |
| [Day 22: Crab Combat](https://adventofcode.com/2020/day/22)             | [day22.fsx](src/day22.fsx) | ✔️     | ✔️     |
| [Day 23: Crab Cups](https://adventofcode.com/2020/day/23)               |                            |        |        |
| [Day 24: Lobby Layout](https://adventofcode.com/2020/day/24)            |                            |        |        |
| [Day 25: Combo Breaker](https://adventofcode.com/2020/day/25)           |                            |        |        |
