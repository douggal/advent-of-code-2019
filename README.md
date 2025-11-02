# Advent of Code 2019

This repository contains my personal solutions to the [Advent of Code 2019](https://adventofcode.com) programming challenges, implemented in **Scala 2**.

Created:  Autumn 2021

My solution to each day's puzzles.  In Scala v2.13 unless otherwise noted.

> ⚠️ Puzzle descriptions and input files are not included due to copyright restrictions.  
> Please visit the official Advent of Code website to view the original puzzles.


### Notes
#### Day 01
Had to look up solution to part two on Reddit.  For each mass calculate the fuel needed with the recursive formula.
Have to do separately for each mass and not work once from the fuel sum from each mass.  Very surprising how close the two numbers are (4812287 vs incorrect 4815103, maybe coincidence with my input?).
Still not sure I understand it.
[AoC 2019 Day 1 Part Two](https://www.reddit.com/r/adventofcode/comments/k1h4bq/2019_day_1_part_2_spits_out_right_answer_for/)

#### Day 02
Works but used imperative programming style instead of FP

#### Day 03
Some functional programming style.  Seems code could be shorter?  It works for my input.

#### Day 04
Pulled in some tail recursion

#### Day 05
I too found the instructions confusing. I read them over and over. I first thought the task was to find errors in the 
input file, but finally realized it is just to validate the intcode 
computer program I wrote by getting all the tests to pass.

License
-------
Choose a license that suits your needs (MIT, Apache-2.0, etc.). If you want, I can add a LICENSE file and apply a default license for you.

Acknowledgements
----------------
- Advent of Code (https://adventofcode.com)
- Scala language and community resources
