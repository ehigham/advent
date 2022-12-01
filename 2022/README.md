# Advent of Code 2021
Solutions to [Advent of Code 2022](https://adventofcode.com/2022) in Haskell

[![Advent 2022 Haskell CI](https://github.com/ehigham/advent/actions/workflows/2022-haskell.yaml/badge.svg)](https://github.com/ehigham/advent/actions/workflows/2022-haskell.yaml)

## Getting Started

Ensure you have [`stack`](https://docs.haskellstack.org/en/stable/README/) installed and on your path.

To build the source:
```bash
$ stack build
```

Once built, you can run any of the challenges via
```bash
$ stack exec dayN -- data/dayN/input
```
where `N` is the day number (1 to 25)

## Running the tests

You can build and run the unit tests via executing
```bash
$ stack test
```

## Running the benchmarks

A number of challenges also include benchmarks. You can run them via
```bash
$ stack bench
```
