# frecently
[![frecently on hackage](https://img.shields.io/hackage/v/frecently)](http://hackage.haskell.org/package/frecently)
[![frecently on Stackage Nightly](https://stackage.org/package/frecently/badge/nightly)](https://stackage.org/nightly/package/frecently)

Extremely simple CLI tool for maintaining a [frecency](https://en.wikipedia.org/wiki/Frecency) history.

The intended use case is to add a frecency-based search history to CLI tools like [rofi](https://github.com/davatorium/rofi) or [dmenu](https://tools.suckless.org/dmenu/).

### Usage

`frecently` supports 4 commands:

- `frecently HISTORY_FILE bump STRING` adds 1 to `STRING`'s score.
- `frecently HISTORY_FILE delete STRING` removes `STRING`.
- `frecently HISTORY_FILE view [STRING*]` prints the entries in the history file in order of descending score.

  Passing string arguments will, if the string is not already in the history, append them in the printed list _as if their score were 0_.
  This is useful when choosing from a fixed set of answers, like with `rofi`'s `-no-custom` flag.

- `frecently HISTORY_FILE debug [STRING*]` prints the contents of a history file twice; first, as it is on disk, and second, as it would be if we applied decay to it.

You don't need to manually make sure a history file exists; they are created if not yet present.

### CLI Example

```console
$ frecently .freqs view
$ frecently .freqs bump query1
$ frecently .freqs view
query1
$ frecently .freqs bump query2
$ frecently .freqs view
query2
query1
$ frecently .freqs bump query1
$ frecently .freqs view
query1
query2
$ frecently .freqs debug
Tue Mar 15 08:53:16 PM JST 2022

1.99999         query1
1.00000         query2
```

### Implementation details

The history file contains two pieces of information;
  - a time stamp of when scores were last calculated
  - a list of unique strings and their scores.

Scores are recalculated every time we `bump` a string.
The calculation is as follows: `score_new = score_old * 0.5 ^ ((time_new - time_old) / 30 days)`.
In other words, scores have a half-time of 30 days.
If a score drops below 0.1, it is removed from the list.
This means that a string that you bump only once is removed after about 100 days.
This is currently not configurable, but might be in the future.

A bumped string has 1 added to its score.

White space s always stripped from the beginning and end of strings

### Comparison with other tools

This tool was inspired by [frece](https://github.com/YodaEmbedding/frece).
I like the ideas behind `frece`, but I think the execution is more complicated than it needs to be.
`frecently` is both simpler and easier to integrate into CLI applications.
