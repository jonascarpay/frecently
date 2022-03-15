# frecently
[![frecently on hackage](https://img.shields.io/hackage/v/frecently)](http://hackage.haskell.org/package/frecently)
[![frecently on Stackage Nightly](https://stackage.org/package/frecently/badge/nightly)](https://stackage.org/nightly/package/frecently)

Extremely simple CLI tool for maintaining a [frecency](https://en.wikipedia.org/wiki/Frecency) history.

The intended use case is to add a frecency-based search history to CLI tools like [rofi](https://github.com/davatorium/rofi) or [dmenu](https://tools.suckless.org/dmenu/).

### Usage

`frecently` supports 4 commands:

- `frecently HISTORY_FILE bump STRING` adds 1 to `STRING`'s frecency score.
- `frecently HISTORY_FILE delete STRING` removes `STRING`.
- `frecently HISTORY_FILE view` prints the entries in the history file in order of descending score.
- `frecently HISTORY_FILE scores` is like `view`, but also shows the scores.

If the history file does not exist yet, it is created.

### Example

```console
$ frecently .history view           # .history doesn't exist yet, which is handled as if it were empty
$ frecently .history bump query1    # we create .history, and give query1 a frecency of 1
$ frecently .history view
query1
$ frecently .history bump query2
$ frecently .history view           # since query1 has decayed, it appears below query2
query2
query1
$ frecently .history bump query1
$ frecently .history view
query1
query2
$ frecently .history scores
1.99998         query1
0.99999         query2
```

### Fixed entries

It can be useful to force certain strings to be present in the output.
An example is when you use rofi to choose from a fixed set of options, like a power menu.
If you pass these strings as extra arguments to `view` or `scores`, and they are not in the history already, they will be listed at the end, as if they had a score of 0.

```console
$ frecently .history view poweroff sleep reboot
poweroff
reboot
sleep
$ frecently .history bump sleep
$ frecently .history view poweroff sleep reboot
sleep
poweroff
reboot
$ frecently .history view
sleep
```

### Installation

`frecently` is built like any other Haskell application.

For Nix users: the `flake.nix` file exposes the executable both directly and as an overlay.

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
