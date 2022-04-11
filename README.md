# frecently
[![frecently on hackage](https://img.shields.io/hackage/v/frecently)](http://hackage.haskell.org/package/frecently)
[![frecently on Stackage Nightly](https://stackage.org/package/frecently/badge/nightly)](https://stackage.org/nightly/package/frecently)

Extremely simple CLI tool for maintaining a [frecency](https://en.wikipedia.org/wiki/Frecency) history.

The intended use case is to add a frecency-based search history to CLI tools like [dmenu](https://tools.suckless.org/dmenu/), [rofi](https://github.com/davatorium/rofi), or [fzf](https://github.com/junegunn/fzf).


### Examples

#### Basic CLI:

```console
$ frecently view .history      # .history doesn't exist yet. By default, all commands treat a missing file as empty
$ frecently bump .history foo  # creates .history, and bumps foo
$ frecently bump .history bar
$ frecently view .history      # `view` shows all entries, ordered by frecency
bar
foo
$ echo -e "bar\nbaz" | frecently view .history --augment   # with --augment, frecently accepts extra entries on stdin (newline-separated) that should always appear in the output
bar
foo
baz
$ echo -e "foo\nbar\nbaz" | frecently scores .history --augment   # `scores` has the same interface as `view`, but shows the entire scores table
weighted score  hourly          daily           monthly
  178.663539      0.985473        0.999390        0.999980      bar
  178.267277      0.983010        0.999286        0.999976      foo
    0.000000      0.000000        0.000000        0.000000      baz
$ echo -e "bar\nbaz" | frecently view .history --augment --restrict  # with --restrict, we exclusively output entries that appear on stdin
bar
baz
```

#### Bash script for dmenu web searches with history

```bash
set -eo pipefail
HISTORY=~/.search-history
QUERY=$(frecently view $HISTORY | dmenu -p "Web search:")

if [[ -n "$QUERY" ]]; then
  frecently bump "$HISTORY" "$QUERY"
  xdg-open "https://www.duckduckgo.com/?q=$QUERY"
fi
```

See `frecently --help` for more detailed usage information.

### Installation

`frecently` is currently only distributed as source through GitHub, or through Hackage.

It is built like any other Haskell application.

For Nix users: the `flake.nix` file exposes the executable both directly and as an overlay.

### Implementation details

`frecently` works by maintaining three exponentially-decaying energy per entry.
The three energies each have different half-lives: an hour, a day, and a month.
We bump an entry by adding 1 to each of its three energies.

An entry's frecency score is calculated by multiplying each of these three energies by a weight.
The weights default to 160, 20, and 1, for the hourly, daily, and monthly energies, respectively, but can be overridden on the CLI.

Entries' energies are updates _only_ when the history file is used in a `bump` or `touch` command, and when we do, we update every entry's energy simultaneously.
Always updating all entries at the same time allows score calculations to be very efficient, since the decay factors since the last update are the same for every entry.

### Comparison with other tools

This tool was inspired by [frece](https://github.com/YodaEmbedding/frece).
I really like the idea behind `frece`, but I think the execution is more complicated than it needs to be.
`frecently` is both simpler and easier to integrate into CLI applications.
