# hed

A selection-based text editor with simplistic interface.

## Principles

You can select multiple lines and have a cursor on them all at the same time.

You can have a selection on line that includes many characters.

You can then execute actions on the lines with those selection.

See the full command list [here](./commands.md)

## Building

Building requires `stack` and nothing else.
You can get it [here](https://docs.haskellstack.org)

After you got stack, run the following commands:
```
git clone https://github.com/d86leader/hed.git
cd hed
stack build
```

Then to run hed simply execute
```
stack exec -- hed-exe
```
