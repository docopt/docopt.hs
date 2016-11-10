Docopt.hs
=========

A Haskell port of python's [docopt](http://docopt.org).

----------

## Want a command-line interface *without* building a parser?

How about writing your help text first, and getting a parser for free!

Save your help text to a file (i.e. `USAGE.txt`):

    Usage:
      myprog cat <file>
      myprog echo [--caps] <string>

    Options:
      -c, --caps    Caps-lock the echoed argument

Then, in your `Myprog.hs`:

```haskell
{-# LANGUAGE QuasiQuotes #-}
import Control.Monad (when)
import Data.Char (toUpper)
import System.Environment (getArgs)
import System.Console.Docopt

patterns :: Docopt
patterns = [docoptFile|USAGE.txt|]

getArgOrExit = getArgOrExitWith patterns

main = do
  args <- parseArgsOrExit patterns =<< getArgs

  when (args `isPresent` (command "cat")) $ do
    file <- args `getArgOrExit` (argument "file")
    putStr =<< readFile file

  when (args `isPresent` (command "echo")) $ do
    let charTransform = if args `isPresent` (longOption "caps")
                          then toUpper
                          else id
    string <- args `getArgOrExit` (argument "string")
    putStrLn $ map charTransform string
```

That's it! No Template Haskell, no unreadable syntax, no learning yet *another* finicky API. Write the usage patterns you support, and docopt builds the appropriate option parser for you (internally using [`parsec`](http://hackage.haskell.org/package/parsec)). If your user invokes your program correctly, you query for the arguments they provided. If the arguments provided do not match a supported usage pattern, you guessed it: docopt automatically prints the help text and exits!


Installation
------------

    cabal sandbox init
    cabal install docopt

API Reference
-------------

See [the package on hackage](https://hackage.haskell.org/package/docopt)


Help text format
================

Docopt only cares about 2 parts of your help text:

- **Usage patterns**, e.g.:

  ```
      Usage:
        my_program [-hs] [-o=<file>] [--quiet | --verbose] [<input>...]
  ```
  These begin with `Usage:` (case-insensitive), and end with a blank line.

- **Option descriptions**, e.g.:

  ```
      Options:
        -h --help    show this
        -s --sorted  sorted output
        -o=<file>    specify output file
                     [default: ./test.txt]
        --quiet      print less text
        --verbose    print more text
  ```

  Any line after the usage patterns that begins with a `-` is treated as an option description (though an option's default may be on a different line).

Usage Patterns
--------------

- #### `<argument>` or `ARGUMENT`

  Positional arguments. Constructed via `argument`, i.e. `argument "arg"` matches an `<arg>` element in the help text, and `argument "ARG"` matches an `ARG` element.

- #### `--flag` or `--option=<arg>`

  Options are typically optional (though this is up to you), and can be either boolean (present/absent), as in `--flag`, or expect a trailing argument, as in `--option=<arg>` or `--option=ARG`. Arguments can be separated from the option name by an `=` or a single space, and can be specified as `<arg>` or `ARG` (consistency of style is recommended, but it is not enforced).

  Short-style options, as in `-f` or `-f ARG` or `-f=<arg>`, are also allowed. Synonyms between different spellings of the same option (e.g. `-v` and `--verbose`) can be established in the option descriptions (see below). Short-style options can also be stacked, as in `-rfA`. When options are stacked, `-rfA` is effectively equivalent to `(-r | -f | -A)...` to the argument parser.

  You can match a long-style option `--option` or `--option=<arg>` with `longOption "option"`, and a short-style option `-f` `or -f=<arg>` with `shortOption 'f'`. _Note that neither `--option=<arg>` nor `-f=<arg>` would be matched by `argument "arg"`._

- #### `command`

  Anything not recognized as a positional argument or a short or long option is treated as a command (or subcommand, same thing to docopt). A command named `pull` can be matched with `command "pull"`.

- #### `[]` (brackets) e.g. `command [--option]`

  Patterns inside brackets are **optional**.

- #### `()` (parens)

  Patterns inside parens are **required** (the same as patterns *not* in `()` are required). Parens are useful if you need to group some elements, either for use with `|` or `...`.

- #### `|` (pipe) e.g. `command [--quiet | --verbose]`

  A pipe `|` separates mutually exclusive elements in a group. A group could be elements inside `[]`, `()`, or the whole usage line.

  ```
      Usage:
        myprog command [--opt1 | --opt2]  # valid
        myprog go (left | right)          # valid
        myprog -v | -h                    # valid
  ```

  When elements are separated by a pipe, the elements are tried from left to right until one succeeds. At least one of the elements are required unless in an eplicitly optional group surrounded by `[]`.

- #### `...` (ellipsis) e.g. `command <file>...`

  An ellipsis can trail any element or group to make it repeatable. Repeatable elements will be accumulated into a list of occurrences.

- #### `[options]` (case sensitive)

  The string `[options]` is a shortcut to match any options specified in your option descriptions.

- #### `[-]` and `[--]`

  Single hyphen `-` is used by convention to specify using `stdin` as input instead of reading a file. Double hyphen `--` is typically used to manually separate leading options from trailing positional arguments. Both of these are treated as `command`s, and so are perfectly legal in usage patterns. They are typically optional elements, but can be required if you drop the `[]`. These are treated as commands and can be matched with `command "-"` or `command "--"`, whether they're wrapped `[-]` or not.

Option descriptions
-------------------

Option descriptions establish:
- which short and long options are synonymous
- whether an option expects an argument or is a simple flag
- if an option's argument has a default value

**Rules**:

- Any line *after* the usage patterns whose first non-space character is a `-` is treated as an option description. (`Options:` prefix line not required).

  ```
      Options: --help       # invalid: line does not start with '-'
               --verbose    # good
  ```

- Options on the same line will be treated by the parser as synonyms (everywhere interchangeable). Synonymous options are separated by a space (with optional comma):

  ```
      Usage:
        myprog --help | --verbose

      Options:
        -h, --help      Print help text
        -v --verbose    Print help text twice
  ```

  Here, `myprog --help` and `myprog -h` will both work the same, as will `myprog --verbose` and `myprog -v`.

- If any synonymous options are specified in the description with an argument, the option parser will expect an argument for all synonyms. If not, all synonyms will be treated as flags.

  ```
      Usage:
        myprog analyze [--verbose] <file>

      Options:
        --verbose, -v LEVEL   The level of output verbosity.
  ```

  Here, in the arguments `myprog analyze --verbose ./file1.txt` would be invalid, because `-v` *and its synonyms* expect an argument, so `./file1.txt` is captured as the argument of `--verbose`, *not* as the positional argument `<file>`. Be careful!

  Options can be separated from arguments with a single space or a `=`, and arguments can have the form `<arg>` or `ARG`. Just be sure to separate synonyms and arguments from the beginning of the description by **at least 2 spaces**.

  ```
      --opt1 ARG1   Option 1.
      --opt2=<arg2> Option 2.        # BAD: use 2 spaces
      -a <arg3>     Option 3.
      -b=ARG4       Option 4.
  ```

- Options that expect arguments can be given a default value, in the form `[default: <default-val>]`. Default values do not need to be on the same line

  ```
      --host=NAME       Host to listen on. [default: localhost]
      --port=PORT       Port number [default: 8080]
      --directory=DIR   This option has an especially long description
                        explaining its meaning. [default: ./]
  ```

----------------


#### Differences from reference python implementation:

  - does not automatically exclude from the `[options]` shortcut options that are already used elsewhere in the usage pattern (e.g. `usage: prog [options] -a` will try to parse `-a` twice).

  - does not automatically resolve partially-specified arguments, e.g. `--verb` does not match where `--verbose` is expected. This is planned to be deprecated in future versions of docopt, and will likely not be implemented in docopt.hs

  - is not insensitive to the ordering of adjacent options, e.g. `usage: prog -a -b` does not allow `prog -b -a` (reference implementation currently does).
