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

Then, in your `myprog.hs`:
    
```haskell

import Control.Monad (when)
import Data.Char (toUpper)
import System.Console.Docopt (optionsWithUsageFile, getArg, isPresent)

main = do
  opts <- optionsWithUsageFile "USAGE.txt"

  when (opts `isPresent` (command "cat")) $ do
    file <- opts `getArg` (argument "file")
    putStr =<< readFile file

  when (opts `isPresent` (command "echo")) $ do
    let charTransform = if opts `isPresent` (longOption "caps")
                          then toUpper
                          else id
    string <- opts `getArg` (argument "string")
    putStrLn $ map charTransform string

```

That's it! No Template Haskell, no unreadable syntax, no learning yet *another* finicky API. Write the usage patterns you support, and docopt builds the appropriate option parser for you (internally using [`parsec`](http://hackage.haskell.org/package/parsec)). If your user invokes your program correctly, you query for the arguments they provided. If the arguments provided do not match a supported usage pattern, you guessed it: docopt automatically prints the help text and exits!

----------

Installation
------------

For now, installation is manual. Soon this package will be registered on hackage, allowing you to install via:

    cabal install docopt

----------

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

- #### `<argument>`

  Positional arguments. Constructed via `argument`, i.e. `argument "arg"` matches an `<arg>` element in the help text.

- #### `--flag` or `--option=<arg>`

  Options are typically optional (though this is up to you), and can be either boolean (present/absent), as in `--flag`, or expect a trailing argument, as in `--option=<arg>`. Arguments can be separated from the option name by an `=` or a single space, and can be in `<arg>` form or `ARG` form (though consistency of style is recommended, it is not enforced). 

  Short-style options, as in `-f` or `-f ARG`, are also allowed. Synonyms between different spellings of the same option (e.g. `-v` and `--verbose`) can be established in the option descriptions (see below). Short-style options can also be stacked, as in `-rfA`. When options are stacked, `-rfA` is effectively equivalent to `(-r | -f | -A)...` to the argument parser.

  You can match a long-style option `--flag` with `longOption "flag"`, and a short-style option `-f` with `shortOption 'f'` The same constructor is used whether the option expects an argument or not.

- #### `command`

  Anything not recognized as a positional argument or a short or long option is treated as a command (or subcommand, same thing to docopt). A command named `pull` can be matched with `command "pull"`. 

- #### `[]` (brackets) e.g. `command [--option]`

  Patterns inside brackets are **optional**.

- #### `()` (parens)

  Patterns inside parens are **required** (the same as patterns *not* in `()` are required). Parens are useful if you need to group some elements, either for use with `|` or `...`.

- #### `|` (pipe) e.g. `command [--quiet | --verbose]`

  A pipe `|` separates mutually elements in a group. A group could be elements inside `[]`, `()`, or the whole usage line. 

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

  Single hyphen `-` is used by convention to specify using `stdin` as input instead of reading a file. Double hyphen `--` is typically used to manually separate leading options from trailing positional arguments. Both of these are treated as `command`s, and so are perfectly legal in usage patterns. They are typically optional elements, but can be required if you drop the `[]`. 

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

## Types

- **`Options`**

  This is the object you receive when docopt has successfully parsed your help text *and* the arguments passed to your program. 

- **`Expectation`**

  This is the data type for individual elements of your usage patterns (and internally, recursive patterns as well - it is the 'form' that docopt *expects* your program's arguments to take).

  Constructors:

  ```
  command        ==>  command "command"
  <argument>     ==>  argument "argument"
  --flag         ==>  longOption "flag"`
  --option=ARG   ==>  longOption "option"
  -c             ==>  shortOption 'c'`
  -c ARG         ==>  shortOption 'c'
  ```

## Option parsing

- **`optionsWithUsageFile :: FilePath -> IO Options`**

  Most basic options parser. Give it the path to a file with your help text, and it will read the file, build your option parser, and parse your program's options (via `getArgs`). If successful, you get an `Options`, and if not, it will print your help text and fail (hence `IO`).

- **`optionsWithUsageFileDebug :: FilePath -> IO Options`**

  Same as `optionsWithUsageFile`, but prints `ParseError`s if it fails, instead of your help text. Useful if you run into problems parsing options.

## Queries

- **`getArg :: Monad m => Options -> Expectation -> m String`**

  ``opts `getArg` exp`` returns the last value of `exp` specified in the arguments, or the default value (if one is specified), or `fail`s.

- **`isPresent :: Options -> Expectation -> Bool`**

  ``opts `isPresent` exp`` returns `True` if `exp` was given in the arguments, else `False`. Useful for use with flags.

- **`getAllArgs :: Options -> Expectation -> [String]`**

  ``opts `getAllArgs` exp`` returns the list of all occurrences of `exp` in the arguments, in order of last to first given. If none given, returns a singleton list of the default value, if specified. Otherwise returns an empty list. Useful for repeatable elements.

- **`getArgWithDefault :: Options -> String -> Expectation -> String`**

  ``getArgWithDefault opts "default" exp`` returns what `getArg opts exp` returns if it succeeds, or `"default"` if that fails.

These are the important basics, though there are others exposed by docopt, located in the `Public` submodule. Examples forthcoming!

----------

#### Differences from reference python implementation:

- does not automatically exclude from the `[options]` shortcut options that are already used elsewhere in the usage pattern (e.g. `usage: prog [options] -a` will try to parse `-a` twice).

- eagerly parses argv in usage-specified order, which means something like `usage: prog [-v | -vv]` will always fail on `prog -vv` (and other similar issues; I think these can all be resolved by intelligently reordering order-insensitive subpatterns, e.g. transforming `prog [-v | -vv]` to `prog [-vv | -v]`).

- does not automatically resolve partially-specified arguments, e.g. `--verb` does not match where `--verbose` is expected. This is planned to be deprecated in future versions of docopt, and will likely not be implemented in docopt.hs

- is not insensitive to the ordering of adjacent options, e.g. `usage: prog -a -b` does not allow `prog -b -a` (reference implementation currently does).