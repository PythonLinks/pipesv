# Pipelined System Verilog (PipeSV)

Pipelines are very important in digital circuit design, but they are
not directly supported by System Verilog; there are no `stage` nor
`pipeline` keywords.  PipeSV is a preprocessor which adds the `stage`
and `pipeline` keywords, to System Verilog. 

```diff
- March 1, 2026 This is the second releae.  All 653 tests pass.
- AFAIK the functionality is complete.
- This software is being used every day, and is rapidly evolving.
- Check out the dev branch!
```

Here is an example extracted from [the larger pipeline demo
source code](./demo/pipeline.pl).  

```
initial pixels = initializePixels(pixels);
pipeline
  stage #{increment}
      pixels <= incrementColor(pixels#{+0});
  stage #{editImage}
      pixels <= createEdge(pixels, counter);
  stage #{addNoise}
      pixels <= addNoise(pixels);
  stage #{firstDelay}
      pixels <= pixels;
  stage #{secondDelay}
      pixels <= pixels;    
  stage #{averagePixels}
      var PixelArray average2 ;
      average2  <= average(pixels#{-2}, pixels#{-1});
  stage #{thirdDelay}
      average2 <= average2;
endpipeline
```

PipeSV automatically renames variables to variableName_stageName, and then accesses them using either relative stage names #{-1}, or absolute stage names #{addNoise}. 

For more details, you also read [the generated System
Verilog](./demo/pipeline.sv). The PipeSVsource code is more terse and
readable than traditional System Verilog, so it can be modified
faster.  The syntax prevents a number of possible inconsistencies with
indexes in traditionally written code.


## Motivation

The [inspiration for this pipeline
abstraction](https://docs.spade-lang.org/pipelines.html) comes from the
Spade HDL. SpinalHDL also has great pipeline abstractions.



There are a number of hardware definition languages (HDLs), but in the
author's experience, they are either hard to learn, or add a great
deal of complexity to the generated output, making the resulting
System Verilog harder to read and debug.

PipeSV uses System Verilog as its base language and just adds two very
easy to learn keywords.  The output can remain as readable SystemVerilog, or can be converted to Verilog using sv2v converters..


## Technical Spec

PipeSV adds the keywords `pipeline` ,
`stage #{stageName}`, `endstage`, and `endpipeline` to System Verilog. Within
a pipeline are stages.  Within stages you can include declarations,
statements and blocks. The stage provides a default `always @(posedge
clock)` to any dangling statements that need it, but one can also add always
blocks with custom sensitivity lists.  Multiple stages can have a
`reg`, `wire`, or `logic` variable with the same name, the transpiler appends the stage
name to variable names to prevent clashes.  The resulting verilog:

```pixels_addNoise <= addNoise(pixels_input);```

is much more readable and maintainable than 

```pixels[1] <= addNoise(pixels[0]);```

More importantly the source code can be quickly edited without risk of making an error in the offsets.  

```
PipeSV Source (*.pl)
    ↓
Read File 
    ↓
Generate AST 
    ↓    
Optional PipeSV Conversion of Pipeline and Stage Keywords
    ↓    
SystemVerilog
    ↓
Optinal Sv2v Conversion
    ↓
Verilog
    ↓
Syhthesis
    ↓
Place and route
```

## PipeSV Vs sv2v

PipeSV is a fork of [the `sv2v`](https://github.com/zachjs/sv2v) System
Verilog parser.  PipeSV adds `Pipeline` and `Stage` nodes to the sv2v
Abstract Syntax Tree (AST).  PipeSV also processes those nodes to
create legal System Verilog, which can then be further transformed by
sv2v into legal Verilog and fed to the Yosys synthesiser.  So PipeSV is also able to convert
pipelined System Verilog to Verilog and synthesize with Yosys.


`sv2v` converts SystemVerilog ([IEEE 1800-2017]) to
Verilog ([IEEE 1364-2005]), with a focus on the [extended version of
Verilog](https://yosyshq.readthedocs.io/projects/yosys/en/latest/using_yosys/verilog.html#supported-features-from-systemverilog)
supported by the open source [Yosys synthesis
suite](https://yosyshq.net/yosys/).  `sv2v` has an active user base
among those wishing to synthesize SystemVerilog with Yosys.  `sv2v`
has an extensive test suite, over 1000 files, meaning that it is very
mature software.  While there is a C++ System Verilog to Verilog
converter, the Haskell transpiler runs quite fast (0.1 seconds), and
is a more productive development environment.


## Dependencies

All of PipeSV's dependencies are free and open-source.

* Build Dependencies
    * [Haskell Stack](https://www.haskellstack.org/) - Haskell build system
    * Haskell dependencies are managed in `sv2v.cabal`
* Test Dependencies
    * [Icarus Verilog](https://steveicarus.github.io/iverilog/) - for Verilog
      simulation
    * [shUnit2](https://github.com/kward/shunit2) - test framework
    * Python 3.x - for evaluating certain test cases


## Installation

### Building from source

You must have [Stack] installed to build sv2v. Then you can:

[Stack]: https://www.haskellstack.org/

```
git clone https://github.com/PythonLinks/pipesv.git
cd pipesv
make
```

This creates the executable at `./bin/sv2v`. Stack takes care of installing
exact (compatible) versions of the compiler and sv2v's build dependencies.

You can install the binary to your local bin path (typically `~/.local/bin`) by
running `stack install`, or copy over the executable manually.

## Usage

PipeSV adds a single option to `sv2v`.  The --pipesv option processes
`pipeline` and `stage` keywords after preprocessing but before converting to Verilog.  From the repository root directory,
here is how to use PipeSV on a single file with the output going to stdout.

```
bin/sv2v --pass-through file.sv             # Parse and dump, no transforms
bin/sv2v --pipesv --pass-through file.sv    # PipeSV transforms only, show output
bin/sv2v --pipesv file.sv                   # PipeSV transforms + full sv2v Convert
```

The default is to display on stdout.
sv2v users should typically pass all of their SystemVerilog source files to
sv2v at once so it can properly resolve packages, interfaces, type parameters,
etc., across files. Using `--write=adjacent` will create a converted `.v` for
every `.sv` input file rather than printing to `stdout`. `--write`/`-w` can also
be used to specify a path to a `.v` output file. Undefined modules and
interfaces can be automatically loaded from library directories using
`--libdir`/`-y`.

Users may specify `include` search paths, define macros during preprocessing,
and exclude some of the conversions. Specifying `-` as an input file will read
from `stdin`.

Below is the current usage printout.

```
sv2v [OPTIONS] [FILES]

Preprocessing:
  -I --incdir=DIR           Add a directory to the include search path
  -y --libdir=DIR           Add a directory to the library search path used
                            when looking for undefined modules and interfaces
  -D --define=NAME[=VALUE]  Define a macro for preprocessing
     --siloed               Lex input files separately, so macros from
                            earlier files are not defined in later files
     --skip-preprocessor    Disable preprocessing of macros, comments, etc.
Conversion:
     --pass-through         Dump input without converting
  -p --pipesv               Run PipeSV transforms before conversion
  -E --exclude=CONV         Exclude a particular conversion (Always, Assert,
                            Interface, Logic, SeverityTask, or UnbasedUnsized)
  -v --verbose              Retain certain conversion artifacts
  -w --write=MODE/FILE/DIR  How to write output; default is 'stdout'; use
                            'adjacent' to create a .v file next to each input;
                            use a path ending in .v to write to a file; use a
                            path to an existing directory to create a .v within
                            for each converted module
     --top=NAME             Remove uninstantiated modules except the given
                            top module; can be used multiple times
Other:
     --oversized-numbers    Disable standard-imposed 32-bit limit on unsized
                            number literals (e.g., 'h1_ffff_ffff, 4294967296)
     --dump-prefix=PATH     Create intermediate output files with the given
                            path prefix; used for internal debugging
     --bugpoint=SUBSTR      Reduce the input by pruning modules, wires, etc.,
                            that aren't needed to produce the given output or
                            error substring when converted
     --help                 Display this help message
     --version              Print version information
     --numeric-version      Print just the version number
```

## Testing

Once the [test dependencies] are installed, tests can be run with `make test`.
GitHub Actions is used to [automatically test] commits. Please review the [test
documentation] for guidance on adding, debugging, and interpreting tests.

[test dependencies]: #dependencies
[test documentation]: test/README.md
[automatically test]: https://github.com/zachjs/sv2v/actions

There is also a [SystemVerilog compliance suite] that tests open-source tools'
SystemVerilog support. Although not every test in the suite is applicable, it
has been a valuable asset in finding edge cases.

[SystemVerilog compliance suite]: https://github.com/chipsalliance/sv-tests

