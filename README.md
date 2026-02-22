# ARMLiteC

An experimental C23 (ish) compiler for [ARMLite](https://peterhigginson.co.uk/ARMlite/) and
semi-hosted ARMv7 written in Rust \:D

https://github.com/user-attachments/assets/b875552b-229e-4a58-9646-2afb5234a5f0

I'm pretty sure this is the only compiler out there targetting ARMLite (though last time I said
something like that I was disappointed, so I'm choosing *not* to fact-check that!)

## Usage

```
cargo run [--release] [input-file [output-file]] [--armlite|--armv7]
```

For example, to compile the included demo program shown in the video:

```
cargo run ./build.c ./test.armlite
```

This compiler only supports unity builds - that is, you provide a single input C file which in turn
`#include`s the list of source files.

### Interactive mode

There's an interactive mode I haven't tested in months:

```
cargo run
```

Type `;;` and hit enter to trigger compilation of your input.

## Overview

To get a general idea of where this compiler is at, check the **Issues** tab, and/or have a look
through the sample C files!
