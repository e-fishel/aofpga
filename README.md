## Credits
The template was taken under the MIT License from [Jane Street's Hardcaml Template Project](https://github.com/janestreet/hardcaml_template_project/), and adapted to be a little bit simpler (that's what the 3 demo_range_finder's are for).

## How to Run
To run the project, you'll need OCaml 5.2.0+ox and a few libraries. Run this command to install all the necessary packages into your switch:

``` sh
$ opam install core hardcaml hardcaml_test_harness hardcaml_waveterm ppx_hardcaml ppx_jane re
```

If you get an error, make sure you're in a 5.2.0+ox switch. It turns out that you **need** the +ox version of 5.2.0, because as I found out, `hardcaml_test_harness` only exists in that version (maybe some others, but the only one out of those which I checked). To get intellisense, you'd better be prepared to suffer... I had to uglily patch `core_kernel` and `utop`, because of an error about the version I got in `ocamllsp` (the version showed up as "*NO_VERSION_UTIL*").

Then, build the project:

``` sh
$ dune build
```

To run the code for, say, day 3, run the command:

``` sh
$ dune exec src/day03/test.exe
```

It displays the waveform output thanks to the beautiful `hardcaml_test_harness` package.

## Backstory
I actually had no prior experience with Hardcaml, Verilog, FPGA design, or even OCaml.
I randomly stumbled upon the challenge when browsing LinkedIn and found it cool. My first AOC was in 2022, but that was also my last lol. My process of learning all this went as follows:
- I'm first [learning OCaml](https://www.youtube.com/playlist?list=PLre5AT9JnKShBOPeuiD9b-I4XROIJhkIU) while doing some [Clash of Code](https://www.codingame.com/multiplayer/clashofcode) rounds
- While learning OCaml, I peek into [this Hardcaml course](https://hardcaml-mini-course-at-stevens.github.io/hardcaml-docs/)
- I realize I don't understand the `~clock` syntax, and don't even talk about modules, signatures, packages, opam or anything (I realized too late that intellisense was a big part of what I was missing)
- I discover utop can handle Hardcaml (you need to `#require "hardcaml";;
open Hardcaml;;
open Hardcaml.Bits;;
#install_printer Bits.pp;;`), and I finally get to play around with some basic Hardcaml
- ... TODO (run the range_demo project; [learn verilog](https://hdlbits.01xz.net/wiki/Main_Page); discover [this awesome website](https://ocamlstreet.gitbook.io/hardcaml-wiki/); play around with basic circuits in 5.1.1)
- I suffer greatly trying to get both intellisense and the `hardcaml_test_harness` package
- ... TODO (i ***FINALLY*** conquer that)
- I am currently writing this README
- I will finish writing this README and fill in the TODO's
