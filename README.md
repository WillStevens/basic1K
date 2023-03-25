# basic8080
A 1K BASIC interpreter for the Intel 8080

It will soon be the 50th birthday of the
Intel 8080 microprocessor. This is the
device that started the microcomputer
revolution. I wrote an emulator for it
recently, and decided to write a BASIC
interpreter in the spirit of the early
microprocessor BASICs that were developed
in the late 1970s. In this era, memory was
expensive (in 1975 1Kb of RAM cost nearly
£1000 in today's money) and hobbyists could
often only afford a few KB. I want to
understand how small a BASIC interpreter
can be.

This implementation is loosely based on
Tiny BASIC, but a key difference is that
basic8080 is tokenised rather than being
parsed at run-time. Whether or not this
is a net benefit for code size remains to
be seen.

I learned BASIC in the 1980s on the
ZX Spectrum and BBC Micro. An appealing
feature of the language for beginners is
that it builds on concepts that are already
familiar to most people - following
sequences of numbered steps, and the syntax
of mathematical expressions.
