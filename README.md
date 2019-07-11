# [Uuuu](https://www.youtube.com/watch?v=jjD9WzW6dK4)

Uhuhuhuhuhuh! `uuuu` (Universal Unifier to Unicode Un OCaml) is a little library
to normalize an ISO-8859 input to Unicode. This library uses tables provided by
the Unicode Consortium:

[Unicode table](https://ftp.unicode.org/Public/MAPPINGS/ISO8859/)

This project takes tables and converts them to OCaml code. Then, it provides a
non-blocking decoder to translate ISO-8859 codepoint to UTF-8 codepoint.

## How to use it?

`uuuu` has an _dbuenzli_ interface. So it should be easy to use it and trick on
it. `uuuu` has a simple goal, offer a general way to decode an ISO8859 input and
normalize it to unicode codepoints. We need to be able to control
memory-consumption and ensure to offer a non-blocking computation.
