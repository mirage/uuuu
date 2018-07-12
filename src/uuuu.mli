type encoding =
  [ `ISO_8859_1
  | `ISO_8859_2
  | `ISO_8859_3
  | `ISO_8859_4
  | `ISO_8859_5
  | `ISO_8859_6
  | `ISO_8859_7
  | `ISO_8859_8
  | `ISO_8859_9
  | `ISO_8859_10
  | `ISO_8859_11
  | `ISO_8859_13
  | `ISO_8859_14
  | `ISO_8859_15
  | `ISO_8859_16 ]

val encoding_of_string: string -> encoding
val encoding_to_string: encoding -> string

type 'kind decoder constraint 'kind = [< encoding ]
type src = [ `Manual | `Channel of in_channel | `String of string ]
type decode = [ `Await | `End | `Uchar of Uchar.t | `Malformed of string ]

val src: 'kind decoder -> Bytes.t -> int -> int -> unit

val decoder: ([< encoding ] as 'kind) -> src -> 'kind decoder

val decode: 'kind decoder -> decode

val decoder_line: 'kind decoder -> int
val decoder_column: 'kind decoder -> int
val decoder_byte_count: 'kind decoder -> int
val decoder_count: 'kind decoder -> int
val decoder_src: 'kind decoder -> src
val decoder_kind: 'kind decoder -> 'kind

module Char: sig
  val is_valid: encoding -> char -> bool
  val equal: encoding -> char -> char -> bool
  val compare: encoding -> char -> char -> int
  val unicode: encoding -> char -> Uchar.t
end

module String: sig
  type 'a folder = 'a -> int -> [ `Malformed of string | `Uchar of Uchar.t ] -> 'a

  val fold: encoding -> ?off:int -> ?len:int -> 'a folder -> 'a -> string -> 'a
end
