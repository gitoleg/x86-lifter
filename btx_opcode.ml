open Core_kernel.Std

type bt_rr = [
  | `BT64rr
  | `BT32rr
  | `BT16rr
] [@@deriving bin_io, sexp, compare, enumerate]

type bt_ri = [
  | `BT64ri8
  | `BT32ri8
  | `BT16ri8
] [@@deriving bin_io, sexp, compare, enumerate]

type bt_reg = [bt_rr | bt_ri] [@@deriving bin_io, sexp, compare, enumerate]

type bt_mr = [
  | `BT64mr
  | `BT32mr
  | `BT16mr
] [@@deriving bin_io, sexp, compare, enumerate]

type bt_mi = [
  | `BT64mi8
  | `BT32mi8
  | `BT16mi8
] [@@deriving bin_io, sexp, compare, enumerate]

type bt_mem = [bt_mr | bt_mi] [@@deriving bin_io, sexp, compare, enumerate]

type bt = [bt_reg | bt_mem] [@@deriving bin_io, sexp, compare, enumerate]

type btc_rr = [
  | `BTC64rr
  | `BTC32rr
  | `BTC16rr
] [@@deriving bin_io, sexp, compare, enumerate]

type btc_ri = [
  | `BTC64ri8
  | `BTC32ri8
  | `BTC16ri8
] [@@deriving bin_io, sexp, compare, enumerate]

type btc_reg = [btc_rr | btc_ri] [@@deriving bin_io, sexp, compare, enumerate]

type btc_mr = [
  | `BTC64mr
  | `BTC32mr
  | `BTC16mr
] [@@deriving bin_io, sexp, compare, enumerate]

type btc_mi = [
  | `BTC64mi8
  | `BTC32mi8
  | `BTC16mi8
] [@@deriving bin_io, sexp, compare, enumerate]

type btc_mem = [btc_mr | btc_mi] [@@deriving bin_io, sexp, compare, enumerate]

type btc = [btc_reg | btc_mem] [@@deriving bin_io, sexp, compare, enumerate]

type btr_rr = [
  | `BTR64rr
  | `BTR32rr
  | `BTR16rr
] [@@deriving bin_io, sexp, compare, enumerate]

type btr_ri = [
  | `BTR64ri8
  | `BTR32ri8
  | `BTR16ri8
] [@@deriving bin_io, sexp, compare, enumerate]

type btr_reg = [btr_rr | btr_ri] [@@deriving bin_io, sexp, compare, enumerate]

type btr_mr = [
  | `BTR64mr
  | `BTR32mr
  | `BTR16mr
] [@@deriving bin_io, sexp, compare, enumerate]

type btr_mi = [
  | `BTR64mi8
  | `BTR32mi8
  | `BTR16mi8
] [@@deriving bin_io, sexp, compare, enumerate]

type btr_mem = [btr_mr | btr_mi] [@@deriving bin_io, sexp, compare, enumerate]

type btr = [btr_reg | btr_mem] [@@deriving bin_io, sexp, compare, enumerate]

type bts_rr = [
  | `BTS64rr
  | `BTS32rr
  | `BTS16rr
] [@@deriving bin_io, sexp, compare, enumerate]

type bts_ri = [
  | `BTS64ri8
  | `BTS32ri8
  | `BTS16ri8
] [@@deriving bin_io, sexp, compare, enumerate]

type bts_reg = [bts_rr | bts_ri] [@@deriving bin_io, sexp, compare, enumerate]

type bts_mr = [
  | `BTS64mr
  | `BTS32mr
  | `BTS16mr
] [@@deriving bin_io, sexp, compare, enumerate]

type bts_mi = [
  | `BTS64mi8
  | `BTS32mi8
  | `BTS16mi8
] [@@deriving bin_io, sexp, compare, enumerate]

type bts_mem = [bts_mr | bts_mi] [@@deriving bin_io, sexp, compare, enumerate]

type btx_rr = [
  | bt_rr
  | btc_rr
  | btr_rr
  | bts_rr ] [@@deriving bin_io, sexp, compare, enumerate]

type btx_ri = [
  | bt_ri
  | btc_ri
  | btr_ri
  | bts_ri ] [@@deriving bin_io, sexp, compare, enumerate]

type btx_mr = [
  | bt_mr
  | btc_mr
  | btr_mr
  | bts_mr
] [@@deriving bin_io, sexp, compare, enumerate]

type btx_mi = [
  | bt_mi
  | btc_mi
  | btr_mi
  | bts_mi
] [@@deriving bin_io, sexp, compare, enumerate]

type btx_reg = [
  | bt_reg
  | btc_reg
  | btr_reg
  | bts_reg
] [@@deriving bin_io, sexp, compare, enumerate]

type btx_mem = [
  | bt_mem
  | btc_mem
  | btr_mem
  | bts_mem
] [@@deriving bin_io, sexp, compare, enumerate]

type t = [
  | btx_reg
  | btx_mem
] [@@deriving bin_io, sexp, compare, enumerate]
