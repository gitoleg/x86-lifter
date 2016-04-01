type bt_rr = [
  | `BT64rr
  | `BT32rr
  | `BT16rr
] [@@deriving sexp]

type bt_ri = [
  | `BT64ri8
  | `BT32ri8
  | `BT16ri8
] [@@deriving sexp]

type bt_reg = [bt_rr | bt_ri] [@@deriving sexp]

type bt_mr = [
  | `BT64mr
  | `BT32mr
  | `BT16mr
] [@@deriving sexp]

type bt_mi = [
  | `BT64mi8
  | `BT32mi8
  | `BT16mi8
] [@@deriving sexp]

type bt_mem = [bt_mr | bt_mi] [@@deriving sexp]

type bt = [bt_reg | bt_mem] [@@deriving sexp]

type btc_rr = [
  | `BTC64rr
  | `BTC32rr
  | `BTC16rr
] [@@deriving sexp]

type btc_ri = [
  | `BTC64ri8
  | `BTC32ri8
  | `BTC16ri8
] [@@deriving sexp]

type btc_reg = [btc_rr | btc_ri] [@@deriving sexp]

type btc_mr = [
  | `BTC64mr
  | `BTC32mr
  | `BTC16mr
] [@@deriving sexp]

type btc_mi = [
  | `BTC64mi8
  | `BTC32mi8
  | `BTC16mi8
] [@@deriving sexp]

type btc_mem = [btc_mr | btc_mi] [@@deriving sexp]

type btc = [btc_reg | btc_mem] [@@deriving sexp]

type btr_rr = [
  | `BTR64rr
  | `BTR32rr
  | `BTR16rr
] [@@deriving sexp]

type btr_ri = [
  | `BTR64ri8
  | `BTR32ri8
  | `BTR16ri8
] [@@deriving sexp]

type btr_reg = [btr_rr | btr_ri] [@@deriving sexp]

type btr_mr = [
  | `BTR64mr
  | `BTR32mr
  | `BTR16mr
] [@@deriving sexp]

type btr_mi = [
  | `BTR64mi8
  | `BTR32mi8
  | `BTR16mi8
] [@@deriving sexp]

type btr_mem = [btr_mr | btr_mi] [@@deriving sexp]

type btr = [btr_reg | btr_mem] [@@deriving sexp]

type bts_rr = [
  | `BTS64rr
  | `BTS32rr
  | `BTS16rr
] [@@deriving sexp]

type bts_ri = [
  | `BTS64ri8
  | `BTS32ri8
  | `BTS16ri8
] [@@deriving sexp]

type bts_reg = [bts_rr | bts_ri] [@@deriving sexp]

type bts_mr = [
  | `BTS64mr
  | `BTS32mr
  | `BTS16mr
] [@@deriving sexp]

type bts_mi = [
  | `BTS64mi8
  | `BTS32mi8
  | `BTS16mi8
] [@@deriving sexp]

type bts_mem = [bts_mr | bts_mi] [@@deriving sexp]

type btx_rr = [bt_rr | btc_rr | btr_rr | bts_rr ] [@@deriving sexp]
type btx_ri = [bt_ri | btc_ri | btr_ri | bts_ri ] [@@deriving sexp]
type btx_mr = [bt_mr | btc_mr | btr_mr | bts_mr ] [@@deriving sexp]
type btx_mi = [bt_mi | btc_mi | btr_mi | bts_mi ] [@@deriving sexp]

type btx_reg = [bt_reg | btc_reg | btr_reg | bts_reg ] [@@deriving sexp]
type btx_mem = [bt_mem | btc_mem | btr_mem | bts_mem ] [@@deriving sexp]

type btx = [btx_reg | btx_mem] [@@deriving sexp]

type movx_rr = [
  | `MOV8rr
  | `MOV16rr
  | `MOV32rr
  | `MOV64rr
] [@@deriving sexp]

type movx_ri = [
  | `MOV8ri
  | `MOV16ri
  | `MOV32ri
  | `MOV64ri
  | `MOV64ri32
] [@@deriving sexp]

type movx = [
  | movx_rr
  | movx_ri
] [@@deriving sexp]

type t = [btx | movx] [@@deriving sexp]

type prefix = [
  | `LOCK_PREFIX
  | `REX64_PREFIX
  | `DATA16_PREFIX
  | `REP_PREFIX
  | `REPNE_PREFIX
] [@@deriving sexp]
