
type adc_rr = [
  | `ADC64rr
  | `ADC32rr
  | `ADC16rr
  | `ADC8rr
] [@@deriving sexp]

type adc_rm = [
  | `ADC64rm
  | `ADC32rm
  | `ADC16rm
  | `ADC8rm
] [@@deriving sexp]

type adc_rax = [
  | `ADC64i32
  | `ADC32i32
  | `ADC16i16
  | `ADC8i8
] [@@deriving sexp]

type adc_ri8 = [
  | `ADC64ri8
  | `ADC32ri8
  | `ADC16ri8
] [@@deriving sexp]

type adc_ri = [
  | `ADC32ri
  | `ADC16ri
  | `ADC8ri
] [@@deriving sexp]

type adc_reg = [ adc_rr | adc_rm | adc_rax | adc_ri8 | adc_ri] [@@deriving sexp]

type adc_mr = [
  | `ADC64mr
  | `ADC32mr
  | `ADC16mr
  | `ADC8mr
] [@@deriving sexp]

type adc_mi8 = [
  | `ADC64mi8
  | `ADC32mi8
  | `ADC16mi8
] [@@deriving sexp]

type adc_mi = [
  | `ADC32mi
  | `ADC16mi
  | `ADC8mi
] [@@deriving sexp]

type adc_mem = [ adc_mr | adc_mi8 | adc_mi ] [@@deriving sexp]

type adc64_i32 = [
  | `ADC64mi32
  | `ADC64ri32
] [@@deriving sexp]

type adcx = [ adc_reg | adc_mem | adc64_i32] [@@deriving sexp]

