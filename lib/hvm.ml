module Term = Term
module Irepr = Irepr
module Compile = struct
  let compile t = Compile.compile t
end

module Parsing = struct
  module Parse = Parse
  module Lex = Lex
end

module type VM = sig
  val run : Irepr.cl -> Irepr.regv
end

module Vm : VM = Vm

module Cfg = struct
  type t = Cfg.t
  type fullcfg = Cfg.t

  let mkcfg = Cfg.mkcfg
end

