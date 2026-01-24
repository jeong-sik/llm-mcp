/-!
Formal model for lossless interop (Chain JSON ↔ Mermaid).
This is a mechanized proof of the *model*, not of the OCaml implementation.
-/

namespace ChainInterop

structure Node where
  id : String
deriving Repr, DecidableEq

structure Edge where
  src : String
  dst : String
deriving Repr, DecidableEq

structure Chain where
  nodes : List Node
  edges : List Edge
deriving Repr, DecidableEq

structure Mermaid where
  graph : List Node × List Edge
  chainFull : Option Chain
deriving Repr, DecidableEq

def project (c : Chain) : List Node × List Edge :=
  (c.nodes, c.edges)

def embed (c : Chain) : Mermaid :=
  { graph := project c, chainFull := some c }

def infer (g : List Node × List Edge) : Chain :=
  { nodes := g.1, edges := g.2 }

def parse (m : Mermaid) : Chain :=
  match m.chainFull with
  | some c => c
  | none => infer m.graph

theorem lossless_roundtrip (c : Chain) : parse (embed c) = c := by
  rfl

theorem lossless_roundtrip_canonical (c : Chain) :
    (parse (embed c)).nodes = c.nodes ∧ (parse (embed c)).edges = c.edges := by
  constructor <;> rfl

end ChainInterop
