
From Stdlib Require MSets.MSetWeakList.
From Stdlib Require Structures.DecidableTypeEx.
From Stdlib Require Import Structures.Equalities.
From Stdlib Require Import Lists.List.
Import PeanoNat.Nat.

Module NatEqMDT <: DecidableTypeEx.MiniDecidableType.
	Definition t := nat.
	Definition eq_dec := PeanoNat.Nat.eq_dec.
End NatEqMDT.

Module NatEq  := DecidableTypeEx.Make_UDT NatEqMDT.
Module NatSet := MSetWeakList.Make NatEq.

Definition NatPair: Type := (nat * nat).

Module NatPairUDT := DecidableTypeEx.PairUsualDecidableType NatEq NatEq.
Module NatPairEq  := DecidableTypeEx.Make_UDT NatPairUDT.
Module NatPairSet := MSetWeakList.Make NatPairEq.

(* ----------------------------------------------------------------------- *)
(* digraph *)

Record digraph := {
	V: NatSet.t_;
	E: NatPairSet.t_;
}.

Fixpoint make_verts (l: list nat) : NatSet.t_ := match l with
| nil         => NatSet.empty
| val :: rest => NatSet.add val (make_verts rest)
end.

Fixpoint make_edges (l: list NatPair) : NatPairSet.t_ := match l with
| nil         => NatPairSet.empty
| val :: rest => NatPairSet.add val (make_edges rest)
end.

Notation "d 'has_vert' v" := (NatSet.mem v d.(V))
	(at level 30, no associativity).
Notation "d 'has_edge' e" := (NatPairSet.mem e d.(E))
	(at level 30, no associativity).

(* testing *)
Definition test := {|
	V := make_verts (0 :: 1 :: 2 :: nil);
	E := make_edges ((0, 1) :: (1, 2) :: (2, 1) :: nil);
|}.

Eval compute in test has_edge (1, 0).
Eval compute in test has_vert 1.
Eval compute in NatPairSet.elements test.(E).

(* ----------------------------------------------------------------------- *)
(* domtree *)
(* a domtree is just a digraph with edges from dominators to dominees. *)

(* a dominates b if there is a path from a to b. *)

Definition P_edge_from (a: nat) (edge: NatPair) : bool := 
	a =? (fst edge).
	
Definition P_edge_to (b: nat) (edge: NatPair) : bool := 
	b =? (snd edge).

Theorem partition_length2:
 forall (A : Type) (f : A -> bool) (l l1 l2 : list A),
  partition f l = (l1, l2) ->
  length l1 <> 0 -> length l2 < length l.
Proof.
	induction l as [ | a rest Hrec]; intros l1 l2.
	- 
		now intros [= <- <- ].
	- 
		simpl.
		destruct (f a), (partition f rest) as (left, right);
		intros [= <- <- ];
		simpl.
		(*steps above from stdlib*)
		
		intros.
		unfold Peano.lt.
		
		
		(*original steps from the stdlib below... then off the rails*)
		rewrite (Hrec left right).
		-- 
			intro.
			unfold Peano.lt.
			apply le_n.
		-- 
			apply @Logic.eq_refl.
		-- 
			induction (length left) as [ | left_len].
			---
				unfold not.
				intro.
				admit.
			---
				apply not_eq_sym.
				apply O_S.
		--
			induction (length left) as [ | left_len].
			unfold not.
			firstorder.
			intro.

		contradiction.
		debug auto. 
		auto.
		induction left as [ | b left' Hrec2].
		-- simpl.
	
Fixpoint dominates_list (a b: nat) (edges: list NatPair) : bool := 
	let (from_a, not_from_a) :=  partition (P_edge_from a) edges in
	if (length from_a) =? 0 then
		false
	else if existsb (P_edge_to b) from_a then
		true
	else
		dominates_list_recur b from_a not_from_a
		
with dominates_list_recur (b: nat) (from to: list NatPair) : bool :=
	(* does any edge in "from" dominate "b" in "to" ? *)
	existsb (fun (edge: NatPair) => 
		dominates_list (snd edge) b to
	) from
.
	
(* match edges with
	| nil => false
	| (src, dst) :: rest => 
		if src =? a then 
			if dst =? b then 
				true 
			else 
				(dominates_list dst b edges)
		else
			(dominates_list a b rest)
end.
 *)
(* Fixpoint dominates_list (a b: nat) (edges: list NatPair) : bool := 
match edges with
	| nil => false
	| (src, dst) :: rest => 
		if PeanoNat.Nat.eqb src a then 
			if PeanoNat.Nat.eqb dst b then 
				true 
			else 
				(dominates_list dst b edges)
		else
			(dominates_list a b rest)
end. *)

Definition dominates (a b: nat) (edges: NatPairSet.t_) : bool :=
	dominates_list a b (NatPairSet.elements edges).

Eval compute in dominates 0 2 test.(E).