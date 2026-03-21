
# Rocq notes (from tutorial at https://coq.vercel.app/ext/mdnahas/nahas_tutorial.html)

**IT IS VITALLY IMPORTANT THAT YOU DO NOT THINK OF A Prop AS BEING EITHER "TRUE" OR "FALSE".** Prop can be either **proven or unproven,** rather than true or false. 

- `True` and `False` are poorly-named
	- capital-T `True` means `Provable` or `AlwaysProvable`
	- capital-F `False` means `Unprovable` or `NeverProvable`
- lowercase-t `true` and lowercase-f `false` behave how you're used to

## Vernacular commands

- `Inductive <name> : Type := | val1 : Type1 | val2: Type2 ...` lets you create a new type.
	- `Inductive` constructors are functions with no definitions. they are **opaque constants**
- `Definition <thing> := <otherthing>` says two things are interchangeable.
- `Notation "<symbol> <var>" := expr: Type` creates an operator.
- `Require Import <module>` is an old way to import things
- `From <library> Require <module>` is how you import things, `Stdlib` is the standard library
	- *but I'm guessing you have to either glob things or refer to them as Module.Item because changing `Require Import` to this caused all kinds of name errors*
- `Fixpoint` declares a recursive function.
- `Check <name>` outputs the type of `<name>`.
- `Print whatever` outputs the definition of something
- `Compute whatever` executes all function calls and prints the results.

## Tactics

- `intros <name>` takes a `forall` or `->`, strips off the beginning, and assigns that beginning to `<name>`, leaving you with a subgoal which is the thing after the beginning.
	- so if you have `forall A : Prop, A -> B`, `intros A` will give you `A` (of type... `A`... it's a little confusing) and leave you with `A -> B`.
	- you can also put multiple names to strip multiple things in a row.
	- **WARNING: Don't use "intros" with no arguments - it doesn't do what you'd expect!**
- `exact <name>` says "`<name>` is of the exact type of the subgoal, so we're done."
- `refine (<hyp_name> _ _)` does a *backwards proof* by taking a `<hyp_name>` which results in the current, subgoal type, then introduces `N` subgoals (where `N` is the number of `_`) of the types of the arguments of `<hyp_name>`.
- `pose (new_name := thing arg1 arg2 .. argn)` takes a function, calls it with the arguments, and gives `new_name` of the result type.
	- you can also use it to just... declare a hypothesis. `pose (hyp := value).`
- `assert (name : type)` is... kinda like `pose`? 
	- you can use it in cases where you want to do `pose (name := _: type)` which is invalid...
- `simpl` executes a function call and gives you the return value.
	- `simpl in <hyp>` does the same but on an existing hypothesis.
- `unfold <hyp>` turns a `Definition`'s first thing into the second thing.
	- `unfold <hyp1> in <hyp2>` does the same thing but to an already-existing `<hyp2>`.
- `fold <hyp>` turns a `Definition`'s second thing into the first thing
- `case <hyp>` creates subgoals for every possible construction of its argument.
	- **but be careful:** it only works on subgoals, *not* on hypotheses, so don't move anything into hypotheses with `intros` that you want to use `case` on!
- `destruct <hyp> as [ <arg1> <arg2> ... ]` is like `case` but useful for types with a single ctor.
	- tho it works for multiple ctors too: `destruct <hyp> as [<ctor1 args> | <ctor2 args>]`
- `elim <name>` is like `case` but for recursive types. it creates subgoals for the base case and the recursive case.
- `induction <name> as [<base case vars> | <inductive case vars>]` is to `elim` as `destruct` is to `case` - creates subgoals for the base and recursive cases, but lets you declare the variables all at once instead of requiring use of `intros` later.
- `admit` is a copout. it says "there's a proof for this honest"
- `rewrite <name>`, if you have `<name> = <other>`, replaces instances of `<name>` with `<other>`.
	- `rewrite <- <other>` does the same, but replaces `<other>` with `<name>`.
- `discriminate <name>` can be used when you have `<name>: (ctor1 ...) = (ctor2 ...)` to say "this is impossible." it will eliminate that hypothesis.

## Rules

- for subgoals
	- If the subgoal matches a hypothesis,
		- Then **use tactic `exact <hyp_name>.`.**
	- If your subgoal is `True`
		- Then **use tactic `exact I.`.**
	- If your subgoal is `False`,
		- trying `refine` might be the way to go, to go backwards from `False` to something that leads to it.
	- If the current subgoal contains a function call with all its arguments
		- Then **use tactic `simpl.`.**
	- If the subgoal is of the form `x = x`,
		- Then **use tactic `reflexivity`.**
	- If the subgoal starts with
		- any of
			- `(forall <name> : <type>, ...`
			- `<type> -> ...`
			- `~<type>`
			- `~(<term>)`
			- `(not <term>)`
		- Then **use tactic `intros <name>.`.**
	- If your subgoal is
		- any of:
			- `<hyp_name>: <type1> -> <type2> -> ... -> <result_type>`
			- `<hyp_name>: (forall <obj1>:<type1>, (forall <obj2>:<type2>, ... <result_type> ...))`
			- any combination of `->` and `forall`
		- *and* you have hypotheses of type `type1`, `type2`...
		- Then **use tactic `pose (new_name := thing arg1 arg2 .. argn)` to create `new_name` of type `result_type`.**
	- If you have subgoal `<goal_type>` AND hypothesis `<hyp_name>: <type1> -> <type2> -> ... -> <typeN> -> <goal_type>`,
		- Then **use tactic `refine (<hyp_name> _ ...).` with N underscores.**
	- If the subgoal's top-most term is a created type,
		- Then use `refine (<name_of_constructor> _ _ ...).`.
	- If you have a subgoal that you want to ignore for a while
		- Then **use tactic `admit.`.**
- for hypotheses
	- If a hypothesis `<name>` contain a function call with all its arguments
		- Then **use tactic `simpl in <name>.`.**
	- If any hypothesis is `<name> : False`
		- Then **use tactic `case <name>.`** this will produce 0 cases and eliminate the subgoals. 
	- If a hypothesis `<name>` is a created type with only one constructor
		- Then **use `destruct <name> as <arg1> <arg2> ... ` to extract its arguments.**
	- If you have a hypothesis 
		- `<name> : (<constructor1> ...) = (<constructor2> ...)` OR 
		- `<name> : <constant1> = <constant2>` 
		- Then **use tactic `discriminate <name>.`.**
- for subgoals and hypotheses
	- If there is a hypothesis `<name>` of a created type *AND* that hypothesis is used in the subgoal,
		- Then **try tactic `case <name>.`.**
	- If you have a hypothesis `<name> : <a> = <b>` AND `<a>` in your current subgoal
		- Then **use tactic `rewrite <name>.`.**
	- If you have a hypothesis `<name> : <a> = <b>` AND `<b>` in your current subgoal
		- Then **use tactic `rewrite <- <name>.`.**
	- If there is a hypothesis `<name>` of a created type AND `<name>` is used in the subgoal AND the type has a recursive definition, 
		- Then **try tactic `elim <name>.`.**

## Existential

- `Theorem thm : (exists a, <predicate involving a>)`
- `(ex_intro <predicate> <witness> <proof of predicated called on witness>)`
	
- can use it like `refine (ex_intro predicate witness _)` to generate a subgoal where you prove that the predicate can be called on the witness
- can use it like `refine (ex_intro _ witness _)` to automatically determine the predicate