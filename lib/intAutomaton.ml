open Expr
open Transition
open Automaton

(* Simple implementation: represent states as integers.
   Initial state is always 0. When combining two states, support a "shifting" operation
   Might refactor to make the initial state special. *)
   module IntAutomaton (T : TEST) (O : Map.OrderedType with type t = T.atom) :
   AUTOMATON with type test = T.t = struct
   module AtomMap = Map.Make (O)
   module IntMap = Map.Make (Int)
 
   type atom = T.atom
   type test = T.t
   type s = int
 
   type 'action t = {
     size : int;
     delta : ('action, s) transition AtomMap.t IntMap.t;
   }
 
   let get delta st at = AtomMap.find at (IntMap.find st delta)
   let initState _ = 0
   let trans { delta; _ } = get delta
 
   let getNewTrans f =
     let atoms = T.getAtoms in
     let bindings = Seq.zip atoms (Seq.map f atoms) in
     AtomMap.add_seq bindings AtomMap.empty
 
   let updateTrans st f = IntMap.add st (getNewTrans f)
 
   (** Shift transition *)
   let shift_transition d = function
     | Next (p, s) -> Next (p, s + d)
     | Accept -> Accept
     | Reject -> Reject
 
   (** Shift all indices up by d, except for 0 *)
   let shift delta d =
     let remap f mp =
       IntMap.fold
         (fun k v cur ->
           let k', v' = f k v in
           IntMap.add k' v' cur)
         mp IntMap.empty
     in
     let fn = AtomMap.map (shift_transition d) in
     remap (fun k v -> (k + d, fn v)) (IntMap.remove 0 delta)
 
   let uniform_continuation ub h delta =
     let rec loop i aut =
       if i == 0 then aut
       else
         loop (i - 1)
           (updateTrans i
              (fun at ->
                match get aut i at with Accept -> h at | _ -> get aut i at)
              aut)
     in
     loop ub delta
 
   let merge delta_f delta_g d =
     IntMap.union (fun _ x _ -> Some x) delta_f (shift delta_g d)
 
   let rec from_expr = function
     | Do p ->
         {
           size = 1;
           delta =
             updateTrans 1
               (fun _ -> Accept)
               (updateTrans 0 (fun _ -> Next (p, 1)) IntMap.empty);
         }
     | Assert b ->
         {
           size = 0;
           delta =
             updateTrans 0
               (fun at -> if T.entails at b then Accept else Reject)
               IntMap.empty;
         }
     | Seq (f, g) ->
         let { size = size_f; delta = delta_f } = from_expr f in
         let { size = size_g; delta = delta_g } = from_expr g in
         {
           size = size_f + size_g;
           delta =
             updateTrans 0
               (fun at ->
                 match get delta_f 0 at with
                 | Accept -> shift_transition size_f (get delta_g 0 at)
                 | _ -> get delta_f 0 at)
               (uniform_continuation size_f
                  (fun at -> shift_transition size_f (get delta_g 0 at))
                  (merge delta_f delta_g size_f));
         }
     | If (b, f, g) ->
         let { size = size_f; delta = delta_f } = from_expr f in
         let { size = size_g; delta = delta_g } = from_expr g in
         {
           size = size_f + size_g;
           delta =
             updateTrans 0
               (fun at ->
                 if T.entails at b then get delta_f 0 at
                 else shift_transition size_f (get delta_g 0 at))
               (merge delta_f delta_g size_f);
         }
     | While (b, f) ->
         let { size; delta = delta_f } = from_expr f in
         let delta_e =
           updateTrans 0
             (fun at ->
               if T.entails at (BNot b) then Accept
               else
                 match get delta_f 0 at with
                 | Accept -> Reject
                 | _ -> get delta_f 0 at)
             delta_f
         in
         { size; delta = uniform_continuation size (get delta_e 0) delta_e }
 
   (* NOTE: this does NOT actually normalize the automaton.
      Maybe this can be a default one anyways. *)
   let reify { delta; _ } : 'action Bisimulation.normalized_automaton =
     let l =
       Seq.map
         (fun (_, mp) ->
           List.map (fun (_, y) -> y) (List.of_seq (AtomMap.to_seq mp)))
         (IntMap.to_seq delta)
     in
     Array.of_list (List.of_seq l)
 
   (*
   let show_atom_map f (mp : ('action, s) transition AtomMap.t) =
     show_seq
       (fun (at, x) ->
         "(" ^ T.show_atom at ^ ", " ^ show_transition f string_of_int x ^ ")")
       "" (AtomMap.to_seq mp)
 
   let show_trans_map f mp =
     show_seq
       (fun (s, mp') -> "(" ^ string_of_int s ^ ", " ^ show_atom_map f mp' ^ ")")
       "\n" (IntMap.to_seq mp)
 
   open Printf
 
   let print f { size; delta } =
     sprintf "size = %d, delta = %s" size (show_trans_map f delta)
     *)
 end
 