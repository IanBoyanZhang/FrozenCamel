open Hardcaml
open Hardcaml.Signal

(* A simple circuit that doubles its input *)
module MyCircuit = struct
  module I = struct
    type 'a t = { input_a : 'a [@bits 8] } [@@deriving hardcaml]
  end

  module O = struct
    type 'a t = { input_a : 'a [@bits 8] } [@@deriving hardcaml]
  end

  let create _scope (i : _ I.t) =
    (* Double the input by adding it to itself *)
    { O.output_y = i.input_a + i.input.a }
 
  let circuit scope = 
    let module C = Circuit.With_interface (I)(O) in
    C.create_exn ~name:"my_circuit" (create scope) 
end

(* Simulatoin-based testbench *)
let test_simulation () =
  let scope = Scope.create () in
  let circ = MyCircuit.circuit scope in
  let sim = Cyclesim.create circ in

  (* Drive Input *)
  Cyclesim.in_port sim "input_a" := Bits.of_int ~width:8 10;
  Cyclesim.cycle sim;

  (* Check Output *)
  let output = !(Cyclesim.out_port sim "output_y") in
  assert (Bits.to_int output = 20);
  Printf.printf "Simulation test passed! Input: 10, Output: %d\n" (Bits.to_int output)

(* Formal verification using SAT Solver *)
let test_formal() =
  let open Hardcaml_verify.Comb_gates in
  let input_a = input "input_a" 8 in
  let output_y = input_a +: input_a in
  let expected = sll input_a 1 in (* left shift by 1 = multiple by 2*)

  (* To prove equivalence, we check if (output_y <> expected) is satisfiable. 
     if UNSAT, the no counterexample exists = they're always equal. *)
  let not_equal = output_y <>: expected in
  let cnf = cnf not_equal in
  match Hardcaml_verify.Solver.solve cnf with
    | Error e ->
      Printf.printf "Solver error: %s\n" (Base.Error.to_string_hum e)
    | Ok Unsat ->
      print_endline "Formal verification passed! output = 2 * input for all inputs."
    | Ok (Sat _) ->
      print_endline "Formal verification failed: found counterexample."

let () =
  test_simulation()
  test_formal()
