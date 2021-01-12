(* Since the variable names are now strings, we need to import the required libraries *)
Require Import Strings.String.
Local Open Scope string_scope.
Local Open Scope list_scope.
Scheme Equality for string.

(* ErrorNat encapsulates the constructor error_nat 
   which is useful in the case of arithmetic operations like division by 0*)
Inductive ErrorNat :=
  | error_nat : ErrorNat
  | num : nat -> ErrorNat.

Inductive ErrorBool :=
  | error_bool : ErrorBool
  | boolean : bool -> ErrorBool.

Inductive ErrorString :=
  | error_string : ErrorString
  | stringy : string -> ErrorString.

Coercion num: nat >-> ErrorNat.
Coercion boolean: bool >-> ErrorBool.
Coercion stringy: string >->ErrorString.

(* Section for AExp *)
Inductive AExp :=
  | avar: string -> AExp (* Var ~> string *)
  | anum: ErrorNat -> AExp
  | aplus: AExp -> AExp -> AExp
  | asub: AExp -> AExp -> AExp
  | amul: AExp -> AExp -> AExp (* Multiplication *)
  | adiv: AExp -> AExp -> AExp (* Division *)
  | amod: AExp -> AExp -> AExp. (* Modulo *)

(* Section for BExp *)
Inductive BExp :=
  | berror
  | btrue
  | bfalse
  | bvar: string -> BExp
  | blt : AExp -> AExp -> BExp
  | bnot : BExp -> BExp
  | band : BExp -> BExp -> BExp
  | bor : BExp -> BExp -> BExp.

Inductive SExp :=
  | svar: string -> SExp
  | sconcat: SExp -> SExp -> SExp. (*TODO Maybe add stuff *)

(* Section for Statements *)
Inductive Stmt :=
  | nat_decl: string -> AExp -> Stmt (* Declaration Stmt for variables of type nat *)
  | bool_decl: string -> BExp -> Stmt (* Declaration Stmt for variables of type bool *)
  | string_decl: string -> SExp -> Stmt (* Declaration Stmt for variables of type string *)
  | nat_assign : string -> AExp -> Stmt (* Assignment Stmt for variables of type nat *)
  | bool_assign : string -> BExp -> Stmt (* Assignment Stmt for variables of type bool *)
  | string_assign : string -> SExp -> Stmt  (* Assignment Stmt for variables of type string *)
  | sequence : Stmt -> Stmt -> Stmt
  | while : BExp -> Stmt -> Stmt
  | ifthenelse : BExp -> Stmt -> Stmt -> Stmt
  | ifthen : BExp -> Stmt -> Stmt
  | switch : BExp -> Stmt -> Stmt. (*To add later, too late, not added*)

(* Section for notations *)

Notation "A +' B" := (aplus A B) (at level 60, right associativity).
Notation "A -' B" := (asub A B) (at level 60, right associativity).
Notation "A *' B" := (amul A B) (at level 58, left associativity). 
Notation "A /' B" := (adiv A B) (at level 58, left associativity).
Notation "A %' B" := (amod A B) (at level 58, left associativity).

(* Notations used for boolean operations *)
Notation "A <' B" := (blt A B) (at level 70).
Notation "!' A" := (bnot A)(at level 51, left associativity).
Notation "A &&' B" := (band A B)(at level 52, left associativity).
Notation "A ||' B" := (bor A B)(at level 53, left associativity).

(* Notations used for string operations *)
Notation "A 'concat' B" := (sconcat A B) (at level 50). (*no sintax for this*)

(* Notations for Statements *)
Notation "X :n= A" := (nat_assign X A)(at level 90).
Notation "X :b= A" := (bool_assign X A)(at level 90).
Notation "'iNat' X ::= A" := (nat_decl X A)(at level 90).
Notation "'iBool' X ::= A" := (bool_decl X A)(at level 90).
Notation "'iString' X ::= A" := (string_decl X A)(at level 90).
Notation "S1 ;; S2" := (sequence S1 S2) (at level 93, right associativity).
Notation "'whiles' ( A ) { B }" := (while B A) (at level 97).
Notation "'fors' ( A ~ B ~ C ) { S }" := (A ;; while B ( S ;; C )) (at level 97).
Notation "'if' ( A ) { B }" := (ifthen A B ) (at level 97).
Notation "'if' ( A ) { B } 'else' { C }" := (ifthenelse A B C) (at level 97).
Notation "'do' { A } 'while' ( B ) " := ( A ;; while B A ) (at level 97).

(* Reserved Notations *)
Reserved Notation "A =[ S ]=> N" (at level 60).
Reserved Notation "B ={ S }=> B'" (at level 70).
Reserved Notation "B ={ S }=> B'" (at level 70).
Reserved Notation "S -{ Sigma }-> Sigma'" (at level 60).

(* Coercions for numerical constants and variables *)
Coercion anum: ErrorNat >-> AExp.
Coercion avar: string >-> AExp. (* Var ~> string *)

(* A general type which includes all kind of types *)
Inductive Result :=
  | err_undecl : Result
  | err_assign : Result
  | default : Result
  | res_nat : ErrorNat -> Result
  | res_bool : ErrorBool -> Result
  | res_string : ErrorString -> Result
  | code : Stmt -> Result. (* The functions' names are mapped to the code inside the function *)

Scheme Equality for Result.

(* An environment which maps variable names (strings) to the Result type *)
Definition Env := string -> Result.

(* Initial environment *)
Definition env : Env := fun x => err_undecl.

Definition update (env : Env) (x : string) (v : Result) : Env :=
  fun y =>
    if (eqb y x)
    then v
    else (env y).

(* This function is useful when we need to update the environment based on the state of a variable *)

Definition check_eq_over_types (t1 : Result)(t2 : Result) : bool :=
  match t1 with
  | err_undecl => match t2 with 
                   | err_undecl => true
                   | _ => false
                   end
  | err_assign => match t2 with 
                   | err_assign => true
                   | _ => false
                   end
  | default => false
  | res_nat t1 => match t2 with
                   | res_nat t1 => true
                   | _ => false
                   end
  | res_bool t1 => match t2 with
                   | res_bool t1 => true
                   | _ => false
                   end
  | res_string t1 => match t2 with
                   | res_string t1 => true
                   | _ => false
                   end
  | code t1 => match t2 with
                   | code t1 => true
                   | _ => false
                   end
  end.


(*Errors for aritmethic expressions*)

Definition plus_ErrorNat (n1 n2 : ErrorNat) : ErrorNat :=
  match n1, n2 with
    | error_nat, _ => error_nat
    | _, error_nat => error_nat
    | num v1, num v2 => num (v1 + v2)
    end.

Definition sub_ErrorNat (n1 n2 : ErrorNat) : ErrorNat :=
  match n1, n2 with
    | error_nat, _ => error_nat
    | _, error_nat => error_nat
    | num n1, num n2 => if Nat.ltb n1 n2
                        then error_nat
                        else num (n1 - n2)
    end.

Definition mul_ErrorNat (n1 n2 : ErrorNat) : ErrorNat :=
  match n1, n2 with
    | error_nat, _ => error_nat
    | _, error_nat => error_nat
    | num v1, num v2 => num (v1 * v2)
    end.

Definition div_ErrorNat (n1 n2 : ErrorNat) : ErrorNat :=
  match n1, n2 with
    | error_nat, _ => error_nat
    | _, error_nat => error_nat
    | _, num 0 => error_nat
    | num v1, num v2 => num (Nat.div v1 v2)
    end.

Definition mod_ErrorNat (n1 n2 : ErrorNat) : ErrorNat :=
  match n1, n2 with
    | error_nat, _ => error_nat
    | _, error_nat => error_nat
    | _, num 0 => error_nat
    | num v1, num v2 => num (v1 - v2 * (Nat.div v1 v2))
    end.

Fixpoint aeval_fun (a : AExp) (env : Env) : ErrorNat :=
  match a with
  | avar v => match (env v) with
                | res_nat n => n
                | _ => error_nat
                end
  | anum v => v
  | aplus a1 a2 => (plus_ErrorNat (aeval_fun a1 env) (aeval_fun a2 env))
  | amul a1 a2 => (mul_ErrorNat (aeval_fun a1 env) (aeval_fun a2 env))
  | asub a1 a2 => (sub_ErrorNat (aeval_fun a1 env) (aeval_fun a2 env))
  | adiv a1 a2 => (div_ErrorNat  (aeval_fun a1 env) (aeval_fun a2 env))
  | amod a1 a2 => (mod_ErrorNat (aeval_fun a1 env) (aeval_fun a2 env))
  end.



(* Big-Step semantics for arithmetic operations *)
Inductive aeval : AExp -> Env -> ErrorNat -> Prop :=
| const : forall n sigma, anum n =[ sigma ]=> n
| var : forall v sigma, avar v =[ sigma ]=>  match (sigma v) with
                                              | res_nat x => x
                                              | _ => error_nat
                                              end
| add : forall a1 a2 i1 i2 sigma n,
    a1 =[ sigma ]=> i1 ->
    a2 =[ sigma ]=> i2 ->
    n = (plus_ErrorNat i1 i2) ->
    a1 +' a2 =[sigma]=> n
| times : forall a1 a2 i1 i2 sigma n,
    a1 =[ sigma ]=> i1 ->
    a2 =[ sigma ]=> i2 ->
    n = (mul_ErrorNat i1 i2) ->
    a1 *' a2 =[sigma]=> n
| substract : forall a1 a2 i1 i2 sigma n,
    a1 =[ sigma ]=> i1 ->
    a2 =[ sigma ]=> i2 ->
    n = (sub_ErrorNat i1 i2) ->
    a1 -' a2 =[sigma]=> n
| division : forall a1 a2 i1 i2 sigma n,
    a1 =[ sigma ]=> i1 ->
    a2 =[ sigma ]=> i2 ->
    n = (div_ErrorNat  i1 i2) ->
    a1 /' a2 =[sigma]=> n
| modulo : forall a1 a2 i1 i2 sigma n,
    a1 =[ sigma ]=> i1 ->
    a2 =[ sigma ]=> i2 ->
    n = (mod_ErrorNat i1 i2) ->
    a1 %' a2 =[sigma]=> n
where "a =[ sigma ]=> n" := (aeval a sigma n).

(*Sintax and exceptions for boolean operations*)

Definition lt_ErrorBool (n1 n2 : ErrorNat) : ErrorBool :=
  match n1, n2 with
    | error_nat, _ => error_bool
    | _, error_nat => error_bool
    | num v1, num v2 => boolean (Nat.ltb v1 v2)
    end.

Definition not_ErrorBool (n :ErrorBool) : ErrorBool :=
  match n with
    | error_bool => error_bool
    | boolean v => boolean (negb v)
    end.

Definition and_ErrorBool (n1 n2 : ErrorBool) : ErrorBool :=
  match n1, n2 with
    | error_bool, _ => error_bool
    | _, error_bool => error_bool
    | boolean v1, boolean v2 => boolean (andb v1 v2)
    end.

Definition or_ErrorBool (n1 n2 : ErrorBool) : ErrorBool :=
  match n1, n2 with
    | error_bool, _ => error_bool
    | _, error_bool => error_bool
    | boolean v1, boolean v2 => boolean (orb v1 v2)
    end.

Fixpoint beval_fun (a : BExp) (envnat : Env) : ErrorBool :=
  match a with
  | btrue => true
  | bfalse => false
  | berror => error_bool
  | bvar v => match (env v) with
                | res_bool n => n
                | _ => error_bool
                end
  | blt a1 a2 => (lt_ErrorBool (aeval_fun a1 envnat) (aeval_fun a2 envnat))
  | bnot b1 => (not_ErrorBool (beval_fun b1 envnat))
  | band b1 b2 => (and_ErrorBool (beval_fun b1 envnat) (beval_fun b2 envnat))
  | bor b1 b2 => (or_ErrorBool (beval_fun b1 envnat) (beval_fun b2 envnat))
  end.

Reserved Notation "B ={ S }=> B'" (at level 70).
Inductive beval : BExp -> Env -> ErrorBool -> Prop :=
| b_error: forall sigma, berror  ={ sigma }=> error_bool
| b_true : forall sigma, btrue ={ sigma }=> true
| b_false : forall sigma, bfalse ={ sigma }=> false
| b_var : forall v sigma, bvar v ={ sigma }=>  match (sigma v) with
                                                | res_bool x => x
                                                | _ => error_bool
                                                end
| b_lessthan : forall a1 a2 i1 i2 sigma b,
    a1 =[ sigma ]=> i1 ->
    a2 =[ sigma ]=> i2 ->
    b = (lt_ErrorBool i1 i2) ->
    a1 <' a2 ={ sigma }=> b
| b_not : forall a1 i1 sigma b,
    a1 ={ sigma }=> i1 ->
    b = (not_ErrorBool i1) ->
    !'a1 ={ sigma }=> b
| b_and : forall a1 a2 i1 i2 sigma b,
    a1 ={ sigma }=> i1 ->
    a2 ={ sigma }=> i2 ->
    b = (and_ErrorBool i1 i2) ->
    (a1 &&' a2) ={ sigma }=> b 
| b_or : forall a1 a2 i1 i2 sigma b,
    a1 ={ sigma }=> i1 ->
    a2 ={ sigma }=> i2 ->
    b = (or_ErrorBool i1 i2) ->
    (a1 ||' a2) ={ sigma }=> b 
where "B ={ S }=> B'" := (beval B S B').

Inductive eval : Stmt -> Env -> Env -> Prop :=
| e_nat_decl: forall a i x sigma sigma',
   a =[ sigma ]=> i ->
   sigma' = (update sigma x (res_nat i)) ->
   (x :n= a) -{ sigma }-> sigma'
| e_nat_assign: forall a i x sigma sigma',
    a =[ sigma ]=> i ->
    sigma' = (update sigma x (res_nat i)) ->
    (x :n= a) -{ sigma }-> sigma'
| e_bool_decl: forall a i x sigma sigma',
   a ={ sigma }=> i ->
   sigma' = (update sigma x (res_bool i)) ->
   (x :b= a) -{ sigma }-> sigma'
| e_bool_assign: forall a i x sigma sigma',
    a ={ sigma }=> i ->
    sigma' = (update sigma x (res_bool i)) ->
    (x :b= a) -{ sigma }-> sigma'
| e_seq : forall s1 s2 sigma sigma1 sigma2,
    s1 -{ sigma }-> sigma1 ->
    s2 -{ sigma1 }-> sigma2 ->
    (s1 ;; s2) -{ sigma }-> sigma2
| e_if_then : forall b s sigma,
    ifthen b s -{ sigma }-> sigma
| e_if_then_elsetrue : forall b s1 s2 sigma sigma',
    b ={ sigma }=> true ->
    s1 -{ sigma }-> sigma' ->
    ifthenelse b s1 s2 -{ sigma }-> sigma' 
| e_if_then_elsefalse : forall b s1 s2 sigma sigma',
    b ={ sigma }=> false ->
    s2 -{ sigma }-> sigma' ->
    ifthenelse b s1 s2 -{ sigma }-> sigma' 
| e_whilefalse : forall b s sigma,
    b ={ sigma }=> false ->
    while b s -{ sigma }-> sigma
| e_whiletrue : forall b s sigma sigma',
    b ={ sigma }=> true ->
    (s ;; while b s) -{ sigma }-> sigma' ->
    while b s -{ sigma }-> sigma'
where "s -{ sigma }-> sigma'" := (eval s sigma sigma').

Fixpoint eval_fun (s : Stmt) (env : Env) (gas: nat) : Env :=
    match gas with
    | 0 => env
    | S gas' => match s with
                | sequence S1 S2 => eval_fun S2 (eval_fun S1 env gas') gas'
                | nat_decl a aexp => update (update env a default) a (res_nat (aeval_fun aexp env))
                | bool_decl b bexp => update (update env b default) b (res_bool (beval_fun bexp env))
                | nat_assign a aexp => update env a (res_nat (aeval_fun aexp env))
                | bool_assign b bexp => update env b (res_bool (beval_fun bexp env))
                | ifthen cond s' => 
                    match (beval_fun cond env) with
                    | error_bool => env
                    | boolean v => match v with
                                 | true => eval_fun s' env gas'
                                 | false => env
                                 end
                    end
                | ifthenelse cond S1 S2 => 
                    match (beval_fun cond env) with
                        | error_bool => env
                        | boolean v  => match v with
                                 | true => eval_fun S1 env gas'
                                 | false => eval_fun S2 env gas'
                                 end
                         end
                | while cond s' => 
                    match (beval_fun cond env) with
                        | error_bool => env
                        | boolean v => match v with
                                     | true => eval_fun (s' ;; (while cond s')) env gas'
                                     | false => env
                                     end
                end
    end.

(*Some examples or something*)
Definition while_stmt :=
    iNat "i" ::= 0 ;;
    iNat "sum" ::= 0 ;;
    while 
        ("i" <' 6) 
        (
           "sum" :n= "sum" +' "i" ;;
           "i" :n= "i" +' 1
        ).

Compute (eval_fun while_stmt env 100) "sum".

Definition for_stmt :=
    iNat "sum" ::= 0 ;;
    fors ( iNat "i" ::= 0 ~ "i" <' 6 ~ "i" :n= "i" +' 1 ) {
      "sum" :n= "sum" +' "i"
    }.

Compute (eval_fun for_stmt env 100) "sum".
