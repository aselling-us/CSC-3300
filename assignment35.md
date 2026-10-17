# Assignment 4: DMRG+ Interpreter with Booleans and Lists

## Overview

In Assignment 3 you built a parser and interpreter for DMRG3, a language with only real numbers, arithmetic, `ifleq0?`, and first-order function calls. In this assignment you will extend the language with two new data types, **booleans** and **lists**, and build a **stepper** that shows how a program reduces, one substitution at a time, to a final answer.

Write everything in `#lang typed/racket`. Use `typed/rackunit` for tests. Every function you write must have a full type signature, and you may not use `cast` except where noted in Part 2.

Do not reuse the `ExprC` design from Assignment 3 unchanged. Your new structs and type definitions must be different from the ones in Assignment 3 (see Part 1).

---

## The Language: DMRG+

### New Concrete Syntax

| Form | Meaning |
|---|---|
| `true`, `false` | Boolean literals |
| `{<= a b}` | `#t` if `a <= b`, else `#f` |
| `{== a b}` | `#t` if `a` and `b` are equal numbers or equal booleans, else `#f` |
| `{and a b}` | Boolean and (both arguments are evaluated) |
| `{not a}` | Boolean negation |
| `{if c t f}` | If `c` is `#t`, evaluate `t`; otherwise evaluate `f`. `c` must be a boolean. |
| `{cons h t}` | Prepend `h` onto list `t` |
| `{first l}` | First element of a non-empty list |
| `{rest l}` | All elements except the first, of a non-empty list |
| `{empty? l}` | `#t` if `l` is the empty list |
| `empty` | The empty list |
| `{list e ...}` | Shorthand for nested `cons` ending in `empty` |

Functions keep the Assignment 3 syntax: `{name param ... = body}`.

A program is a list of function definitions. Execution starts with `main`, which takes no parameters.

### Values

A **value** is one of:
- a real number,
- a boolean,
- a list of values.

Lists may contain values of mixed type. For example, `{list 1 true {list 2}}` is valid.

### Errors

Your interpreter must signal an error (using `error`) in these cases. Each message must begin with the name of the function that detected the error, followed by `: DMRG+`. For example: `interp: DMRG+ first of empty list`.

- `first` or `rest` applied to `empty`
- `if` or `not` or `and` applied to a non-boolean
- `<=` applied to a non-number
- an unknown symbol or function name
- a function called with the wrong number of arguments
- `main` is missing

---

## Part 1: Data Definitions (20 points)

Define Typed Racket types and structs for the abstract syntax of DMRG+. You must:

1. Define a struct for each new expression form. Choose sensible names that do not collide with Assignment 3 (for example, `LetBoolC` is not allowed as a name for a boolean literal in this assignment; use something like `BoolLitC`). Your design must have at least one struct that holds a **list** of sub-expressions.
2. Define `ExprC` as a union of all expression types, including `Real`, `Boolean`, and `Symbol`.
3. Define `Value` as a union of `Real`, `Boolean`, and a recursive list type. Typed Racket requires a recursive type alias here, so write it as `(define-type Value (U Real Boolean (Listof Value)))` and check that it type-checks.
4. Define `FundefC` and `ProgC` for function definitions and programs.

Include a short comment block (5 to 10 lines) above your type definitions that explains why you chose each struct and how your `ExprC` union is organized.

---

## Part 2: Parser (25 points)

Write:

```racket
(define (parse [s : Sexp]) : ExprC ...)
(define (parse-fundef [s : Sexp]) : FundefC ...)
(define (parse-prog [s : Sexp]) : (Listof FundefC) ...)
```

Requirements:

- `parse` must handle every form in the table above, including the `{list e ...}` shorthand. The shorthand must expand into nested cons structs, not into a new struct.
- `parse` must reject `{if ...}` forms with the wrong number of arguments and must reject a function call whose name is `if`, `and`, `not`, `cons`, `first`, `rest`, `empty?`, `list`, `<=`, or `==`. Each rejection uses `error` with a message beginning `parse: DMRG+`.
- `parse-prog` must reject duplicate function names, as in Assignment 3.
- You may use `cast` in `parse-fundef` only, and only for the parameter list. Explain in a comment why the cast is safe.

Write at least **10 `check-equal?` tests** for the parser and **3 `check-exn` tests** for the error cases.

---

## Part 3: Interpreter (30 points)

Write:

```racket
(define (interp [exp : ExprC] [funs : (Listof FundefC)]) : Value ...)
(define (top-interp [s : Sexp]) : Value ...)
```

Requirements:

- `interp` evaluates an expression to a `Value`.
- `if` evaluates only the branch that is taken. Your tests must include a case where the untaken branch would raise an error if it were evaluated, and check that no error occurs.
- `and` evaluates both arguments. Your tests must show this behavior.
- Function calls use **call by value**: evaluate every argument before substitution.
- `first`, `rest`, and `empty?` must reject any argument that is not a list, with an error.

Write at least **12 `check-equal?` tests** covering:

- each boolean operator,
- each list operator,
- a recursive function over a list (for example, a function that sums a list of numbers),
- a function returning a list,
- a case that exercises the error message for `first` of `empty`.

---

## Part 4: Stepper (35 points)

This is the main new feature. Write:

```racket
(define (step-trace [s : Sexp]) : (Listof Sexp) ...)
```

`step-trace` takes a DMRG+ program and returns a list of Racket s-expressions. The first element is the program's `main` body with arguments substituted, written in Racket syntax. Each later element is the result of **one** reduction step. The last element is the final value.

Rules:

1. Each step reduces exactly one subexpression. Reduce the leftmost-innermost redex first. A redex is an operator applied to values, or an `if` whose condition is already a boolean, or a function call whose arguments are all values.
2. Output each intermediate expression as a plain Racket s-expression. For example, `{+ 3 {* 2 4}}` produces `'(+ 3 (* 2 4))`, then `'(+ 3 8)`, then `11`.
3. Lists print as Racket lists. `{list 1 2}` prints as `'(cons 1 (cons 2 empty))`, and the value `(list 1 2)` prints as `'(1 2)`. Booleans print as `#t` and `#f`.
4. A function call is reduced in one step by substituting the argument values into the body. Show the substituted body as the next element of the trace.

### Example

Given:

```
{{add7 a = {+ a 7}}
 {main = {add7 {+ 1 2}}}}
```

the trace is:

```racket
'((add7 (+ 1 2))        ; step 0: main's body, unevaluated
  (add7 3)              ; reduce the argument
  (+ 3 7)               ; apply add7 by substitution
  10)                   ; reduce +
```

Note that step 0 shows the function name as a Racket call, not as a DMRG+ call. Use this convention for all function calls in the trace.

### Required Test Cases

Write at least **8 `check-equal?` tests** for `step-trace`. At least three must involve:

- a list being built or traversed,
- an `if` whose untaken branch is never shown in the trace,
- a function that calls itself recursively, with a trace of at least four steps.

Include in your submission a file `trace-examples.txt` containing the output of `step-trace` for the three programs you choose. Pick programs that show something interesting, and explain in a comment why each one is instructive.

---

## Part 5: Written Reflection (10 points)

Answer each question in 100 to 200 words. Put your answers in a comment block at the end of your file.

1. Your `interp` and your `step-trace` must agree on final values. Describe one test you wrote that would have caught a disagreement between them, and explain what that disagreement would look like.
2. Call-by-value and call-by-name differ on programs where an argument raises an error but the function never uses it. Write a DMRG+ program that demonstrates this, and say what your implementation does with it.
3. Your `Value` type includes `(Listof Value)`. Explain why `Listof` is enough to describe a list of mixed values, and identify one operation that would be harder to type if lists had to be homogeneous.

---

## Submission

Submit one file, `assign4.rkt`, containing:

- Parts 1 through 4 with all tests,
- the Part 5 reflection as a comment block at the end.

Submit a second file, `trace-examples.txt`, with the output of `step-trace` for your three chosen programs.

Your file must run with `racket assign4.rkt` and produce no test failures. Tests that depend on error messages must match the messages in the spec exactly.

---

## Grading

| Part | Points |
|---|---|
| 1. Data definitions | 20 |
| 2. Parser | 25 |
| 3. Interpreter | 30 |
| 4. Stepper | 35 |
| 5. Written reflection | 10 |
| **Total** | **120** |

Your score is capped at 100. Extra credit is available for a stepper that also shows the environment for each step, but only if the basic stepper is complete and passing.

## Academic Integrity

Work individually. You may discuss concepts with classmates, but you may not share code. Cite any online resources you use in a comment at the top of your file.
