# Lambda Calculus

Lambda calculus is a formal system in mathematical logic for expressing computation. It was introduced by Alonzo Church in the 1930s. At its core, it's a universal model of computation that is based on function abstraction and application.

The central concept is the **lambda function**, which is an anonymous function. Instead of defining a function with a name, like `f(x) = x + 1`, you define it just by its behavior.

A lambda expression has three basic parts:

1.  **Variables**: These are the identifiers that act as placeholders, like `x`, `y`, `z`.
2.  **Abstraction**: This is the process of creating a function. In the notation `λx.M`, you are defining an anonymous function that takes an argument `x` and has a body `M`. For example, `λx.x+1` is a function that takes `x` and returns `x+1`.
3.  **Application**: This is the act of applying a function to an argument. `(λx.x+1) 2` would apply the function to the argument `2`, resulting in `3`.

In many programming languages, lambda functions are a feature that allows you to create small, one-off functions without needing to formally define them with a name. This is particularly useful when you need to pass a simple function as an argument to another function, which is a common pattern in functional programming.

### Abstraction: Formal vs. Programming

In **pure lambda calculus**, the `λ` (lambda) symbol is the fundamental and *only* symbol used to denote function abstraction.

In **programming languages**, the syntax for defining anonymous functions varies:
*   **Haskell**: `\x -> x + 1`
*   **Python**: `lambda x: x + 1`
*   **JavaScript**: `x => x + 1`

### Why was lambda calculus invented?

Lambda calculus was invented by Alonzo Church in the 1930s to study the foundations of mathematics. Its primary purpose was to create a formal system to explore two fundamental questions:

1.  What does it mean for a function to be **computable**?
2.  Can we create a universal system that can express **any** possible computation?

This work was used to provide a negative answer to the **Entscheidungsproblem** ("decision problem"), which asked if a universal algorithm existed to determine the provability of any logical statement. Church proved no such algorithm exists by formalizing the very idea of an "algorithm" using lambda calculus.

### How are anonymous functions different from normal ones?

The fundamental difference is the **identifier (a name)**.

1.  **Normal Functions**: Have a name, which can be used to call the function from anywhere in its scope. This also allows for easy recursion.
2.  **Anonymous (Lambda) Functions**: Are defined "in-place" and have no name. They are expressions that evaluate to a function value, perfect for short, "throwaway" operations, especially as arguments to higher-order functions (like `map` or `filter`).

### Building Mathematics from Pure Functions

A profound goal of lambda calculus was to see if mathematics could be built from only functions. Church showed this was possible by representing numbers and arithmetic using function definitions.

**Understanding the Notation: `λ<argument>.<body_of_function>`**

*   The variable after `λ` is the **argument**.
*   The expression after the `.` is the **body**, which is what the function evaluates to (its "return value").
*   `λf.λx. x` is a nested function (shorthand for `λf.(λx. x)`). It's a function that takes an argument `f` and returns a *new function* (`λx. x`). This is how multiple arguments are handled.

**Church Numerals**

The core idea is that a number `N` is represented by a function that takes an operation `f` and a starting value `x`, and applies `f` to `x` exactly `N` times.

*   **Zero (`0`)**: `λf.λx. x` (Applies `f` zero times to `x`, returning `x`.)
*   **One (`1`)**: `λf.λx. f x` (Applies `f` one time to `x`.)
*   **Two (`2`)**: `λf.λx. f (f x)` (Applies `f` two times to `x`.)

**Function Application Notation**

While `f(x)` is used for clarity, the pure, standard notation in lambda calculus is just a **space**: `f x`.
*   `f(g(x))` is written as `f (g x)`. Parentheses are only for grouping.

### Example of a Complex Reduction: ADD 1 1

**β-reduction** is the process of substituting an argument into a function's body. Let's see how `ADD 1 1` reduces to `2`.

**The Components:**
*   `ONE := λf.λx. f x`
*   `ADD := λm.λn.λf.λx. m f (n f x)`

**The Expression:** `( (λm.λn.λf.λx. m f (n f x)) (λf.λx. f x) ) (λf.λx. f x)`

**Step 1: Apply `ADD` to the first `ONE`**
Substitute the first `ONE` for `m` in `ADD`.
*Result:* `(λn.λf.λx. (λf.λx. f x) f (n f x)) (λf.λx. f x)`

**Step 2: Apply the result to the second `ONE`**
Substitute the second `ONE` for `n` in the new function.
*Result:* `λf.λx. (λf.λx. f x) f ( (λf.λx. f x) f x )`
This is the function for the sum, but it needs to be simplified.

**Step 3: Reduce the new function's body**
The body is `(λf.λx. f x) f ( (λf.λx. f x) f x )`.
The innermost part `( (λf.λx. f x) f x )` is just `ONE` applied to `f` and `x`, which reduces to `f x`.
The body becomes `(λf.λx. f x) f (f x)`.

**Step 4: Reduce the body again**
This is `ONE` applied to the arguments `f` and `(f x)`. Substituting these into the body of `ONE` (`f x`) gives `f (f x)`.

**Final Return Value**
The entire expression `ADD 1 1` reduces to:
`λf.λx. f (f x)`
This is, by definition, the Church numeral **Two**.

**References:**

*   **"The Lambda Calculus, Its Syntax and Semantics"** by Henk Barendregt.
*   **"An Introduction to the Lambda Calculus"** by Harold Simmons.
*   **"Structure and Interpretation of Computer Programs (SICP)"** by Abelson, Sussman, and Sussman.
*   **Stanford Encyclopedia of Philosophy: "The Lambda Calculus"**: [https://plato.stanford.edu/entries/lambda-calculus/](https://plato.stanford.edu/entries/lambda-calculus/)

# Functional Programming with Haskell

Functional programming is a programming paradigm where programs are constructed by applying and composing functions. It treats computation as the evaluation of mathematical functions and avoids changing-state and mutable data.

Here are the core concepts:

*   **Pure Functions**: A pure function is a function where the return value is only determined by its input values, without any observable side effects. For the same input, a pure function will always produce the same output. This makes code easier to reason about and test.
*   **Immutability**: In functional programming, you strive to work with immutable data. Instead of modifying existing data structures, you create new ones with the updated values. This helps to prevent bugs that arise from unexpected changes in state.
*   **First-Class Functions**: Functions are treated like any other variable. You can pass them as arguments to other functions, return them from functions, and store them in data structures. Functions that take other functions as arguments are called higher-order functions.
*   **Recursion**: Instead of using loops (like `for` or `while`) to iterate, functional programming often relies on recursion.

Haskell is a purely functional programming language, which means it enforces these concepts. Some of its key features are:

*   **Static, Strong Typing with Type Inference**: Haskell has a strong type system, meaning you can't, for example, add a number and a string. It also has type inference, so you often don't have to explicitly write down the types of your functions; the compiler is smart enough to figure them out.
*   **Laziness by Default**: Expressions in Haskell are not evaluated until their results are actually needed. This can lead to more efficient code and allows for the creation of infinite data structures.
*   **Pattern Matching**: This is a powerful feature that allows you to deconstruct data and define different function behaviors based on the structure of the input.

**References:**

*   **"Learn You a Haskell for Great Good!"** by Miran Lipovača: [http://learnyouahaskell.com/](http://learnyouahaskell.com/)
*   **"Real World Haskell"** by Bryan O'Sullivan, Don Stewart, and John Goerzen: [http://book.realworldhaskell.org/](http://book.realworldhaskell.org/)
*   **The Haskell Official Website**: [https://www.haskell.org/](https://www.haskell.org/)