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

### Basic Haskell: "Hello World" Example

Let's start with the classic "Hello, World!" program in Haskell:

```haskell
-- This is a single-line comment in Haskell

-- The 'main' function is the entry point of every Haskell program.
-- Its type signature 'IO ()' indicates that it performs an I/O action
-- and returns no meaningful value (represented by '()', the unit type).
main :: IO ()
main = putStrLn "Hello, World!"
```

**Explanation:**

1.  **`-- This is a single-line comment in Haskell`**:
    *   Haskell uses `--` for single-line comments. Multi-line comments are enclosed in `{-` and `-}`.

2.  **`main :: IO ()`**:
    *   This is the **type signature** for the `main` function.
    *   `main` is the special function that serves as the entry point for any executable Haskell program.
    *   `::` can be read as "has the type".
    *   `IO ()` is a type that signifies an **I/O action**. In Haskell, functions are typically pure (no side effects). Interacting with the outside world (like printing to the console) is handled by wrapping these actions in the `IO` type.
    *   `()` (pronounced "unit") is a type that has only one value, also written `()`. It's used when a function doesn't need to return any specific data. So, `IO ()` means "an I/O action that produces no meaningful result."

3.  **`main = putStrLn "Hello, World!"`**:
    *   This is the **definition** of the `main` function.
    *   `=` assigns the result of the expression on the right to the name on the left.
    *   `putStrLn` is a standard library function that takes a `String` and prints it to the console, followed by a newline character.
    *   `"Hello, World!"` is a string literal.

**Haskell Conventions:**

*   **Type Signatures**: It's a strong convention to include type signatures for top-level functions.
*   **Function Definition**: Functions are defined by stating their name, followed by their arguments, and then an `=` sign, followed by the expression that defines their behavior.
*   **No Parentheses for Function Application**: Function application is denoted by simply placing the function name before its arguments, separated by spaces (e.g., `putStrLn "Hello"`).

### Drone Simulation Example

This code simulates the movement of a drone in a 3D space based on a series of random movements.

```haskell
import Control.Monad (replicateM)
import Data.Foldable (foldl')
import qualified System.Random.Stateful as Rand

data Drone = Drone
  { xPos :: Int
  , yPos :: Int
  , zPos :: Int
  } deriving Show

data Movement
  = Forward | Back | ToLeft | ToRight | Up | Down
  deriving (Show, Enum, Bounded)

main :: IO ()
main = do
  let initDrone = Drone { xPos = 0, yPos = 100, zPos = 0 }
  -- Generate 15 moves randomly
  randomMoves <- replicateM 15 $ Rand.uniformEnumM Rand.globalStdGen
  let resultDrone = foldl' moveDrone initDrone randomMoves
  print resultDrone

moveDrone :: Drone -> Movement -> Drone
moveDrone drone move =
  case move of
    Forward -> drone { zPos = zPos drone + 1 }
    Back    -> drone { zPos = zPos drone - 1 }
    ToLeft  -> drone { xPos = xPos drone - 1 }
    ToRight -> drone { xPos = xPos drone + 1 }
    Up      -> drone { yPos = yPos drone + 1 }
    Down    -> drone { yPos = yPos drone - 1 }
```

**Explanation:**

1.  **`import ...`**: These lines import necessary modules for monadic operations (`Control.Monad`), strict folding (`Data.Foldable`), and random number generation (`System.Random.Stateful`).
2.  **`data Drone = Drone { ... } deriving Show`**:
    *   Defines a new data type `Drone` using **record syntax**. It has three fields: `xPos`, `yPos`, and `zPos`, all of type `Int`.
    *   `deriving Show` automatically generates code to allow `Drone` values to be converted to a string for printing.
3.  **`data Movement = Forward | Back | ... deriving (Show, Enum, Bounded)`**:
    *   Defines an algebraic data type `Movement` with six **constructors** representing different directions.
    *   `deriving (Show, Enum, Bounded)`:
        *   `Show`: Allows `Movement` values to be printed.
        *   `Enum`: Allows `Movement` values to be treated as elements of an enumerated type (e.g., for generating random values).
        *   `Bounded`: Indicates that there's a minimum (`minBound`) and maximum (`maxBound`) value for `Movement`.
4.  **`main :: IO ()`**: The program's entry point.
    *   The `do` notation is used to sequence `IO` actions, making them look more imperative while maintaining Haskell's functional purity.
5.  **`let initDrone = Drone { xPos = 0, yPos = 100, zPos = 0 }`**:
    *   Defines an initial `Drone` state using record construction syntax. `let` introduces local, immutable bindings.
6.  **`randomMoves <- replicateM 15 $ Rand.uniformEnumM Rand.globalStdGen`**:
    *   This line generates a list of 15 random `Movement` values.
    *   `Rand.uniformEnumM Rand.globalStdGen`: A function from `System.Random.Stateful` that generates a random value from an `Enum` type.
    *   `replicateM 15`: A monadic function from `Control.Monad` that repeats an `IO` action (generating a random move) 15 times and collects the results into a list within the `IO` context.
    *   `<-`: The **bind operator** in `do` notation. It "unwraps" the `IO` action on the right (which is `IO [Movement]`) and assigns its *result* (the list of random moves) to the `randomMoves` variable.
7.  **`let resultDrone = foldl' moveDrone initDrone randomMoves`**:
    *   This line simulates the drone's movement by applying a sequence of movements.
    *   `foldl'` (from `Data.Foldable`) is a **strict left fold**. It's a higher-order function that takes:
        *   A function (`moveDrone`) to apply repeatedly.
        *   An initial accumulator value (`initDrone`).
        *   A list (`randomMoves`).
    *   It applies `moveDrone` to the accumulator and each element of the list, updating the accumulator with each step.
8.  **`print resultDrone`**: Prints the final state of the drone to the console.
9.  **`moveDrone :: Drone -> Movement -> Drone`**:
    *   This function takes a `Drone` and a `Movement` and returns a *new* `Drone` with updated coordinates.
    *   **`case move of ...`**: This uses **pattern matching** to inspect the `Movement` value and execute different code branches based on it.
    *   **`drone { zPos = zPos drone + 1 }`**: This is **record update syntax**. It creates a *new* `Drone` record that is identical to the original `drone`, except the specified field (`zPos`) is updated. This is a key demonstration of **immutability** in Haskell; the original `drone` value is never changed.

### Type Inference and Type Classes in Action

In the drone example, the line `randomMoves <- replicateM 15 $ Rand.uniformEnumM Rand.globalStdGen` is particularly interesting. The `Movement` type is not explicitly passed as a parameter. So how does Haskell know to generate `Movement` values?

This happens through **type inference** and **type classes**.

1.  **The Compiler Infers the Required Type**: The compiler sees how `randomMoves` is used *later* in the code: `foldl' moveDrone initDrone randomMoves`. It knows the type of `moveDrone` is `Drone -> Movement -> Drone`. From this context, it deduces that `randomMoves` **must be a list of `Movement` values**, i.e., its type is `[Movement]`.

2.  **The Compiler Checks the Polymorphic Function**: Now it looks at how `randomMoves` is created. The function `Rand.uniformEnumM` is **polymorphic**. Its simplified type is `(Enum a, Bounded a) => g -> IO a`.
    *   This means it can produce a value of *any* type `a`.
    *   However, the `(Enum a, Bounded a) =>` part is a **constraint**. It means that the chosen type `a` *must* belong to the `Enum` and `Bounded` type classes.

3.  **The Compiler Connects the Dots**:
    *   The compiler needs to produce a `[Movement]`.
    *   It sees that `Rand.uniformEnumM` can produce an `IO a`.
    *   It asks: "Can I use `Movement` for the type variable `a`?"
    *   It checks your definition: `data Movement = ... deriving (Show, Enum, Bounded)`. Because you derived `Enum` and `Bounded`, the answer is **yes**.
    *   Therefore, the compiler automatically **specializes** the polymorphic `uniformEnumM` function to use `Movement` as the type `a` in this specific context.

In short, the type isn't passed as a value. It's **inferred** from the context, and the compiler uses **type classes** to verify that the requested type is valid for that function.

**References:**

*   **"Learn You a Haskell for Great Good!"** by Miran Lipovača: [http://learnyouahaskell.com/](http://learnyouahaskell.com/)
*   **"Real World Haskell"** by Bryan O'Sullivan, Don Stewart, and John Goerzen: [http://book.realworldhaskell.org/](http://book.realworldhaskell.org/)
*   **The Haskell Official Website**: [https://www.haskell.org/](https://www.haskell.org/)
