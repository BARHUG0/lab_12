This document provides a consolidated explanation of the concepts required to solve Problem 2.

### 1. Problem Description

The task is to write a Haskell program that calculates the n-th power of each element in a list of integers. The program should take the power `n` and the list of integers as input and should use a lambda function as part of the solution.

**Example:**
*   **Input `n`**: `3`
*   **Input list**: `[1, 2, 3, 4, 5, 6, 7, 8, 9, 10]`
*   **Output list**: `[1, 8, 27, 64, 125, 216, 343, 512, 729, 1000]`

### 2. Function Signature

A good starting point is to define the type signature for the main function that will perform this calculation. It needs to take an integer (the power `n`) and a list of integers, and it will return a new list of integers.

```haskell
calculatePowers :: Int -> [Int] -> [Int]
```

### 3. List Transformation: The `map` Function

In functional programming, when you want to apply a function to every element of a list to produce a new list of the results, you use the higher-order function `map`.

The type signature for `map` is:
`map :: (a -> b) -> [a] -> [b]`

*   It takes a function `(a -> b)` and a list `[a]`.
*   It returns a new list `[b]` where each element is the result of applying the function to the corresponding element of the input list.

For this problem, `map` is the perfect tool to apply the "n-th power" calculation to each integer in your input list.

### 4. The Core Logic: Power Calculation and Lambdas

To calculate the n-th power of a number, you can use the `^` operator in Haskell. For example, `2 ^ 3` evaluates to `8`.

The problem requires using a lambda function. We can combine `map` with a lambda function to create a concise solution. The lambda will take a single number from the list (let's call it `x`) and calculate `x ^ n`.

```haskell
-- 'n' is the power, and 'inputList' is the list of integers
let resultList = map (\x -> x ^ n) inputList
```

*   `(\x -> ...)` defines an anonymous function (a lambda) that takes one argument, `x`.
*   `x ^ n` is the body of the lambda, which calculates the power.
*   `map` applies this lambda to every element in `inputList`.

### 5. Handling User Input

To make the program interactive, you need to get the power `n` from the user. This involves I/O operations, which are handled in the `IO` monad in Haskell.

**Reading Input with `getLine`**
The `getLine :: IO String` function reads a line of text from the user's input.

**Parsing Input with `readMaybe`**
The string from `getLine` must be converted to an integer. A naive way is to use `read`, but this will crash the program if the user enters non-numeric text. A much safer way is to use `readMaybe` from the `Text.Read` module.

`readMaybe :: Read a => String -> Maybe a`

*   `readMaybe` attempts to parse a string.
*   If successful, it returns `Just value` (e.g., `Just 5`).
*   If it fails, it returns `Nothing`.

You must add `import Text.Read (readMaybe)` to the top of your file to use it.