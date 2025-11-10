This document provides a consolidated explanation of the concepts required to solve Problem 3.

### 1. Problem Description

The task is to write a Haskell program that calculates the transpose of a matrix, where the matrix is represented as a list of lists. The implementation must be done from scratch, using lambda functions where appropriate.

**Example:**
*   **Input Matrix**: `[[1, 2, 3], [4, 5, 6]]`
*   **Output Matrix**: `[[1, 4], [2, 5], [3, 6]]`

### 2. Data Representation: Type Alias

For clarity, it's good practice to create a type alias for our matrix structure.

```haskell
type Matrix = [[Int]]
```
This makes our function signatures, like `myTranspose :: Matrix -> Matrix`, much more readable.

### 3. The Core Logic: A Recursive Approach

The key insight to transposing a matrix is to see it as a recursive process:

1.  The **first row** of the transposed matrix is made up of the first element (`head`) of each row from the input matrix.
2.  The **rest** of the transposed matrix is simply the transpose of the *rest* of the input matrix (i.e., the `tail` of each row).

This leads to a natural recursive definition.

### 4. Pattern Matching in Function Definitions

Instead of using `if/else` or `case` within a single function body, Haskell allows you to provide multiple function definitions using **pattern matching**. Haskell checks the patterns from top to bottom and executes the body of the first one that matches the input.

This is similar to function overloading in other languages, but more powerful, as it matches on the **structure and values** of the arguments, not just their types.

### 5. Understanding the Base Case Patterns

The base case patterns can be confusing, so let's look at them closely.

**Pattern 1: `[]`**
*   This pattern matches **only** the empty list `[]`. It handles the case where the input matrix has zero rows.

**Pattern 2: `([]:_)`**
*   This pattern uses the `(:)` "cons" operator to deconstruct a list. It matches any **non-empty list** whose first element (`head`) is an empty list `[]`.
*   The `_` is a wildcard that means we don't care what the rest of the list (`tail`) is.
*   This pattern handles the case where we have processed all the columns, leaving a matrix with empty rows (e.g., `[[], [], []]`).

### 6. The Polymorphic Empty List `[]`

A key concept is that the empty list `[]` is **polymorphic**. Its type is `[a]`, where `a` can be any type. The compiler infers the specific type from the context.

This is why `[]` can be used in different ways in our patterns:

*   In `myTranspose [] = []`: The function signature is `Matrix -> Matrix`, which is `[[Int]] -> [[Int]]`. The compiler sees the pattern `[]` and treats it as a valid value of type `[[Int]]` (an empty list of lists).
*   In `myTranspose ([]:_) = []`: The input is a `[[Int]]`, so its elements must be of type `[Int]`. In this pattern, the `head` of the list is `[]`. The compiler sees this and treats it as a valid value of type `[Int]` (an empty list of integers).

The ability of `[]` to adapt its type to the context is a powerful feature of Haskell's type system.
