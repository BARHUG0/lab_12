This document provides a consolidated explanation of the concepts required to solve Problem 4.

### 1. Problem Description

The task is to write a Haskell program that removes elements from an initial list that are present in a second "to-remove" list. The solution should use a lambda function.

**Example:**
*   **Initial List**: `["rojo", "verde", "azul", "amarillo", "gris", "blanco", "negro"]`
*   **To-Remove List**: `["amarillo", "cafe", "blanco"]`
*   **Output List**: `["rojo", "verde", "azul", "gris", "negro"]`

### 2. Data Representation

Both the initial list of items and the list of items to remove are lists of strings. The appropriate Haskell type for both is `[String]`.

### 3. The Core Logic: The `filter` Function

In functional programming, when you need to select a subset of a list based on a condition, the standard tool is the `filter` function.

The type signature for `filter` is:
`filter :: (a -> Bool) -> [a] -> [a]`

*   It takes a **predicate function** `(a -> Bool)` and a list `[a]`.
*   A predicate is a function that returns `True` or `False`.
*   `filter` returns a new list containing only those elements from the input list for which the predicate returned `True`.

### 4. Checking for List Membership

To create our predicate, we need a way to check if an element from the initial list exists in the "to-remove" list. Haskell provides two helpful functions for this, which can be used in either prefix or infix (with backticks) notation.

**`elem`**: Checks if an element is **in** a list.
*   Prefix: `elem 'a' "banana"` (returns `True`)
*   Infix: `'a' `elem` "banana"` (returns `True`)

**`notElem`**: Checks if an element is **not in** a list. This is exactly what we need.
*   Prefix: `notElem 'z' "banana"` (returns `True`)
*   Infix: `'z' `notElem` "banana"` (returns `True`)

