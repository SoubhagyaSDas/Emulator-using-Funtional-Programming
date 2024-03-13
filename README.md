# Emulator using Functional Programming

This F# program implements a simple emulator using functional programming concepts. It provides functions to perform bitwise logical operations (NOT, AND, OR, XOR) and arithmetic operations (ADD, SUB) on hexadecimal values and binary bits.

## Functions

### `dec_to_bin`
- Converts a decimal number to an 8-bit binary list.

### `bin_to_dec`
- Converts an 8-bit binary list to a decimal number.

### `perform_not(hexValue)`
- Performs a bitwise NOT operation on the hexadecimal value provided.
- Returns the binary list result and its decimal equivalent.

### `perform_and(hexValue1, hexValue2)`
- Performs a bitwise AND operation between two hexadecimal values.
- Returns the binary list result and its decimal equivalent.

### `perform_or(hexValue1, hexValue2)`
- Performs a bitwise OR operation between two hexadecimal values.
- Returns the binary list result and its decimal equivalent.

### `perform_xor(hexValue1, hexValue2)`
- Performs a bitwise XOR operation between two hexadecimal values.
- Returns the binary list result and its decimal equivalent.

### `perform_add(hexValue1, hexValue2)`
- Converts the hexadecimal values to binary, performs binary addition, and converts the result back to hexadecimal and decimal.
- Returns the binary list result and its decimal equivalent.

### `perform_sub(hexValue1, hexValue2)`
- Converts the hexadecimal values to binary, performs binary subtraction, and converts the result back to hexadecimal and decimal.
- Returns the binary list result and its decimal equivalent.

### `emulator()`
- Interactive console-based emulator loop.
- Prompts the user to select an operation (NOT, AND, OR, XOR, ADD, SUB, or QUIT) and provides input accordingly.
- Executes the selected operation and displays the result.
- Loops until the user chooses to quit.

## How to Run

1. Ensure you have the .NET SDK installed on your system.
2. Clone the repository containing the code.
3. Navigate to the directory containing the code.
4. Open a terminal or command prompt.
5. Run the following command to build and execute the code:

```bash
dotnet run
