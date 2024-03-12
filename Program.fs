let rec dec_to_bin decimal =
    let rec dec_to_bin_helper decimal acc =
        match decimal with
        | 0 -> acc
        | _ -> dec_to_bin_helper (decimal / 2) ((decimal % 2) :: acc)
    let binList = dec_to_bin_helper decimal []
    List.rev (List.padLeft 8 0 binList)

let rec bin_to_dec binary =
    let rec bin_to_dec_helper binary acc power =
        match binary with
        | [] -> acc
        | bit::bits -> bin_to_dec_helper bits (acc + bit * (1 <<< power)) (power + 1)
    bin_to_dec_helper binary 0 0

let rec perform_not hexValue =
    let decValue = Convert.ToInt32(hexValue, 16)
    let binaryValue = dec_to_bin decValue
    let result = List.map (fun bit -> if bit = 0 then 1 else 0) binaryValue
    (result, bin_to_dec (List.rev result))

let rec perform_and hexValue1 hexValue2 =
    let decValue1 = Convert.ToInt32(hexValue1, 16)
    let decValue2 = Convert.ToInt32(hexValue2, 16)
    let binaryValue1 = dec_to_bin decValue1
    let binaryValue2 = dec_to_bin decValue2
    let result = List.map2 (&&&) binaryValue1 binaryValue2
    (result, bin_to_dec (List.rev result))

let rec perform_or hexValue1 hexValue2 =
    let decValue1 = Convert.ToInt32(hexValue1, 16)
    let decValue2 = Convert.ToInt32(hexValue2, 16)
    let binaryValue1 = dec_to_bin decValue1
    let binaryValue2 = dec_to_bin decValue2
    let result = List.map2 (|||) binaryValue1 binaryValue2
    (result, bin_to_dec (List.rev result))

let rec perform_xor hexValue1 hexValue2 =
    let decValue1 = Convert.ToInt32(hexValue1, 16)
    let decValue2 = Convert.ToInt32(hexValue2, 16)
    let binaryValue1 = dec_to_bin decValue1
    let binaryValue2 = dec_to_bin decValue2
    let result = List.map2 (^^^) binaryValue1 binaryValue2
    (result, bin_to_dec (List.rev result))

let rec perform_add number1 number2 =
    let result = number1 + number2
    (result, dec_to_bin result)

let rec perform_sub number1 number2 =
    let result = number1 - number2
    (result, dec_to_bin result)

let rec emulator () =
    printfn "Enter the operation you want to perform (NOT, OR, AND, XOR, ADD, SUB or QUIT): "
    let operation = Console.ReadLine().ToLower()
    match operation with
    | "not" -> 
        printfn "Enter Hex value between 00 and FF: "
        let hexValue = Console.ReadLine()
        let (result, resultDec) = perform_not hexValue
        printfn "Result of NOT [%A] = [%A] = %X" (dec_to_bin (Convert.ToInt32(hexValue, 16))) result resultDec
        emulator ()
    | "and" -> 
        printfn "Enter Hex value between 00 and FF: "
        let hexValue1 = Console.ReadLine()
        printfn "Enter Hex value between 00 and FF: "
        let hexValue2 = Console.ReadLine()
        let (result, resultDec) = perform_and hexValue1 hexValue2
        printfn "       [%A] = %s" (dec_to_bin (Convert.ToInt32(hexValue1, 16))) hexValue1.ToUpper()
        printfn "AND    [%A] = %s" (dec_to_bin (Convert.ToInt32(hexValue2, 16))) hexValue2.ToUpper()
        printfn "--------------------------------------------"
        printfn "       [%A] = %X" result resultDec
        emulator ()
    | "or" -> 
        printfn "Enter Hex value between 00 and FF: "
        let hexValue1 = Console.ReadLine()
        printfn "Enter Hex value between 00 and FF: "
        let hexValue2 = Console.ReadLine()
        let (result, resultDec) = perform_or hexValue1 hexValue2
        printfn "       [%A] = %s" (dec_to_bin (Convert.ToInt32(hexValue1, 16))) hexValue1.ToUpper()
        printfn "OR     [%A] = %s" (dec_to_bin (Convert.ToInt32(hexValue2, 16))) hexValue2.ToUpper()
        printfn "--------------------------------------------"
        printfn "       [%A] = %X" result resultDec
        emulator ()
    | "xor" -> 
        printfn "Enter Hex value between 00 and FF: "
        let hexValue1 = Console.ReadLine()
        printfn "Enter Hex value between 00 and FF: "
        let hexValue2 = Console.ReadLine()
        let (result, resultDec) = perform_xor hexValue1 hexValue2
        printfn "       [%A] = %s" (dec_to_bin (Convert.ToInt32(hexValue1, 16))) hexValue1.ToUpper()
        printfn "XOR    [%A] = %s" (dec_to_bin (Convert.ToInt32(hexValue2, 16))) hexValue2.ToUpper()
        printfn "--------------------------------------------"
        printfn "       [%A] = %X" result resultDec
        emulator ()
    | "add" -> 
        printfn "Enter a number between -128 and 127: "
        let number1 = int(Console.ReadLine())
        printfn "Enter a number between -128 and 127: "
        let number2 = int(Console.ReadLine())
        let (result, resultBin) = perform_add number1 number2
        printfn "       [%A] = %d" (dec_to_bin number1) number1
        printfn "ADD    [%A] = %d" (dec_to_bin number2) number2
        printfn "--------------------------------------------"
        printfn "       [%A] = %d" resultBin result
        emulator ()
    | "sub" -> 
        printfn "Enter a number between -128 and 127: "
        let number1 = int(Console.ReadLine())
        printfn "Enter a number between -128 and 127: "
        let number2 = int(Console.ReadLine())
        let (result, resultBin) = perform_sub number1 number2
        printfn "       [%A] = %d" (dec_to_bin number1) number1
        printfn "SUB    [%A] = %d" (dec_to_bin number2) number2
        printfn "--------------------------------------------"
        printfn "       [%A] = %d" resultBin result
        emulator ()
    | "quit" -> ()
    | _ -> 
        printfn "Invalid operation"
        emulator ()

emulator ()
