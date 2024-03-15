open System

let rec dec_to_bin decimal =
    let rec dec_to_bin_helper decimal acc =
        match decimal with
        | 0 -> acc
        | _ -> dec_to_bin_helper (decimal / 2) ((decimal % 2) :: acc)
    let binList = dec_to_bin_helper decimal []
    List.rev (List.init (8 - List.length binList) (fun _ -> 0) @ binList)

let rec bin_to_dec binary =
    let rec bin_to_dec_helper binary acc power =
        match binary with
        | [] -> acc
        | bit::bits -> bin_to_dec_helper bits (acc + bit * (1 <<< power)) (power + 1)
    bin_to_dec_helper binary 0 0

let perform_not hexValue =
    let decValue = Convert.ToInt32(hexValue, 16)
    let binaryValue = dec_to_bin decValue
    let result = List.map (fun bit -> if bit = 0 then 1 else 0) binaryValue
    let resultDec = bin_to_dec (List.rev result)
    (result, resultDec)

let perform_and hexValue1 hexValue2 =
    let decValue1 = Convert.ToInt32(hexValue1, 16)
    let decValue2 = Convert.ToInt32(hexValue2, 16)
    let binaryValue1 = dec_to_bin decValue1
    let binaryValue2 = dec_to_bin decValue2
    let paddedBinary1 = List.rev (List.init (8 - List.length binaryValue1) (fun _ -> 0) @ binaryValue1)
    let paddedBinary2 = List.rev (List.init (8 - List.length binaryValue2) (fun _ -> 0) @ binaryValue2)
    let result = List.map2 (&&&) paddedBinary1 paddedBinary2
    let resultDec = bin_to_dec (List.rev result)
    (result, resultDec)

let perform_or hexValue1 hexValue2 =
    let decValue1 = Convert.ToInt32(hexValue1, 16)
    let decValue2 = Convert.ToInt32(hexValue2, 16)
    let binaryValue1 = dec_to_bin decValue1
    let binaryValue2 = dec_to_bin decValue2
    let paddedBinary1 = List.rev (List.init (8 - List.length binaryValue1) (fun _ -> 0) @ binaryValue1)
    let paddedBinary2 = List.rev (List.init (8 - List.length binaryValue2) (fun _ -> 0) @ binaryValue2)
    let result = List.map2 (|||) paddedBinary1 paddedBinary2
    let resultDec = bin_to_dec (List.rev result)
    (result, resultDec)

let perform_xor hexValue1 hexValue2 =
    let decValue1 = Convert.ToInt32(hexValue1, 16)
    let decValue2 = Convert.ToInt32(hexValue2, 16)
    let binaryValue1 = dec_to_bin decValue1
    let binaryValue2 = dec_to_bin decValue2
    let paddedBinary1 = List.rev (List.init (8 - List.length binaryValue1) (fun _ -> 0) @ binaryValue1)
    let paddedBinary2 = List.rev (List.init (8 - List.length binaryValue2) (fun _ -> 0) @ binaryValue2)
    let result = List.map2 (^^^) paddedBinary1 paddedBinary2
    let resultDec = bin_to_dec (List.rev result)
    (result, resultDec)

let rec perform_add binList1 binList2 =
    let rec add_binary_lists list1 list2 carry acc =
        match list1, list2 with
        | [], [] ->
            if carry = 1 then 1 :: acc
            else acc
        | bit1::rest1, bit2::rest2 ->
            let sum = bit1 + bit2 + carry
            let new_bit = sum % 2
            let new_carry = sum / 2
            add_binary_lists rest1 rest2 new_carry (new_bit :: acc)
        | _, _ -> failwith "Invalid input lengths for addition"
    
    let reversedResult = add_binary_lists (List.rev binList1) (List.rev binList2) 0 []
    let result = List.rev reversedResult

    if List.length result <= 8 then
        (result, bin_to_dec result)
    else
        failwith "Overflow occurred during addition"

let rec perform_sub binList1 binList2 =
    let rec sub_binary_lists list1 list2 borrow acc =
        match list1, list2 with
        | [], [] ->
            if borrow = 1 then failwith "Subtraction resulted in negative value"
            else acc
        | bit1::rest1, bit2::rest2 ->
            let diff = bit1 - bit2 - borrow
            let new_bit, new_borrow =
                if diff < 0 then (diff + 2, 1)
                else (diff, 0)
            sub_binary_lists rest1 rest2 new_borrow (new_bit :: acc)
        | _, _ -> failwith "Invalid input lengths for subtraction"
    
    let reversedResult = sub_binary_lists (List.rev binList1) (List.rev binList2) 0 []
    let result = List.rev reversedResult

    if List.length result <= 8 then
        (result, bin_to_dec result)
    else
        failwith "Overflow occurred during subtraction"

let rec emulator () =
    printfn "Enter the operation you want to perform (NOT, OR, AND, XOR, ADD, SUB or QUIT): "
    let operation = Console.ReadLine().ToLower()
    match operation with
    | "not" -> 
        printfn "Enter Hex value between 00 and FF: "
        let hexValue = Console.ReadLine()
        let (result, resultDec) = perform_not hexValue
        printfn "Result of NOT [%A] = [%A] = %X" (dec_to_bin (Convert.ToInt32(hexValue, 16))) (List.map string result) resultDec
        emulator ()
    | "and" -> 
        printfn "Enter Hex value between 00 and FF: "
        let hexValue1 = Console.ReadLine()
        printfn "Enter Hex value between 00 and FF: "
        let hexValue2 = Console.ReadLine()
        let (result, resultDec) = perform_and hexValue1 hexValue2
        printfn "       [%A] = %s" (dec_to_bin (Convert.ToInt32(hexValue1, 16))) hexValue1
        printfn "AND    [%A] = %s" (dec_to_bin (Convert.ToInt32(hexValue2, 16))) hexValue2
        printfn "--------------------------------------------"
        printfn "       [%A] = %X" (List.map string result) resultDec
        emulator ()
    | "or" -> 
        printfn "Enter Hex value between 00 and FF: "
        let hexValue1 = Console.ReadLine()
        printfn "Enter Hex value between 00 and FF: "
        let hexValue2 = Console.ReadLine()
        let (result, resultDec) = perform_or hexValue1 hexValue2
        printfn "       [%A] = %s" (dec_to_bin (Convert.ToInt32(hexValue1, 16))) hexValue1
        printfn "OR     [%A] = %s" (dec_to_bin (Convert.ToInt32(hexValue2, 16))) hexValue2
        printfn "--------------------------------------------"
        printfn "       [%A] = %X" (List.map string result) resultDec
        emulator ()
    | "xor" -> 
        printfn "Enter Hex value between 00 and FF: "
        let hexValue1 = Console.ReadLine()
        printfn "Enter Hex value between 00 and FF: "
        let hexValue2 = Console.ReadLine()
        let (result, resultDec) = perform_xor hexValue1 hexValue2
        printfn "       [%A] = %s" (dec_to_bin (Convert.ToInt32(hexValue1, 16))) hexValue1
        printfn "XOR    [%A] = %s" (dec_to_bin (Convert.ToInt32(hexValue2, 16))) hexValue2
        printfn "--------------------------------------------"
        printfn "       [%A] = %X" (List.map string result) resultDec
        emulator ()
    | "add" -> 
        printfn "Enter a number between -128 and 127: "
        let number1 = int(Console.ReadLine())
        printfn "Enter a number between -128 and 127: "
        let number2 = int(Console.ReadLine())
        let (result, _) = perform_add (dec_to_bin number1) (dec_to_bin number2)
        let resultDec = bin_to_dec result
        printfn "       [%A] = %d" (dec_to_bin number1) number1
        printfn "ADD    [%A] = %d" (dec_to_bin number2) number2
        printfn "--------------------------------------------"
        printfn "       [%A] = %d" (List.map string result) resultDec
        emulator ()
    | "sub" -> 
        printfn "Enter a number between -128 and 127: "
        let number1 = int(Console.ReadLine())
        printfn "Enter a number between -128 and 127: "
        let number2 = int(Console.ReadLine())
        let (result, _) = perform_sub (dec_to_bin number1) (dec_to_bin number2)
        let resultDec = bin_to_dec result
        printfn "       [%A] = %d" (dec_to_bin number1) number1
        printfn "SUB    [%A] = %d" (dec_to_bin number2) number2
        printfn "--------------------------------------------"
        printfn "       [%A] = %d" (List.map string result) resultDec
        emulator ()
    | "quit" -> printfn "Goodbye!"
    | _ -> 
        printfn "Invalid operation"
        emulator ()

emulator ()
