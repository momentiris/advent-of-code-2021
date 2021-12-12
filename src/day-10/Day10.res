open Belt

module Input = {
  let data =
    Node.Fs.readFileAsUtf8Sync("./input.txt")
    ->Js.String2.split("\n")
    ->Array.map(x => x->Js.String2.split(""))

  let sample =
    "[({(<(())[]>[[{[]{<()<>>
[(()[<>])]({[<{<<[]>>(
{([(<{}[<>[]}>{[]{[(<()>
(((({<>}<{<{<>}{[]{[]{}
[[<[([]))<([[{}[[()]]]
[{[{({}]{}}([{[{{{}}([]
{<[[]]>}<{[{[{[]{()[[[]
[<(<(<(<{}))><([]([]()
<{([([[(<>()){}]>(<<{{
<{([{{}}[<[[[<>{}]]]>[]]"
    ->Js.String2.split("\n")
    ->Array.map(x => x->Js.String2.split(""))

  // let rowLen = sample->Js.String2.split("\n")->Array.getUnsafe(0)->String.length
}

module Part1 = {
  let matching = Js.Dict.empty()
  Js.Dict.set(matching, "{", "}")
  Js.Dict.set(matching, "[", "]")
  Js.Dict.set(matching, "(", ")")
  Js.Dict.set(matching, "<", ">")

  let isOpeningBracket = x => ["{", "[", "(", "<"]->Js.Array2.includes(x)

  let isBalanced = (expr: array<string>) => {
    let stack = ref([])
    let failChar = ref("")

    let returnVal = ref(false)
    for i in 0 to expr->Array.size - 1 {
      let current = expr->Array.getUnsafe(i)

      if isOpeningBracket(current) {
        Js.Array2.push(stack.contents, current)->ignore
      } else {
        let last = Js.Array2.pop(stack.contents)->Option.getUnsafe

        if current !== Js.Dict.unsafeGet(matching, last) {
          failChar.contents = current
          returnVal.contents = false
        }
      }

      if stack.contents->Array.size !== 0 {
        returnVal.contents = false
      } else {
        returnVal.contents = true
      }
    }

    failChar.contents
  }

  let getIllegalCharPoint = char =>
    switch char {
    | ")" => 3
    | "]" => 57
    | "}" => 1197
    | ">" => 25137
    | _ => 0
    }

  let make = () => {
    // "{([(<{}[<>[]}>{[]{[(<()>"
    // ->Js.String2.split("\n")
    // ->Array.map(x => x->Js.String2.split(""))

    Input.data
    ->Array.map(isBalanced)
    ->Array.keep(x => x !== "")
    ->Array.reduce(0, (acc, curr) => acc + getIllegalCharPoint(curr))
  }
}

module Part2 = {
  let matching = Js.Dict.empty()
  Js.Dict.set(matching, "{", "}")
  Js.Dict.set(matching, "[", "]")
  Js.Dict.set(matching, "(", ")")
  Js.Dict.set(matching, "<", ">")

  let isOpeningBracket = x => ["{", "[", "(", "<"]->Js.Array2.includes(x)

  let excludeBroken = (expr: array<string>) => {
    let stack = ref([])
    let val = ref(true)

    let returnVal = ref(false)
    for i in 0 to expr->Array.size - 1 {
      let current = expr->Array.getUnsafe(i)

      if isOpeningBracket(current) {
        Js.Array2.push(stack.contents, current)->ignore
      } else {
        let last = Js.Array2.pop(stack.contents)->Option.getUnsafe

        if current !== Js.Dict.unsafeGet(matching, last) {
          val.contents = false
          returnVal.contents = false
        }
      }
    }

    val.contents
  }

  let getIllegalCharPoint = char =>
    switch char {
    | ")" => 3
    | "]" => 57
    | "}" => 1197
    | ">" => 25137
    | _ => 0
    }

  let temp = (expr: array<string>) => {
    let stack = ref([])
    let failStack = ref([])

    let returnVal = ref(false)
    for i in 0 to expr->Array.size - 1 {
      let current = expr->Array.getUnsafe(i)

      if isOpeningBracket(current) {
        Js.Array2.push(stack.contents, current)->ignore
      } else {
        let last = Js.Array2.pop(stack.contents)->Option.getUnsafe

        if current !== Js.Dict.unsafeGet(matching, last) {
          returnVal.contents = false
        }
      }
    }
    if stack.contents->Array.size !== 0 {
      failStack.contents =
        stack.contents->Array.reverse->Array.map(x => matching->Js.Dict.unsafeGet(x))

      returnVal.contents = false
    } else {
      returnVal.contents = true
    }

    failStack.contents
  }

  let getCharScore = char =>
    switch char {
    | ")" => 1.0
    | "]" => 2.0
    | "}" => 3.0
    | ">" => 4.0
    | _ => 0.0
    }

  let make = () => {
    let scores =
      Input.data
      ->Array.keep(excludeBroken)
      ->Array.map(temp)
      ->Array.map(x => x->Array.reduce(0.0, (acc, curr) => acc *. 5.0 +. getCharScore(curr)))
      ->SortArray.stableSortBy((a, b) => a > b ? 1 : -1)

    scores->Array.getUnsafe(scores->Array.size / 2)
  }
}

Part2.make() |> Js.log2("Answer: ")
