
// QUESTION 1 A

let findMax f a b = 
    let calcJump a b = (abs a + abs b) / 10000.0
    let rec doFind f cur limit jump =
        if cur > limit then -1.0
        else
            let next = doFind f (cur + jump) limit jump
            if f cur > next then f cur
                            else next
    let jump = calcJump a b
    doFind f a b jump

let func1 x = 4.0 * (x * x * x) - 2.0 * (x * x) + 9.0 * x - 2.0
let func2 x = -9.0 * (x * x) + 2.0 * x + 8.0

let max1 = findMax func1 -10.0 10.0
let max2 = findMax func2 -2.0 4.0

let calcArea (max:float) a b = max * (b - a)

let area a b = findMax func2 a b |> calcArea a b

let rand = System.Random()

let calcIntegral (f:float -> float) (area:float) num a b  =
    let underGraph (f:float -> float) (x, y) = f x > y
    let integral (B:float) = (B / num) * area
    let rec count f a b (num:float) c B =
        match c with
        | _ when c < num -> if underGraph f (float (rand.Next(a, b)), float (rand.Next(a, b)))  then count f a b num (c + 1.0) (B + 1)
                                                                                    else count f a b num (c + 1.0) B
        | _ -> B
    count f a b num 0.0 0 |> float |> integral 

let calcGraphIntegral f a b =
    let area = findMax f a b |> calcArea a b
    calcIntegral f area 1000000.0 (int a) (int b)



calcIntegral func2 1.0 1000000.0 (int (-2.0)) (int (4.0))

calcGraphIntegral func2 -90.0 100.0

// QUESTION 1 B



// QUESTION 2 A

let list1 = [1;5;9;-2;10;1;-2;3;23;-1]
let list2 = [7..500]

let successiveMult list = 
    let rec findMax list = 
        match list with
        |(x::y::z::v::xs) -> 
            let next = findMax (y::z::v::xs)
            match next with
            |(_,value) -> if x * y * z * v > value then string x + "*" + string y + "*" + string z + "*" + string v, x * y * z * v
                                                    else next

        |_ -> ("", -1)
    findMax list
            
successiveMult list1

// QUESTION 2 B

let rec concatList list = 
    let toInt s = (sprintf "%A" s + sprintf "%A" s)  |> int
    //let rec doList list = 
    match list with
    |(x::xs) -> let s : int = toInt x 
                s :: concatList xs
    |[] -> []
  
    //doList list

let rec sumList list = 
    match list with
    |x::xs when x % 7 <> 0 -> x + sumList xs
    |[] -> 0
    |_::xs -> 0 + sumList xs
    
sumList list2
sumList (concatList list2)
let l = concatList list2
sumList l

// QUESTION 3 A

type Operator = 
    |Plus
    |Minus
    |Multiply
    |Divide

type BooleanOperator =
    |GreaterThan
    |GreaterThanEqual
    |LessThan
    |LessThanEqual
    |Equal

type UnaryOperator = 
    |Negate
    |Increment
    |Decrement

type Expression = 
    |Literal of float
    |UnaryExpression of UnaryOperator * Expression
    |DualExpression of Expression * Operator * Expression
    |BoolExpression of Expression * BooleanOperator * Expression * Expression * Expression

let ex1 = DualExpression(Literal(2.0), Plus, Literal(4.0))
let ex2 = DualExpression(Literal(8.0), Divide, Literal(2.0))
let ex3 = DualExpression(ex1, Multiply, ex2)

// QUESTION 3 B

let rec evaluateTree expression = 
    match expression with
    | Literal(x) -> x
    | UnaryExpression(op, ex) -> 
        match op with
        | Negate    -> evaluateTree ex * -1.0
        | Increment -> evaluateTree ex + 1.0
        | Decrement -> evaluateTree ex - 1.0
    | DualExpression(ex1, op, ex2) -> 
        match op with
        | Plus      -> evaluateTree ex1 + evaluateTree ex2
        | Minus     -> evaluateTree ex1 - evaluateTree ex2
        | Divide    -> evaluateTree ex1 / evaluateTree ex2
        | Multiply  -> evaluateTree ex1 * evaluateTree ex2
    | BoolExpression(ex1, opB, ex2, tEx, fEx) ->
        match opB with
        | GreaterThan       -> if evaluateTree ex1 >  evaluateTree ex2  then evaluateTree tEx else evaluateTree fEx
        | GreaterThanEqual  -> if evaluateTree ex1 >= evaluateTree ex2  then evaluateTree tEx else evaluateTree fEx 
        | LessThan          -> if evaluateTree ex1 <  evaluateTree ex2  then evaluateTree tEx else evaluateTree fEx
        | LessThanEqual     -> if evaluateTree ex1 <= evaluateTree ex2  then evaluateTree tEx else evaluateTree fEx
        | Equal             -> if evaluateTree ex1 =  evaluateTree ex2  then evaluateTree tEx else evaluateTree fEx


evaluateTree ex3

let a = DualExpression(Literal(10.2), Plus, Literal(1.8))
let b = Literal(2.5)
let c = Literal(55.5)
let d = DualExpression(Literal(12.0), Divide, Literal(6.0))
let bEx = BoolExpression(a, GreaterThan, b, c, d)

evaluateTree bEx

// QUESTION 3 C

//let absTree n = UnaryExpression(Negate, Literal(n))
let absTree n = BoolExpression(n, GreaterThan, Literal(0.0), n, UnaryExpression(Negate, n))

// QUESTION 3 D

let maxValueTree x y = DualExpression(DualExpression(DualExpression(x, Plus, y), Plus, absTree(DualExpression(x, Minus, y))), Divide, Literal(2.0))

maxValueTree (Literal 23.1) (Literal -25.9) |> evaluateTree 



// QUESTION 3 E

let rec maxAbsList (list:List<float>) = 
    match list with
    |(x::xs) -> 
        let m = maxAbsList xs
        maxValueTree (absTree (Literal x)) m
    |[] -> Literal(-1.0)


let list3 = [32.0;432.0;23.0;-8.0;-8782.0;60.0;2.0;-345.0;4323.0;23.0;-12.0]

let t = maxAbsList list3
evaluateTree t