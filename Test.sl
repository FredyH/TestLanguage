data Tree = Tip | Bin Tree Int Tree

data Pair = P Int Int

data List = Nil | Cons Int List

data Nat = Zero | Succ Nat

data Option = Some Int | None

def fak(n: Int): Int {
    if (n == 0) {
        1
    } else {
        n * fak(n - 1)
    }
}

def createNatList(n: Int): List {
    match n {
        | 0 => Nil
        | n => Cons(n, createNatList(n - 1))
    }
}

def natToInt(n: Nat): Int {
    match n {
        | Zero => match n {
                | Zero => 0
            }
        | Succ(Succ(Succ(l))) => natToInt(l) + 3
        | Succ(Succ(l)) => natToInt(l) + 2
        | Succ(m) => natToInt(m) + 1
    }
}

def intToNat(n: Int): Nat {
    if (n <= 0) {
        Zero
    } else {
        Succ(intToNat(n - 1))
    }
}

def test(n: Int, m: Int): Float {
    Float(n + m)
}

def main(): Pair {
    createNatList(1000)
    P(5, 10)
}