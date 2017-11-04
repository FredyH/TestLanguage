
data Nat = Zero | Succ Nat

def natToInt(n: Nat): Int {
    match n {
        | Zero => 0
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

def main(): Nat {
    intToNat(1000)
}