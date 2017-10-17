
def fak(n: Int): Int {
    if (n == 0) {
        1
    } else {
        n * fak(n - 1)
    }
}

def test(n: Int, m: Int): Int {
    n + m
}

def main(): Int {
    fak(5)
    test(1, 4)
}

data List = Nil | Cons Int List

data Nat = Zero | Succ Nat