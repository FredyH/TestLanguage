
data List = Nil | Cons Int List

def createIntList(n: Int): List {
    match n {
        | 0 => Nil
        | n => Cons(n, createNatList(n - 1))
    }
}

def main(): List {
    natToInt()
}