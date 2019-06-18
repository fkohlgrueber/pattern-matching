
macro_rules! pattern {
    ($($tokens:tt)*) => {
        // empty
    };
}

macro_rules! abc {
    ($($tokens:tt)*) => {
        // empty
    };
}

fn main() {
    // ...
}

pattern!{
    something: Expr = 
        Lit(_) | _
}

pattern!{
    something: Expr = 
        _ | Lit(_)
}

pattern!{
    something: Expr = 
        _ | Array(_+ Lit(_){5})
}


pattern!{
    something: Expr = 
        Lit(_)
}

abc!{
    something: Expr = 
        Lit(_) | _
}

pattern!{
    bla
}

