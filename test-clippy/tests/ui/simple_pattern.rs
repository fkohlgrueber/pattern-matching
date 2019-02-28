

fn main() {
    
    // match literal false
    if (false) {
    }

    // match array sequences
    let a = ['b', 'c'];
    let b = ['b', 'b', 'c'];
    let no_warn = ['b'];
    
    // match cast
    let x = 0 as *const i32;    

    // match if
    let y = if true {
        123
    } else {
        456
    };
    if true {
        true;
    }
    let z = if true {
        true;
        12
    }else {
        34
    };
}
