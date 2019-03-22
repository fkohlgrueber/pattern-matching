
use syn::parse_str;

enum SimplePattern {
    Node(String, Vec<SimplePattern>),
    Seq(Vec<SimplePattern>)
}

use std::fmt;

impl fmt::Debug for SimplePattern {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        match self {
            SimplePattern::Seq(s) => if s.is_empty() {
                fmt.write_str("()")
            } else {
                for e in s {
                    e.fmt(fmt)?;
                    fmt.write_str("\n")?;
                }
                Ok(())
            },
            SimplePattern::Node(s, v) => {
                let mut tup = fmt.debug_tuple(&s);
                for e in v {
                    tup.field(e);
                }
                tup.finish()
            }
        }
    }
}

static INDENT_SIZE: usize = 4;

impl SimplePattern {
    pub fn to_string(&self, indent: usize, max_width: usize) -> String {
        match self {
            SimplePattern::Node(s, v) => {
                if v.is_empty() {
                    return format!("{}", s);
                }

                if self.get_width() + indent > max_width {
                    // show with newlines
                    let elmts = v.iter()
                        .map(|x| format!(
                            "{}", 
                            x.to_string(indent+INDENT_SIZE, max_width)
                        ))
                        .collect::<Vec<_>>()
                        .join(&format!(",\n{}", " ".repeat(indent+INDENT_SIZE), ));
                    format!("{}(\n{}{}\n{})", s, " ".repeat(indent+INDENT_SIZE), elmts, " ".repeat(indent))
                    
                } else {
                    // show on single line
                    format!("{}({})", s, v.iter().map(|x| x.to_string(indent, max_width)).collect::<Vec<_>>().join(", "))
                }
            },
            SimplePattern::Seq(v) => {
                if v.is_empty() {
                    return format!("()");
                } 

                if self.get_width() + indent > max_width {
                    // show with newlines
                    let elmts = v.iter()
                        .map(|x| format!(
                            "{}",
                            x.to_string(indent, max_width)
                        ))
                        .collect::<Vec<_>>()
                        .join(&format!("\n{}", " ".repeat(indent)));
                    format!("{}", elmts)
                    
                } else {
                    // show on single line
                    format!("{}", v.iter().map(|x| x.to_string(indent, max_width)).collect::<Vec<_>>().join(" "))
                }
            }
        }
    }

    fn get_width(&self) -> usize {
        match self {
            SimplePattern::Node(s, v) => s.len() + if v.is_empty() {
                0
            } else {
                2 + v.iter().map(|x| x.get_width()).sum::<usize>() + (v.len()-1) * 2
            },
            SimplePattern::Seq(v) => if v.is_empty() {
                2
            } else {
                v.iter().map(|x| x.get_width()).sum::<usize>() + v.len()-1
            }

        }
    }
}

trait ToPattern {
    fn to_pattern(&self) -> Result<SimplePattern, String>;
}

impl ToPattern for syn::Expr {
    fn to_pattern(&self) -> Result<SimplePattern, String> {
        match self {
            syn::Expr::Lit(x) => Ok(SimplePattern::Node(
                "Lit".to_string(), 
                vec!(x.lit.to_pattern()?)
            )),
            syn::Expr::Array(x) => Ok(SimplePattern::Node(
                "Array".to_string(), 
                vec!(SimplePattern::Seq(x.elems.iter().map(|e| e.to_pattern()).collect::<Result<Vec<_>, _>>()?))
            )),
            syn::Expr::Block(x) => Ok(SimplePattern::Node(
                "Block_".to_string(), 
                vec!(x.block.to_pattern()?)
            )),
            syn::Expr::If(x) => Ok(SimplePattern::Node(
                "If".to_string(),
                vec!(
                    x.cond.to_pattern()?,
                    x.then_branch.to_pattern()?,
                    SimplePattern::Seq(match &x.else_branch {
                        Some(e) => vec!(e.1.to_pattern()?),
                        None => vec!()
                    })
                )
            )),
            // FIXME: implement IfLet
            
            _ => Err(format!("Not supported yet"))
        }
    }
}

impl ToPattern for syn::Lit {
    fn to_pattern(&self) -> Result<SimplePattern, String> {
        match self {
            syn::Lit::Bool(x) => Ok(SimplePattern::Node(
                "Bool".to_string(), 
                vec!(SimplePattern::Node(x.value.to_string(), vec!()))
            )),
            syn::Lit::Char(x) => Ok(SimplePattern::Node(
                "Char".to_string(),
                vec!(SimplePattern::Node(format!("{:?}", x.value()), vec!()))
            )),
            syn::Lit::Int(x) => Ok(SimplePattern::Node(
                "Int".to_string(),
                vec!(
                    SimplePattern::Node(x.value().to_string(), vec!()),
                    match x.suffix() {
                        syn::IntSuffix::None => SimplePattern::Node("Unsuffixed".to_string(), vec!()),
                        // FIXME: implement other types
                        _ => return Err(format!("Not supported yet"))
                    }
                )
            )),
            _ => Err(format!("Not supported yet"))
        }
    }
}

impl ToPattern for syn::Block {
    fn to_pattern(&self) -> Result<SimplePattern, String> {
        Ok(SimplePattern::Node(
            "Block".to_string(), 
            vec!(SimplePattern::Seq(self.stmts.iter().map(|e| e.to_pattern()).collect::<Result<Vec<_>, _>>()?))
        ))
    }
}

impl ToPattern for syn::Stmt {
    fn to_pattern(&self) -> Result<SimplePattern, String> {
        match self {
            syn::Stmt::Expr(x) => Ok(SimplePattern::Node(
                "Expr".to_string(), 
                vec!(x.to_pattern()?)
            )),
            syn::Stmt::Semi(x, _) => Ok(SimplePattern::Node(
                "Semi".to_string(),
                vec!(x.to_pattern()?)
            )),
            _ => Err(format!("Not supported yet"))
        }
    }
}

pub fn calc_pattern_for_syntax(input: &str) -> String {
    let node: syn::Expr = match parse_str(input) {
        Ok(node) => node,
        Err(s) => return format!("Error parsing the expression: \n\n{}", s)
    };

    match node.to_pattern() {
        //Ok(pattern) => format!("{:#?}", pattern),
        Ok(pattern) => pattern.to_string(0, 80),
        Err(err) => format!("Error generating the pattern:\n\n{}", err)
    }
}
