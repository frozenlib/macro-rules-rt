macro_rules! apply { {
        ( $($from:tt)* ),
        ( $($to:tt)* ),
        ( $($input:tt)* ),
    } => { {
            macro_rules! mr {
                { $($from)* } => { quote::quote!($($to)*) };
            }
            println!("{}", mr! { $($input)* }.to_string());
        }
    };
}
