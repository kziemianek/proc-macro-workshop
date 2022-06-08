use syn::{parse_macro_input, Data, DeriveInput};

mod struct_builder;

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    match &input.data {
        Data::Struct(data_struct) => {
            proc_macro::TokenStream::from(struct_builder::create(input.ident, data_struct))
        }
        _ => unimplemented!(),
    }
}
