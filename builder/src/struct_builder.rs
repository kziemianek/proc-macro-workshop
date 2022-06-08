use proc_macro2::{Span, TokenStream};
use quote::{format_ident, quote, ToTokens};
use std::borrow::Borrow;
use syn::spanned::Spanned;
use syn::{
    AngleBracketedGenericArguments, DataStruct, Error, Fields, GenericArgument, Ident, Lit, Meta,
    NestedMeta, PathArguments, PathSegment, Type, TypePath,
};

pub fn create(struct_name: Ident, data_struct: &DataStruct) -> TokenStream {
    let builder_name = format_ident!("{}Builder", struct_name);
    let type_impl = create_built_struct_impl(&struct_name, &builder_name, data_struct);
    let builder_struct = create_builder_struct(&builder_name, data_struct);
    let builder_impl = create_builder_impl(&struct_name, &builder_name, data_struct).unwrap();

    quote! {
        use std::error::Error;
        #builder_struct
        #type_impl
        #builder_impl;
    }
}

fn create_builder_struct(name: &Ident, data: &DataStruct) -> TokenStream {
    let builder_fields = match data.fields {
        Fields::Named(ref fields) => {
            let builder_fields = fields.named.iter().map(|f| {
                let field_name = f.ident.as_ref().unwrap();
                let field_set_flag_name = format_ident!("{}_set", field_name);
                let field_type = &f.ty;
                quote! {
                    #field_name: #field_type,
                    #field_set_flag_name: bool,
                }
            });

            quote! {
                #(#builder_fields)*
            }
        }
        Fields::Unnamed(_) | Fields::Unit => unimplemented!(),
    };
    quote! {
        pub struct #name {
            #builder_fields
        }
    }
}

fn create_built_struct_impl(
    built_struct_name: &Ident,
    builder_name: &Ident,
    data: &DataStruct,
) -> TokenStream {
    let builder_initialized_fields = match data.fields {
        Fields::Named(ref fields) => {
            let field_initialized_fields = fields.named.iter().map(|f| {
                let name = &f.ident.as_ref().unwrap();
                let field_set_flag_name = format_ident!("{}_set", name);
                let default_value = quote! {
                    Default::default()
                };
                quote! {
                    #name: #default_value,
                    #field_set_flag_name: false,
                }
            });

            quote! {
                #(#field_initialized_fields)*
            }
        }
        Fields::Unnamed(_) | Fields::Unit => unimplemented!(),
    };

    quote! {
        impl #built_struct_name {
            pub fn builder() -> #builder_name {
                #builder_name {
                    #builder_initialized_fields
                }
            }
        }
    }
}

fn create_builder_impl(
    built_struct_name: &Ident,
    builder_name: &Ident,
    data: &DataStruct,
) -> syn::Result<TokenStream> {
    let builder_setters = create_builder_setters(data);
    let builder_build_fn = create_builder_build_fn(built_struct_name, data);
    Ok(quote! {
        impl #builder_name {
            #builder_setters
            #builder_build_fn
        }
    })
}

fn create_builder_build_fn(built_struct_name: &Ident, data: &DataStruct) -> TokenStream {
    let builder_field_set_check_expressions = create_builder_field_set_check_expressions(data);
    let builder_type_field_initializers = create_builder_field_initializers(data);
    quote! {
        pub fn build(&mut self) -> std::result::Result<#built_struct_name, std::boxed::Box<dyn std::error::Error>> {
                if(#builder_field_set_check_expressions) {
                    let err: std::boxed::Box<dyn std::error::Error> = std::boxed::Box::from("All fields should be initialized");
                    Err(err)
                } else {
                    let cmd = Command {
                        #builder_type_field_initializers
                    };
                    std::result::Result::Ok(cmd)
                }
            }
    }
}

fn create_builder_field_set_check_expressions(data: &DataStruct) -> TokenStream {
    match data.fields {
        Fields::Named(ref fields) => {
            let field_set_check_expressions = fields.named.iter().map(|f| {
                let name = &f.ident.as_ref().unwrap();
                let field_set_flag_name = format_ident!("{}_set", name);
                let field_type = &f.ty;

                match field_type {
                    Type::Path(type_path) => {
                        let option = type_path.path.segments.iter().find(|s| s.ident == "Option");
                        match option {
                            Some(_) => {
                                quote! {false}
                            }
                            _ => {
                                quote! {
                                    self.#field_set_flag_name == false
                                }
                            }
                        }
                    }
                    _ => {
                        quote! {
                            self.#field_set_flag_name == false
                        }
                    }
                }
            });
            quote! {
                #(#field_set_check_expressions)||*
            }
        }
        Fields::Unnamed(_) | Fields::Unit => unimplemented!(),
    }
}

fn create_builder_field_initializers(data: &DataStruct) -> TokenStream {
    match data.fields {
        Fields::Named(ref fields) => {
            let initializers = fields.named.iter().map(|f| {
                let name = &f.ident.as_ref().unwrap();
                quote! {
                    #name: self.#name.to_owned()
                }
            });
            quote! {
                #(#initializers),*
            }
        }
        Fields::Unnamed(_) | Fields::Unit => unimplemented!(),
    }
}

fn create_builder_setters(data: &DataStruct) -> TokenStream {
    match data.fields {
        Fields::Named(ref fields) => {
            let setters = fields.named.iter().map(|f| {
                let name = &f.ident.as_ref().unwrap();
                let field_type = &f.ty;
                //check if attribute exists and generate different method :)
                let attr = f.attrs.iter().find(|a| a.path.is_ident("builder"));
                let name_from_arg = format_ident!("{}", name);
                match attr {
                    Some(_attr) => {
                        // we can fail safely at compile time
                        let meta = _attr.parse_meta().unwrap();
                        let span = _attr.path.span().join(_attr.tokens.span()).unwrap();
                        match create_builder_annotated_fields_methods(
                            &name,
                            field_type,
                            meta.clone(),
                            span,
                        ) {
                            Ok(mut methods) => {
                                let builder_method = BuilderSetter::create(
                                    name_from_arg,
                                    field_type,
                                    FieldType::Single,
                                );
                                if let None = methods.iter().find(|m| m.name == builder_method.name)
                                {
                                    methods.push(builder_method);
                                }
                                let methods_tokens =
                                    methods.into_iter().map(|m| m.to_token_stream());
                                quote! {
                                    #(#methods_tokens)*
                                }
                            }
                            Err(e) => e.into_compile_error(),
                        }
                    }
                    None => BuilderSetter::create(name_from_arg, field_type, FieldType::Single)
                        .to_token_stream(),
                }
            });
            quote! {
                #(#setters)*
            }
        }
        Fields::Unnamed(_) | Fields::Unit => unimplemented!(),
    }
}

fn create_builder_annotated_fields_methods<'a>(
    field_name: &'a &&Ident,
    field_type: &'a Type,
    meta: Meta,
    span: Span,
) -> syn::Result<Vec<BuilderSetter<'a>>> {
    let mut methods: Vec<BuilderSetter> = vec![];

    match meta {
        Meta::List(ref meta_list) => {
            for nm in meta_list.nested.iter() {
                match nm {
                    NestedMeta::Meta(nm_meta) => match nm_meta {
                        Meta::NameValue(nv) => {
                            if nv.path.is_ident("each") {
                                match nv.lit.borrow() {
                                    Lit::Str(s) => match field_type {
                                        Type::Path(type_path) => {
                                            let sel = type_path.path.borrow();
                                            let first_segment = sel.segments.first().unwrap();
                                            match first_segment.arguments.borrow() {
                                                PathArguments::AngleBracketed(ab) => {
                                                    let ga = ab.args.first().unwrap();
                                                    match ga {
                                                        GenericArgument::Type(t) => {
                                                            let method = BuilderSetter::create(
                                                                format_ident!("{}", s.value()),
                                                                t,
                                                                FieldType::Repeated(
                                                                    field_name.to_string(),
                                                                ),
                                                            );
                                                            methods.push(method)
                                                        }
                                                        _ => unimplemented!(),
                                                    }
                                                }
                                                _ => unimplemented!(),
                                            }
                                        }
                                        _ => unimplemented!(),
                                    },
                                    _ => unimplemented!(),
                                }
                            } else {
                                return Err(Error::new(span, "expected `builder(each = \"...\")`"));
                            }
                        }
                        Meta::Path(_) | Meta::List(_) => unimplemented!(),
                    },
                    NestedMeta::Lit(_) => unimplemented!(),
                };
            }
        }
        Meta::NameValue(_) | Meta::Path(_) => unimplemented!(),
    }
    Ok(methods)
}

enum FieldType {
    // contains built struct's field name to be altered
    Repeated(String),
    Single,
}

struct BuilderSetter<'a> {
    name: Ident,
    argument_type: &'a Type,
    field_to_set_type: FieldType,
}

impl BuilderSetter<'_> {
    fn create(name: Ident, field_type: &Type, field_to_set_type: FieldType) -> BuilderSetter {
        match field_type {
            Type::Path(type_path) => {
                let option = type_path.path.segments.iter().find(|s| s.ident == "Option");
                match option {
                    Some(option) => {
                        match &option.arguments {
                            PathArguments::AngleBracketed(args) => {
                                // get Option<T> T and use as method argument type
                                match args.args.len() {
                                    1 => match field_to_set_type {
                                        FieldType::Repeated(ref name) => {
                                            let ident = format_ident!("{}", name);
                                            BuilderSetter {
                                                name: ident,
                                                argument_type: &field_type,
                                                field_to_set_type: FieldType::Repeated(
                                                    name.to_owned(),
                                                ),
                                            }
                                        }
                                        _ => BuilderSetter {
                                            name: format_ident!("{}", name),
                                            argument_type: &field_type,
                                            field_to_set_type: FieldType::Single,
                                        },
                                    },
                                    _ => unimplemented!(),
                                }
                            }
                            PathArguments::None | PathArguments::Parenthesized(_) => {
                                unimplemented!()
                            }
                        }
                    }
                    _ => match field_to_set_type {
                        FieldType::Repeated(ref array_name) => BuilderSetter {
                            name,
                            argument_type: &field_type,
                            field_to_set_type: FieldType::Repeated(array_name.to_owned()),
                        },
                        FieldType::Single => BuilderSetter {
                            name: format_ident!("{}", name),
                            argument_type: &field_type,
                            field_to_set_type: FieldType::Single,
                        },
                    },
                }
            }
            _ => BuilderSetter {
                name: format_ident!("{}", name),
                argument_type: &field_type,
                field_to_set_type: FieldType::Single,
            },
        }
    }

    fn prepare_field_set_flag_name(&self) -> Ident {
        match &self.field_to_set_type {
            FieldType::Repeated(name) => format_ident!("{}_set", name),
            _ => format_ident!("{}_set", self.name),
        }
    }

    fn find_option_path_segment(type_path: &TypePath) -> Option<&PathSegment> {
        type_path.path.segments.iter().find(|s| s.ident == "Option")
    }

    fn handle_option_type(&self, path_segment: &PathSegment) -> TokenStream {
        match &path_segment.arguments {
            PathArguments::AngleBracketed(generic_arguments) => {
                BuilderSetter::handle_option_type_generic_arguments(
                    generic_arguments,
                    &self.field_to_set_type,
                    &self.name,
                )
            }
            PathArguments::None | PathArguments::Parenthesized(_) => {
                unimplemented!()
            }
        }
    }

    fn handle_option_type_generic_arguments(
        generic_arguments: &AngleBracketedGenericArguments,
        field_to_set_type: &FieldType,
        name: &Ident,
    ) -> TokenStream {
        match generic_arguments.args.len() {
            1 => {
                let tokens = generic_arguments.args.first().unwrap().into_token_stream();
                match field_to_set_type {
                    FieldType::Repeated(ref name) => {
                        let ident = format_ident!("{}", name);
                        quote! {
                            fn #name(&mut self, #name: #tokens) -> &mut Self {
                                match self.#ident.push(#name);
                            }
                        }
                    }
                    _ => {
                        quote! {
                            fn #name(&mut self, #name: #tokens) -> &mut Self {
                                self.#name = Some(#name);
                                self
                            }
                        }
                    }
                }
            }
            _ => unimplemented!(),
        }
    }

    fn handle_rest_types(self, type_path: &TypePath) -> TokenStream {
        let field_set_flag_name = &self.prepare_field_set_flag_name();
        let tokens = type_path.into_token_stream();
        match self.field_to_set_type.borrow() {
            FieldType::Repeated(ref array_name) => {
                let ident = format_ident!("{}", array_name);
                let name = self.name;
                let stream = quote! {
                    fn #name(&mut self, #name: #tokens) -> &mut Self {
                        self.#ident.push(#name);
                        self.#field_set_flag_name = true;
                        self
                    }
                };
                stream
            }
            FieldType::Single => {
                let name = self.name;
                quote! {
                    fn #name(&mut self, #name: #tokens) -> &mut Self {
                        self.#name = #name;
                        self.#field_set_flag_name = true;
                        self
                    }
                }
            }
        }
    }

    pub fn to_token_stream(self) -> TokenStream {
        match &self.argument_type {
            Type::Path(type_path) => match BuilderSetter::find_option_path_segment(type_path) {
                Some(option) => self.handle_option_type(option),
                _ => self.handle_rest_types(type_path),
            },
            _ => {
                let field_set_flag_name = &self.prepare_field_set_flag_name();
                let name = self.name;
                let field_type = self.argument_type;
                quote! {
                    fn #name(&mut self, #name: #field_type) -> &mut Self {
                        self.#name = Some(#name);
                        self.#field_set_flag_name = true;
                        self
                    }
                }
            }
        }
    }
}
