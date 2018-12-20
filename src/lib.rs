extern crate proc_macro;
#[macro_use]
extern crate syn;
#[macro_use]
extern crate quote;

use syn::DeriveInput;
use proc_macro::TokenStream;

#[proc_macro_derive(ToJs)]
pub fn to_js_derive(input: TokenStream) -> TokenStream {
    let input: DeriveInput = parse_macro_input!(input as DeriveInput);
    let name = &input.ident;

    let expanded = quote! {
        impl<'e> ToJs<'e> for #name {
            type Value = JsObject<'e>;
            fn to_js(self, env: Env) -> Result<JsObject<'e>> {
                Ok(JsObject::from(serialize_to_js(env, &self).unwrap()))
            }
        }
    };

    TokenStream::from(expanded)
}

#[proc_macro_derive(FromArguments)]
pub fn from_args_derive(input: TokenStream) -> TokenStream {
    let input: DeriveInput = parse_macro_input!(input as DeriveInput);
    let name = &input.ident;

    let expanded = quote! {
        impl FromArguments for #name {
            fn from_args(args: &Arguments) -> Result<Self> {
                match args.next_arg() {
                    Some(any) => {
                        pinar_serde::de::from_any(args.env(), any)
                            .map_err(|e| {
                                ArgumentsError::Deserialization(format!("{}", e)).into()
                            })
                    },
                    _ => Err(ArgumentsError::missing(args.arg_number()))
                }
            }
        }
    };

    TokenStream::from(expanded)
}
