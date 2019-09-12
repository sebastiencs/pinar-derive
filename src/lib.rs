extern crate proc_macro;
#[macro_use]
extern crate syn;
#[macro_use]
extern crate quote;

use quote::quote;

use syn::{DeriveInput, ItemImpl, ImplItem, ImplItemMethod, PathSegment, TypePath};
use proc_macro::TokenStream;

#[proc_macro_derive(ToJs)]
pub fn to_js_derive(input: TokenStream) -> TokenStream {
    let input: DeriveInput = parse_macro_input!(input as DeriveInput);
    let name = &input.ident;

    let expanded = quote! {
        impl<'e> ToJs<'e> for #name {
            type Value = JsObject<'e>;
            fn to_js(self, env: Env) -> JsResult<JsObject<'e>> {
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
            fn from_args(args: &Arguments) -> JsResult<Self> {
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

use syn::visit::{self, Visit};
use syn::{File, ItemFn, Receiver, FnArg, ReturnType, Pat, Type, Attribute, Signature};

#[derive(Debug, PartialEq)]
enum MethodKind {
    Normal,
    Setter,
    Getter,
    Constructor
}

impl Default for MethodKind {
    fn default() -> Self { Self::Normal }
}

#[derive(Debug)]
struct MethodDetail {
    name: String,
    with_self: bool,
    self_mutable: bool,
    return_ref: bool,
    kind: MethodKind,
    node: ImplItemMethod
}

impl MethodDetail {
    fn new(name: String, node: ImplItemMethod) -> MethodDetail {
        MethodDetail {
            name,
            node,
            with_self: false,
            self_mutable: false,
            return_ref: false,
            kind: Default::default()
        }
    }
}

#[derive(Default, Debug)]
struct ExportVisitor {
    methods: Vec<MethodDetail>
}

impl ExportVisitor {
    fn new(len: usize) -> ExportVisitor {
        ExportVisitor {
            methods: Vec::with_capacity(len)
        }
    }
}

use syn::punctuated::{Iter, Punctuated};
use syn::token::{Colon2, Comma};

fn get_method_kind_helper(punc: &Punctuated<PathSegment, Colon2>) -> Option<MethodKind> {
    if punc.len() >= 1 {
        let first = punc.first().unwrap();
        let last = punc.last().unwrap();

        if punc.len() == 1 || first.ident == "pinar" {
            if last.ident == "setter" {
                return Some(MethodKind::Setter);
            } else if last.ident == "getter" {
                return Some(MethodKind::Getter);
            } else if last.ident == "constructor" {
                return Some(MethodKind::Constructor);
            }
        }
    }
    None
}

fn get_method_kind(attrs: &[Attribute]) -> MethodKind {
    for attr in attrs {
        if let Some(kind) = get_method_kind_helper(&attr.path.segments) {
            return kind;
        };
    }
    MethodKind::Normal
}

impl<'ast> Visit<'ast> for ExportVisitor {
    fn visit_impl_item_method(&mut self, item: &'ast ImplItemMethod) {
        let name = item.sig.ident.to_string();

        let mut detail = MethodDetail::new(name, item.clone());

        if let Some(FnArg::Receiver(receiver)) = item.sig.inputs.first() {
            detail.with_self = true;
            if receiver.mutability.is_some() {
                detail.self_mutable = true;
            }
        };

        if let ReturnType::Type(_, ret) = &item.sig.output {
            if let Type::Reference(_) = &**ret {
                detail.return_ref = true;
            };
        };

        if !item.attrs.is_empty() {
            detail.kind = get_method_kind(&item.attrs);
        }

        self.methods.push(detail);
    }
}

fn extract_args_ty(inputs: &Punctuated<FnArg, Comma>) -> Vec<Box<Type>> {
    let mut vec = vec![];
    for input in inputs {
        match input {
            FnArg::Receiver(r) => {
                unreachable!()
            },
            FnArg::Typed(arg) => vec.push(arg.ty.clone())
        }
    }
    vec
}

fn extract_args_pat(inputs: &Punctuated<FnArg, Comma>) -> Vec<Box<Pat>> {
    let mut vec = vec![];
    for input in inputs {
        match input {
            FnArg::Receiver(r) => {
                unreachable!()
            },
            FnArg::Typed(arg) => vec.push(arg.pat.clone())
        }
    }
    vec
}

fn filter_constructor(attrs: Vec<Attribute>) -> Vec<Attribute> {
    attrs.into_iter().filter(|attr| {
        !(get_method_kind_helper(&attr.path.segments) == Some(MethodKind::Constructor))
    }).collect()
}

#[proc_macro_attribute]
pub fn exportfn(attr: TokenStream, item: TokenStream) -> TokenStream {
    let input: ItemFn = parse_macro_input!(item as ItemFn);

    let name = &input.sig.ident;
    let name_str = format!("{}", &input.sig.ident);

    let ptr_init_fn = format_ident!("__PINAR_FN_{}", name);
    let init_fn = format_ident!("__pinar_init_{}", name);

    // TODO: Check input
    // No self in params

    (quote! {
        #[distributed_slice(PINAR_FUNCTIONS)]
        static #ptr_init_fn: fn(&mut ModuleBuilder) = #init_fn;

        fn #init_fn(builder: &mut ModuleBuilder) {
            builder.with_function(#name_str, #name);
        }

        #input
    }).into()
}

#[proc_macro_attribute]
pub fn export(attr: TokenStream, item: TokenStream) -> TokenStream {
    let item_clone = item.clone();
    let input: ItemImpl = parse_macro_input!(item_clone as ItemImpl);

    let name = match &*input.self_ty {
        Type::Path(TypePath { path, .. }) => path.get_ident().unwrap(),
        _ => unreachable!()
    };

    let mut visitor = ExportVisitor::new(input.items.len());

    visitor.visit_item_impl(&input);

    // println!("RESULT: {:#?}", visitor);

    let name_str = name.to_string();

    let normals = visitor.methods.iter().filter(|m| m.kind == MethodKind::Normal).collect::<Vec<_>>();
    let idents_str = normals.iter().map(|m| m.name.as_str());
    let idents = normals.iter().map(|m| format_ident!("{}", &m.name));

    let (constructor, args, pats) = if let Some(cons) = visitor.methods.iter().find(|m| m.kind == MethodKind::Constructor) {
        let node = &cons.node;

        let ImplItemMethod {
            attrs,
            sig: syn::Signature {
                generics,
                inputs,
                output,
                ..
            },
            block,
            ..
        } = node;

        let attrs = filter_constructor(attrs.to_vec());
        let args_ty = extract_args_ty(inputs);
        let args_pat = extract_args_pat(inputs);

        let args_ty = quote!( #(#args_ty),* );
        let args_pat = quote!( #(#args_pat),* );
        let output = quote!(
            #(#attrs)*
            fn constructor #generics( ( #args_pat ) : Self::ArgsConstructor ) #output #block
        );

        (output, args_ty, args_pat)
    } else {
        (quote!(), quote!(), quote!())
    };

    let init_fn = format_ident!("__pinar_init_{}", name);
    let init_fn_ptr = format_ident!("__PINAR_FN_{}", name);

    (quote! {
        impl JsClass for #name {
            const CLASSNAME: &'static str = #name_str;
            type ArgsConstructor = ( #args );

            #constructor

            fn default_properties(builder: ClassBuilder<Self>) -> ClassBuilder<Self> {
                builder
                    #( .with_method(#idents_str, Self::#idents) )*
            }
        }

        #[distributed_slice(PINAR_CLASSES)]
        static #init_fn_ptr: fn(&mut ModuleBuilder) = #init_fn;

        fn #init_fn(builder: &mut ModuleBuilder) {
            builder.with_class::<#name>();
        }

        #input
    }).into()
}

mod helper;

#[derive(Debug)]
enum SetGetError {
    Setter,
    SetterMutability,
    Getter,
    GetterMutability,
    GetterReturn(Span)
}

impl Into<&'static str> for SetGetError {
    fn into(self) -> &'static str {
        use SetGetError::*;
        match self {
            Setter => "setters must receive 2 arguments: (&mut self, ..)",
            SetterMutability => "setters must receive a mutable self: (&mut self, ..)",
            Getter => "getters must receive 1 argument: (&self, ..)",
            GetterMutability => "getters must receive an immutable self: (&self, ..)",
            GetterReturn(_) => "getters must return a value"
        }
    }
}

fn check_inout(kind: MethodKind, sig: &Signature) -> Option<SetGetError> {
    use MethodKind::*;

    let inputs = &sig.inputs;

    match kind {
        Setter => {
            if inputs.len() < 2 {
                return Some(SetGetError::Setter)
            }
            if let Some(FnArg::Receiver(receiver)) = inputs.first() {
                if receiver.mutability.is_some() {
                    return None;
                }
            }
            return Some(SetGetError::SetterMutability);
        },
        Getter => {
            if inputs.len() < 1 {
                return Some(SetGetError::Getter)
            }

            if let ReturnType::Default = &sig.output {
                return Some(SetGetError::GetterReturn(sig.span()));
            };

            if let ReturnType::Type(_, ret) = &sig.output {
                if let Type::Tuple(tuple) = &**ret {
                    if tuple.elems.is_empty() {
                        return Some(SetGetError::GetterReturn(ret.span()));
                    }
                };
            };

            if let Some(FnArg::Receiver(receiver)) = inputs.first() {
                if receiver.mutability.is_some() {
                    return Some(SetGetError::GetterMutability)
                }
            }
        },
        _ => {},
    }

    None
}

use syn::export::Span;

use syn::spanned::Spanned;

fn check_setget(kind: MethodKind, item: TokenStream) -> TokenStream {
    let item_clone = item.clone();
    let mut input: ImplItemMethod = parse_macro_input!(item_clone as ImplItemMethod);

    let result = check_inout(kind, &input.sig);

    let span = match result {
        Some(SetGetError::GetterReturn(span)) => span,
        _ => input.sig.paren_token.span
    };

    if let Some(err) = result {
        return syn::Error::new::<&str>(span, err.into()).to_compile_error().into();
    };

    item
}

#[proc_macro_attribute]
pub fn setter(_attr: TokenStream, item: TokenStream) -> TokenStream {
    check_setget(MethodKind::Setter, item)
}

#[proc_macro_attribute]
pub fn getter(_attr: TokenStream, item: TokenStream) -> TokenStream {
    check_setget(MethodKind::Getter, item)
}

#[proc_macro_attribute]
pub fn constructor(_attr: TokenStream, item: TokenStream) -> TokenStream {
    TokenStream::new()
}
