extern crate proc_macro;
#[macro_use]
extern crate syn;
#[macro_use]
extern crate quote;

use quote::{quote};

use syn::{DeriveInput, ItemImpl, ImplItem, ImplItemMethod, PathSegment, TypePath, Item, Lifetime, Ident};
use proc_macro::TokenStream;

trait ToHelperName {
    fn to_helper_name(&self) -> Ident;
}

const HELPER_PREFIX: &str = "__pinar_helper_";

fn to_helper_name(name: &str) -> Ident {
    format_ident!("{}{}", HELPER_PREFIX, name.to_lowercase())
}

impl ToHelperName for Ident {
    fn to_helper_name(&self) -> Ident {
        to_helper_name(&self.to_string())
    }
}

impl ToHelperName for String {
    fn to_helper_name(&self) -> Ident {
        to_helper_name(&self)
    }
}

trait ElideOutputLifetime {
    fn elide_output_lifetime(&mut self);
}

impl ElideOutputLifetime for ImplItemMethod {
    fn elide_output_lifetime(&mut self) {
        let ImplItemMethod {
            attrs,
            sig: syn::Signature {
                constness,
                asyncness,
                generics,
                inputs,
                output,
                unsafety,
                ident,
                ..
            },
            block,
            ..
        } = self;

        *self = parse_quote! {
            #(#attrs)*
            #constness #asyncness #unsafety fn #ident #generics ( #inputs , __pinar__env__: Env ) -> JsResult<Option<Value>> {
                let fun = move || #output {
                    #block
                };
                fun().get_result(__pinar__env__)
            }
        };
    }
}

#[proc_macro_derive(Pinar)]
pub fn pinar_derive(input: TokenStream) -> TokenStream {
    let input: DeriveInput = parse_macro_input!(input as DeriveInput);
    let name = &input.ident;

    let expanded = quote! {
        impl<'e> ToJs<'e> for #name {
            type Value = JsObject<'e>;
            fn to_js(&self, env: Env) -> JsResult<JsObject<'e>> {
                Ok(JsObject::from(serialize_to_js(env, &self).unwrap()))
            }
        }

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

        impl<'e> ToRust<#name> for JsObject<'e>
        {
            fn to_rust(&self) -> JsResult<#name> {
                pinar_serde::de::from_value(self.env(), self.get_value())
                    .map_err(|e| {
                        ArgumentsError::Deserialization(format!("{}", e)).into()
                    })
            }
        }
    };

    TokenStream::from(expanded)
}


use syn::visit_mut::{VisitMut};
use syn::{ItemFn, FnArg, PatType, ReturnType, Pat, Type, Attribute, Signature};

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

// #[derive(Debug)]
struct MethodDetail {
    name: String,
    helper_name: Option<String>,
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
            helper_name: None,
            node,
            with_self: false,
            self_mutable: false,
            return_ref: false,
            kind: Default::default()
        }
    }
}

impl std::fmt::Debug for MethodDetail {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        f.debug_struct("MethodKind")
            .field("name", &self.name)
            .field("with_self", &self.with_self)
            .field("self_mutable", &self.self_mutable)
            .field("return_ref", &self.return_ref)
            .field("kind", &self.kind)
            .finish()
    }
}

#[derive(Default, Debug)]
struct ExportVisitor {
    methods: Vec<MethodDetail>,
    helpers: Vec<(Ident, ImplItemMethod, bool)>
}

impl ExportVisitor {
    fn new(len: usize) -> ExportVisitor {
        ExportVisitor {
            methods: Vec::with_capacity(len),
            helpers: vec![]
        }
    }

    fn add_helpers(&self, input: &mut ItemImpl) {
        for (name, item, return_ref) in &self.helpers {

            let ImplItemMethod {
                attrs,
                sig: syn::Signature {
                    constness,
                    asyncness,
                    generics,
                    inputs,
                    unsafety,
                    ..
                },
                ..
            } = item;

            let name_helper = name.to_helper_name();

            let pats: Vec<_> = extract_args_pat(inputs).into_iter().map(|mut pat| {
                if let Pat::Ident(ref mut ident) = *pat {
                    ident.mutability = None;
                };
                pat
            }).collect();

//            println!("PATS: {:?}", pats);

            let inputs: Vec<&PatType> = inputs.iter().filter_map(|input| {
                if let FnArg::Typed(t) = input { Some(t) } else { None }
            }).collect();

            let fun = match return_ref {
                true => format_ident!("get_result_from_ref"),
                _ => format_ident!("get_result")
            };

            let a: ImplItemMethod = parse_quote! {
                #(#attrs)*
                #constness #asyncness #unsafety fn #name_helper #generics ( &mut self, #(#inputs,)* __pinar__env__: Env ) -> JsResult<Option<Value>> {
                    self.#name( #(#pats,)* )
                        .#fun(__pinar__env__)
                }
            };

            input.items.push(ImplItem::Method(a));
        }
    }
}

use syn::punctuated::{Punctuated};
use syn::token::{Colon2, Comma};

fn get_method_kind_helper(punc: &Punctuated<PathSegment, Colon2>) -> Option<MethodKind> {
    if !punc.is_empty() {
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



fn get_lifetimes_inner(ty: &syn::Type, lifetimes: &mut Vec<Option<Lifetime>>) {
    use syn::Type::*;
    use syn::PathArguments::*;
    use syn::GenericArgument as GA;

    match ty {
        Array(ty) => { get_lifetimes_inner(&ty.elem, lifetimes); }
        Group(ty) => { get_lifetimes_inner(&ty.elem, lifetimes); }
        Paren(ty) => { get_lifetimes_inner(&ty.elem, lifetimes); }
        Slice(ty) => { get_lifetimes_inner(&ty.elem, lifetimes); }
        Reference(ty) => {
            lifetimes.push(ty.lifetime.clone());
            get_lifetimes_inner(&ty.elem, lifetimes);
        }
        Tuple(ty) => {
            for elem in &ty.elems {
                get_lifetimes_inner(elem, lifetimes);
            }
        }
        Path(ty) => {
            for seg in ty.path.segments.iter() {
                match &seg.arguments {
                    AngleBracketed(ab) => {
                        for arg in ab.args.iter() {
                            match arg {
                                GA::Lifetime(x) => { lifetimes.push(Some(x.clone())); }
                                GA::Type(x) => { get_lifetimes_inner(x, lifetimes); }
                                GA::Binding(x) => { get_lifetimes_inner(&x.ty, lifetimes); }
                                GA::Constraint(_x) => { unimplemented!() }
                                GA::Const(_) => { unimplemented!() }
                            }
                        }
                    }
                    Parenthesized(_p) => { unimplemented!() }
                    None => {}
                }
            }
        }
        // BareFn(TypeBareFn) => {}
        // ImplTrait(TypeImplTrait) => {}
        // Infer(TypeInfer) => {}
        // Macro(TypeMacro) => {}
        // Never(TypeNever) => {}
        // Ptr(TypePtr) => {}
        // Verbatim(TokenStream) => {}
        TraitObject(_type_trait_object) => { unimplemented!() }
        _ => {}
    };
}

fn get_lifetimes(output: &ReturnType) -> Option<Vec<String>> {


    let mut vec = vec![];

    match output {
        ReturnType::Default => None,
        ReturnType::Type(_, ty) => {
            get_lifetimes_inner(&ty, &mut vec);
            if vec.is_empty() {
                None
            } else {
                Some(vec.into_iter().map(|lifetime| {
                    lifetime.map(|v| format!("{}", v))
                            .unwrap_or_else(String::new)
                }).collect())
            }
        }
    }
}

fn get_lifetimes_input(sig: &Signature) -> String {
    use syn::FnArg::*;

    for input in &sig.inputs {
        match input {
            Receiver(r) => {
                if let Some((_, Some(ref r))) = r.reference {
                    return format!("{}", r);
                };
            }
            _ => {}
        }
    }
    String::new()
}

fn is_correct_lifetime(sig: &Signature, detail: &mut MethodDetail) -> bool {
    // println!("LIFETIMES: {} {:#?}", name, get_lifetimes(&item.sig.output));
    // println!("LIFETIMES INPUTS: {} {:#?}", name, get_lifetimes_input(&item.sig));

    let _name = sig.ident.to_string();

    let lifetimes = get_lifetimes(&sig.output);
    let lifetime_self = get_lifetimes_input(&sig);

    // println!("LIFETIMES: {} {:#?}", name, lifetimes);
    // println!("LIFETIMES INPUTS: {} {:#?}", name, lifetime_self);

    match lifetimes {
        Some(ref lifetimes) => {
            detail.return_ref = lifetimes.iter().all(|lifetime| {
                lifetime.as_str() == lifetime_self.as_str()
            });

            lifetimes.iter().all(|lifetime| {
                lifetime.as_str() == lifetime_self.as_str() || lifetime == "'static"
            })
        }
        _ => lifetime_self.is_empty()
    }
}

impl VisitMut for ExportVisitor {
    fn visit_impl_item_method_mut(&mut self, item: &mut ImplItemMethod) {
        let name = item.sig.ident.to_string();

        let mut detail = MethodDetail::new(name.clone(), item.clone());

        if let Some(FnArg::Receiver(receiver)) = item.sig.inputs.first() {
            detail.with_self = true;
            if receiver.mutability.is_some() {
                detail.self_mutable = true;
            }
        };

        // if let ReturnType::Type(_, ret) = &item.sig.output {
        //     if let Type::Reference(_) = &**ret {
        //         detail.return_ref = true;
        //     };
        // };

        detail.kind = if name == "constructor" {
            MethodKind::Constructor
        } else {
            get_method_kind(&item.attrs)
        };

        //if detail.kind != MethodKind::Constructor {
        if name != "constructor" {
            let _correct = is_correct_lifetime(&item.sig, &mut detail);
            //println!("CORRECT: {} {:#?}", name, correct);
        }

        if detail.kind != MethodKind::Constructor && (detail.return_ref || !detail.self_mutable) {
//            self.helpers.push((item.sig.ident.clone(), item.clone()));
            self.helpers.push((format_ident!("{}", name.to_lowercase()), item.clone(), detail.return_ref));
            detail.helper_name = Some(name.to_helper_name().to_string());
        }

        item.sig.ident = format_ident!("{}", name.to_lowercase());

        if detail.kind == MethodKind::Constructor {
            *item = parse_quote!( fn _dummy_(&self) {} );
        }

        self.methods.push(detail);
    }
}

fn extract_args_ty(inputs: &Punctuated<FnArg, Comma>) -> Vec<Box<Type>> {
    let mut vec = vec![];
    for input in inputs {
        match input {
            FnArg::Receiver(_r) => {
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
            FnArg::Receiver(_r) => {
                //unreachable!()
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

// #[proc_macro_attribute]
// pub fn exportfn(attr: TokenStream, item: TokenStream) -> TokenStream {
//     let input: ItemFn = parse_macro_input!(item as ItemFn);

//     let name = &input.sig.ident;
//     let name_str = format!("{}", &input.sig.ident);

//     let ptr_init_fn = format_ident!("__PINAR_FN_{}", name);
//     let init_fn = format_ident!("__pinar_init_{}", name);

//     // TODO: Check input
//     // No self in params

//     (quote! {
//         #[distributed_slice(PINAR_FUNCTIONS)]
//         static #ptr_init_fn: fn(&mut ModuleBuilder) = #init_fn;

//         fn #init_fn(builder: &mut ModuleBuilder) {
//             builder.with_function(#name_str, #name);
//         }

//         #input
//     }).into()
// }

fn export_fn(input: &ItemFn) -> TokenStream {
    let name = &input.sig.ident;
    let name_str = format!("{}", &input.sig.ident);

    let init_fn = format_ident!("__PINAR_FN_{}", name.to_string().to_uppercase());

    // TODO: Check input
    // No self in params

    (quote! {
        #[distributed_slice(PINAR_FUNCTIONS)]
        static #init_fn: fn(&mut ModuleBuilder) = {
            fn init(builder: &mut ModuleBuilder) {
                builder.with_function(#name_str, #name);
            }
            init
        };

        #input
    }).into()
}

use syn::export::TokenStream2;

fn make_constructor(
    methods: &[MethodDetail]
) -> (TokenStream2, TokenStream2, TokenStream2)
{
    if let Some(cons) = methods.iter().find(|m| m.kind == MethodKind::Constructor) {
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
        let constructor = quote!(
            #(#attrs)*
            fn constructor #generics( ( #args_pat ) : Self::ArgsConstructor ) #output #block
        );

        (constructor, args_ty, args_pat)
    } else {
        (quote!(), quote!(), quote!())
    }
}

fn process_class(input: &mut ItemImpl) -> TokenStream {
    let name = match &*input.self_ty {
        Type::Path(TypePath { path, .. }) => path.get_ident().unwrap().clone(),
        _ => unreachable!()
    };

    let mut visitor = ExportVisitor::new(input.items.len());

    visitor.visit_item_impl_mut(input);
    visitor.add_helpers(input);

    // println!("RESULT: {:#?}", visitor);

    let name_str = name.to_string();

    let normals = visitor.methods
                         .iter()
                         .filter(|m| m.kind == MethodKind::Normal)
                         .collect::<Vec<_>>();

    let idents_str = normals.iter().map(|m| m.name.as_str());
    let idents = normals.iter().map(|m| format_ident!("{}", m.helper_name.as_ref().unwrap_or(&m.name).to_lowercase()));

    // let idents_str = normals.iter().map(|m| m.name.as_str());
    // let idents = normals.iter().map(|m| format_ident!("{}", &m.name));

    let (constructor, args, _pats) = make_constructor(&visitor.methods);

    let init_fn = format_ident!("__PINAR_FN_{}", name.to_string().to_uppercase());

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
        static #init_fn: fn(&mut ModuleBuilder) = {
            fn init(builder: &mut ModuleBuilder) {
                builder.with_class::<#name>();
            }
            init
        };

        #input
    }).into()
}

fn process(_attr: (), input: &mut Item) -> Result<TokenStream, SetGetError> {
    match input {
        syn::Item::Fn(fun) => {
            Ok(export_fn(fun))
        }
        syn::Item::Impl(ref mut class) => {
            Ok(process_class(class))
        }
        _ => {
            unreachable!();
            // Unsuported
        }
    }
}

#[proc_macro_attribute]
pub fn pinar(_attr: TokenStream, input: TokenStream) -> TokenStream {
    let mut input: Item = parse_macro_input!(input as Item);

    match process((), &mut input) {
        Ok(input) => input,
        Err(_e) => unimplemented!()
    }
}

// #[proc_macro_attribute]
// pub fn export(attr: TokenStream, item: TokenStream) -> TokenStream {
//     let item_clone = item.clone();
//     let input: ItemImpl = parse_macro_input!(item_clone as ItemImpl);

//     let name = match &*input.self_ty {
//         Type::Path(TypePath { path, .. }) => path.get_ident().unwrap(),
//         _ => unreachable!()
//     };

//     let mut visitor = ExportVisitor::new(input.items.len());

//     // visitor.visit_item_impl(&input);

//     // println!("RESULT: {:#?}", visitor);

//     let name_str = name.to_string();

//     let normals = visitor.methods.iter().filter(|m| m.kind == MethodKind::Normal).collect::<Vec<_>>();
//     let idents_str = normals.iter().map(|m| m.name.as_str());
//     let idents = normals.iter().map(|m| format_ident!("{}", &m.name));

//     let (constructor, args, pats) = if let Some(cons) = visitor.methods.iter().find(|m| m.kind == MethodKind::Constructor) {
//         let node = &cons.node;

//         let ImplItemMethod {
//             attrs,
//             sig: syn::Signature {
//                 generics,
//                 inputs,
//                 output,
//                 ..
//             },
//             block,
//             ..
//         } = node;

//         let attrs = filter_constructor(attrs.to_vec());
//         let args_ty = extract_args_ty(inputs);
//         let args_pat = extract_args_pat(inputs);

//         let args_ty = quote!( #(#args_ty),* );
//         let args_pat = quote!( #(#args_pat),* );
//         let output = quote!(
//             #(#attrs)*
//             fn constructor #generics( ( #args_pat ) : Self::ArgsConstructor ) #output #block
//         );

//         (output, args_ty, args_pat)
//     } else {
//         (quote!(), quote!(), quote!())
//     };

//     // let init_fn = format_ident!("__pinar_init_{}", name);
//     //let init_fn_ptr = format_ident!("__PINAR_FN_{}", name);
//     let init_fn_ptr = format_ident!("__PINAR_FN_{}", name.to_string().to_uppercase());

//     (quote! {
//         impl JsClass for #name {
//             const CLASSNAME: &'static str = #name_str;
//             type ArgsConstructor = ( #args );

//             #constructor

//             fn default_properties(builder: ClassBuilder<Self>) -> ClassBuilder<Self> {
//                 builder
//                     #( .with_method(#idents_str, Self::#idents) )*
//             }
//         }

//         #[distributed_slice(PINAR_CLASSES)]
//         static #init_fn_ptr: fn(&mut ModuleBuilder) = {
//             fn init(builder: &mut ModuleBuilder) {
//                 builder.with_class::<#name>();
//             }
//             init
//         };

//         // #[distributed_slice(PINAR_CLASSES)]
//         // static #init_fn_ptr: fn(&mut ModuleBuilder) = #init_fn;

//         // fn #init_fn(builder: &mut ModuleBuilder) {
//         //     builder.with_class::<#name>();
//         // }

//         #input
//     }).into()
// }

//mod helper;

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
            if inputs.is_empty() {
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
    let input: ImplItemMethod = parse_macro_input!(item_clone as ImplItemMethod);

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
pub fn constructor(_attr: TokenStream, _item: TokenStream) -> TokenStream {
    TokenStream::new()
}
