use core::convert::TryInto;
use darling::{ast, util, FromDeriveInput, FromField};
use proc_macro::TokenStream;
use quote::quote;
use sha2::Digest;
use syn::{
    parse_macro_input,
    visit_mut::{self, VisitMut},
    DeriveInput, Ident, Item, Lit, LitBool, LitStr,
};

/// Shim for `min_const_generics` only supporting integer types
///
/// This macro creates an identifier for a generic prop name
/// that can be used with the `min_const_generics` feature
/// set.
///
/// Once `const_generics` supports `&'static str`, this will be a no-op.
#[proc_macro]
pub fn id(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as Ident);
    let result = hash(&input.to_string());
    quote!(#result).into()
}

/// Shim for `min_const_generics` only supporting integer types
///
/// This macro replaces any occurances of generic prop names with an
/// internal id that can be used with the `min_const_generics` feature
/// set.
///
/// Once `const_generics` supports `&'static str`, this will be a no-op.
#[proc_macro_attribute]
pub fn props(_attr: TokenStream, input: TokenStream) -> TokenStream {
    // TODO enable collision filtering
    let mut input = parse_macro_input!(input as Item);

    PropHasher.visit_item_mut(&mut input);

    quote!(#input).into()
}

#[derive(Debug, FromDeriveInput)]
#[darling(attributes(prop), supports(struct_named))]
struct Prop {
    ident: Ident,
    generics: syn::Generics,
    data: ast::Data<util::Ignored, PropField>,
}

#[derive(Debug, FromField)]
#[darling(attributes(prop))]
struct PropField {
    ident: Option<Ident>,
    vis: syn::Visibility,
    ty: syn::Type,
    #[darling(default)]
    get: PropValue,
    #[darling(default)]
    try_get: PropValue,
    #[darling(default)]
    set: PropValue,
    #[darling(default)]
    try_set: PropValue,
    #[darling(default)]
    skip: bool,
    #[darling(default)]
    error: Option<syn::Path>,
}

#[derive(Debug)]
enum PropValue {
    Disabled,
    Enabled,
    Str(LitStr),
    Bool(LitBool),
}

impl Default for PropValue {
    fn default() -> Self {
        Self::Disabled
    }
}

impl darling::FromMeta for PropValue {
    fn from_word() -> darling::Result<Self> {
        Ok(Self::Enabled)
    }

    fn from_value(value: &Lit) -> darling::Result<Self> {
        (match value {
            Lit::Bool(b) => Ok(Self::Bool(b.clone())),
            Lit::Str(b) => Ok(Self::Str(b.clone())),
            _ => Err(darling::Error::unexpected_lit_type(value)),
        })
        .map_err(|e| e.with_span(value))
    }
}

impl PropValue {
    fn is_enabled(&self, default: bool) -> bool {
        match self {
            Self::Enabled | Self::Str(_) => true,
            Self::Bool(v) => v.value,
            Self::Disabled => default,
        }
    }
}

impl PropField {
    fn get(&self) -> bool {
        !self.skip && self.get.is_enabled(self.is_pub())
    }

    fn try_get(&self) -> bool {
        !self.skip && self.try_get.is_enabled(self.get())
    }

    fn set(&self) -> bool {
        !self.skip && self.set.is_enabled(self.is_pub())
    }

    fn try_set(&self) -> bool {
        !self.skip && self.try_set.is_enabled(self.set())
    }

    fn is_pub(&self) -> bool {
        matches!(self.vis, syn::Visibility::Public(_))
    }
}

macro_rules! unwrap {
    ($expr:expr, $fmt:expr) => {
        match $expr {
            Ok(v) => v,
            Err(err) => return $fmt(err),
        }
    };
}

fn to_compile_error(err: syn::Error) -> TokenStream {
    err.to_compile_error().into()
}

#[proc_macro_derive(Prop, attributes(prop))]
pub fn derive_prop(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let input = unwrap!(Prop::from_derive_input(&input), |err: darling::Error| err
        .write_errors()
        .into());

    let mut tokens = quote!();

    let s_name = input.ident;
    let fields = input.data.take_struct().unwrap().fields;
    let (impl_generics, type_generics, where_generics) = input.generics.split_for_impl();

    for field in fields.iter() {
        let name = if let Some(n) = field.ident.as_ref() {
            n
        } else {
            continue;
        };

        let id = hash(&name.to_string());
        let ty = &field.ty;

        let get_error = || {
            if let Some(e) = field.error.as_ref() {
                Ok(quote!(#e))
            } else {
                Err(syn::Error::new(
                    name.span(),
                    "Missing `#[prop(error = MyError)]`",
                ))
            }
        };

        if field.get() {
            let getter = if let PropValue::Str(s) = &field.get {
                let path: syn::Path = unwrap!(syn::parse_str(&s.value()), |err: syn::Error| err
                    .to_compile_error()
                    .into());
                quote!(#path(self))
            } else {
                quote!(&self.#name)
            };

            tokens.extend(quote!(impl #impl_generics prop::Get<#ty, #id> for #s_name #type_generics #where_generics {
                fn get_value(&self) -> &#ty {
                    #getter
                }
            }
            ));
        }

        if field.set() {
            let setter = if let PropValue::Str(s) = &field.set {
                let path: syn::Path = unwrap!(syn::parse_str(&s.value()), |err: syn::Error| err
                    .to_compile_error()
                    .into());
                quote!(#path(self, value))
            } else {
                quote!(self.#name = value)
            };

            tokens.extend(quote!(impl #impl_generics prop::Set<#ty, #id> for #s_name #type_generics #where_generics {
                fn set_value(&mut self, value: #ty) {
                    #setter
                }
            }
            ));
        }

        if field.try_get() {
            let (getter, error) = if let PropValue::Str(s) = &field.try_get {
                let path: syn::Path = unwrap!(syn::parse_str(&s.value()), |err: syn::Error| err
                    .to_compile_error()
                    .into());
                let v = quote!(#path(self));
                let err = unwrap!(get_error(), to_compile_error);
                (v, err)
            } else {
                let v = quote!(Ok(<Self as prop::Get<#ty, #id>>::get_value(self)));
                let err = quote!(::core::convert::Infallible);
                (v, err)
            };

            tokens.extend(quote!(impl #impl_generics prop::TryGet<#ty, #error, #id> for #s_name #type_generics #where_generics {
                fn try_get_value(&self) -> ::core::result::Result<&#ty, #error> {
                    #getter
                }
            }));
        }

        if field.try_set() {
            let (setter, error) = if let PropValue::Str(s) = &field.try_set {
                let path: syn::Path = unwrap!(syn::parse_str(&s.value()), |err: syn::Error| err
                    .to_compile_error()
                    .into());
                let v = quote!(#path(self, value));
                let err = unwrap!(get_error(), to_compile_error);
                (v, err)
            } else {
                let v = quote!(
                    <Self as prop::Set<#ty, #id>>::set_value(self, value);
                    ::core::result::Result::Ok(())
                );
                let err = quote!(::core::convert::Infallible);
                (v, err)
            };

            tokens.extend(quote!(impl #impl_generics prop::TrySet<#ty, #error, #id> for #s_name #type_generics #where_generics {
                fn try_set_value(&mut self, value: #ty) -> ::core::result::Result<(), #error> {
                    #setter
                }
            }));
        }
    }

    tokens.into()
}

fn hash(name: &str) -> u128 {
    let mut hasher = sha2::Sha256::new();
    hasher.update(name);
    let result = hasher.finalize();
    let result = result[..16].try_into().unwrap();
    u128::from_le_bytes(result)
}

struct PropHasher;

impl VisitMut for PropHasher {
    fn visit_path_mut(&mut self, node: &mut syn::Path) {
        if let Some(first) = node.segments.first() {
            match first.ident.to_string().as_str() {
                "Prop" | "Set" | "Get" | "GetMut" => {}
                _ => return,
            }
        }

        visit_mut::visit_path_mut(self, node)
    }

    fn visit_generic_argument_mut(&mut self, node: &mut syn::GenericArgument) {
        if let syn::GenericArgument::Const(lit) = node {
            if let syn::Expr::Lit(lit) = lit {
                if let syn::Lit::Str(s) = &mut lit.lit {
                    let hash = hash(&s.value());
                    *node = syn::parse2(quote!(#hash)).unwrap();
                }
            }
        }
    }
}
