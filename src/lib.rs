#![feature(proc_macro_span)]

extern crate proc_macro;

use darling::FromMeta;
use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use quote::quote;
use syn::export::ToTokens;
use syn::{parse_macro_input, AttributeArgs};

#[derive(Debug, FromMeta)]
struct AutologArgs {
    #[darling(default)]
    flame: bool,
    #[darling(multiple)]
    blacklist: Vec<String>,
}

#[proc_macro_attribute]
pub fn autolog(args: TokenStream, input: TokenStream) -> TokenStream {
    let attr_args = parse_macro_input!(args as AttributeArgs);

    let args = match AutologArgs::from_list(&attr_args) {
        Ok(v) => v,
        Err(e) => return e.write_errors().into(),
    };

    let module = parse_macro_input!(input as syn::ItemMod);

    let mut walker = ASTWalker::new(args);

    let output = walker.run(&module);
    TokenStream::from(quote!(#output))
}

struct ASTLocationDescriptor {
    pub span: proc_macro2::Span,
    pub fn_desc: Option<FunctionDescriptor>,
}

struct FunctionDescriptor {
    pub ident: proc_macro2::Ident,
    pub decl: syn::FnDecl,
}

struct ASTWalker {
    config: AutologArgs,
    loc_stack: Vec<ASTLocationDescriptor>,
}

impl ASTWalker {
    fn new(config: AutologArgs) -> Self {
        ASTWalker {
            config,
            loc_stack: Vec::new(),
        }
    }

    fn run(&mut self, input: &syn::ItemMod) -> impl ToTokens {
        self.process_item_mod(input)
    }

    fn process_item_mod(&mut self, item: &syn::ItemMod) -> impl ToTokens {
        let (attrs, vis, mtoken, ident, content, semi) = (
            &item.attrs,
            &item.vis,
            &item.mod_token,
            &item.ident,
            &item.content,
            &item.semi,
        );

        let content = content.as_ref().map(|(_, items)| {
            let items = self.process_items(&items);
            quote!({
                use autolog_types::Loggable as _;
                #items
            })
        });

        quote! {
            #(#attrs)* #vis #mtoken #ident #content #semi
        }
    }

    fn process_items(&mut self, items: &[syn::Item]) -> impl ToTokens {
        let items = items.iter().map(|item| self.process_item(item));
        quote!(#(#items)*)
    }

    fn process_item(&mut self, item: &syn::Item) -> impl ToTokens {
        use syn::Item;
        match item {
            Item::Fn(item_fn) => Either3::A(self.process_fn(item_fn)),
            Item::Impl(item_impl) => Either3::B(self.process_impl(item_impl)),
            _ => Either3::C(quote!(#item)),
        }
    }

    fn process_fn(&mut self, item: &syn::ItemFn) -> impl ToTokens {
        let (attrs, vis, constness, unsafety, asyncness, abi, ident, decl, block) = (
            &item.attrs,
            &item.vis,
            &item.constness,
            &item.unsafety,
            &item.asyncness,
            &item.abi,
            &item.ident,
            &item.decl,
            &item.block,
        );

        // Skip blacklisted functions
        if self.config.blacklist.contains(&ident.to_string()) {
            return quote!(#item);
        }

        let (decl_generics, decl_inputs, decl_output) = (&decl.generics, &decl.inputs, &decl.output);

        // Skip modification for const and unsafe functions
        if constness.is_some() || unsafety.is_some() {
            return quote!(#item);
        };

        self.loc_stack.push(ASTLocationDescriptor {
            span: decl.fn_token.span,
            fn_desc: Some(FunctionDescriptor {
                ident: ident.clone(),
                decl: Clone::clone(decl),
            }),
        });

        let block = self.process_fn_block(block);

        self.loc_stack.pop();

        quote!(
            #(#attrs)* #abi #vis #unsafety #constness #asyncness
            fn #ident #decl_generics ( #decl_inputs ) #decl_output
            #block
        )
    }

    fn process_fn_block(&mut self, item: &syn::Block) -> impl ToTokens {
        let processor = FnBlockProcessor {
            block: item,
            config: &self.config,
            func_descriptor: self
                .loc_stack
                .last()
                .as_ref()
                .expect("loc stack is empty")
                .fn_desc
                .as_ref()
                .expect("function descriptor is empty"),
        };

        processor.generate_block()
    }

    fn process_impl(&mut self, item: &syn::ItemImpl) -> impl ToTokens {
        quote!(#item)
    }
}

struct FnBlockProcessor<'a> {
    config: &'a AutologArgs,
    block: &'a syn::Block,
    func_descriptor: &'a FunctionDescriptor,
}

impl<'a> FnBlockProcessor<'a> {
    fn generate_block(self) -> impl ToTokens {
        let mut block = Clone::clone(self.block);

        // check that block isn't empty
        if block.stmts.is_empty() {
            return quote!(#block);
        }

        let (flame_fmt, flame_fmt_args, flame_prologue, flame_epilogue) = {
            let flame = self.generate_flame();
            (flame.fmt_line, flame.fmt_args, flame.prologue, flame.epilogue)
        };

        let (log_fmt, log_fmt_args, log_prologue, log_epilogue) = {
            let log = self.generate_log();
            (log.fmt_line, log.fmt_args, log.prologue, log.epilogue)
        };

        let prologue = quote!(#log_prologue #flame_prologue);

        // Remove last stmt as it's duplicated in the `generate_log`
        block.stmts.pop();

        let fmt_line = format!("{}{}", flame_fmt, log_fmt);

        let epilogue = quote!(
            #log_epilogue
            #flame_epilogue
            log::trace!(#fmt_line, #flame_fmt_args, #log_fmt_args);
            __autolog_last_block_val
        );

        let statements = &block.stmts;

        quote!(
            { #prologue #(#statements)* #epilogue }
        )
    }

    fn generate_flame(&self) -> BlockGenerationSegment {
        if !self.config.flame {
            BlockGenerationSegment {
                fmt_line: String::new(),
                fmt_args: TokenStream2::new(),
                prologue: TokenStream2::new(),
                epilogue: TokenStream2::new(),
            }
        } else {
            BlockGenerationSegment {
                fmt_line: "[{}] ".to_string(),
                fmt_args: quote!(__autolog_flame_elapsed),
                prologue: quote!(let __autolog_flame_start = std::time::Instant::now();),
                epilogue: quote!(
                    let __autolog_flame_elapsed = {
                        let duration = __autolog_flame_start.elapsed();
                        if duration.as_secs() > 0 {
                            format!("{}.{}s", duration.as_secs(), duration.subsec_millis())
                        } else if duration.subsec_millis() > 0 {
                            format!("{}.{}ms", duration.subsec_millis(), duration.subsec_micros() / 1_000)
                        } else if duration.subsec_micros() > 0 {
                            format!("{}.{}μs", duration.subsec_micros(), duration.subsec_nanos() / 1_000)
                        } else {
                            format!("{}ns", duration.subsec_nanos())
                        }
                    };
                ),
            }
        }
    }

    fn generate_log(&self) -> BlockGenerationSegment {
        let args: Vec<_> = self
            .func_descriptor
            .decl
            .inputs
            .iter()
            .filter_map(|arg| {
                if let syn::FnArg::Captured(carg) = &arg {
                    match &carg.pat {
                        syn::Pat::Ident(syn::PatIdent { ident, .. }) => Some(ident.clone()),
                        _ => None,
                    }
                } else {
                    None
                }
            })
            .collect();

        let fmt_arg_placeholders: Vec<_> = args
            .iter()
            .map(|ident| format!("{} = {{}}", ident.to_string()))
            .collect();

        let prelude_fmt_line = format!(
            "fn {}({})",
            self.func_descriptor.ident.to_string(),
            fmt_arg_placeholders.join(", "),
        );

        let prelude_fmt_args = args.iter().map(|ident| quote!(#ident.autolog_fmt()));

        let last_stmt = self.block.stmts.last().unwrap();

        BlockGenerationSegment {
            fmt_line: "{} -> {}".into(),
            fmt_args: quote!(__autolog_args_formatted, __autolog_last_block_val.autolog_fmt()),
            prologue: quote!(let __autolog_args_formatted = format!(#prelude_fmt_line, #(#prelude_fmt_args,)*);),
            epilogue: quote!(let __autolog_last_block_val = #last_stmt;),
        }
    }
}

struct BlockGenerationSegment {
    fmt_line: String,
    fmt_args: TokenStream2,
    prologue: TokenStream2,
    epilogue: TokenStream2,
}

enum Either3<T1, T2, T3> {
    A(T1),
    B(T2),
    C(T3),
}

impl<T1, T2, T3> ToTokens for Either3<T1, T2, T3>
where
    T1: ToTokens,
    T2: ToTokens,
    T3: ToTokens,
{
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        match self {
            Either3::A(x) => x.to_tokens(tokens),
            Either3::B(x) => x.to_tokens(tokens),
            Either3::C(x) => x.to_tokens(tokens),
        }
    }
}
