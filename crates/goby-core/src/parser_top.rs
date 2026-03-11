use std::collections::HashSet;

use crate::ast::{EffectDecl, EffectMember, EmbedDecl, ImportDecl, ImportKind, TypeDeclaration};
use crate::parser::ParseError;
use crate::parser_util::{
    collect_indented_body, is_camel_case_identifier, is_identifier, is_indented,
    is_lowercase_start_identifier, is_module_path, is_non_reserved_identifier, is_reserved_keyword,
    is_type_parameter_identifier, parse_record_field, skip_blank_and_comment_lines,
    split_record_constructor_shape, split_top_level_definition, split_top_level_pipes,
    split_top_level_type, starts_with_keyword_token, strip_line_comment,
};
use crate::str_util::split_top_level_commas;

pub(crate) enum TopLevelItem {
    Import(ImportDecl),
    Embed(EmbedDecl),
    Type(TypeDeclaration),
    Effect(EffectDecl),
    Declaration(DeclarationParts),
}

pub(crate) struct DeclarationParts {
    pub(crate) name: String,
    pub(crate) type_annotation: Option<String>,
    pub(crate) params: Vec<String>,
    pub(crate) body: String,
    pub(crate) line: usize,
}

pub(crate) fn parse_top_level_item(
    lines: &[&str],
    index: usize,
) -> Result<(TopLevelItem, usize), ParseError> {
    let line = strip_line_comment(lines[index]).trim_end();
    let trimmed = line.trim();
    let line_no = index + 1;

    if trimmed.starts_with("import ") {
        let import = parse_import_line(trimmed).ok_or_else(|| ParseError {
            line: line_no,
            col: 1,
            message: "invalid import declaration".to_string(),
        })?;
        return Ok((TopLevelItem::Import(import), index + 1));
    }

    if trimmed.starts_with("@embed") {
        let (effect_name, handler_name) =
            parse_embed_line(trimmed).map_err(|message| ParseError {
                line: line_no,
                col: 1,
                message,
            })?;
        return Ok((
            TopLevelItem::Embed(EmbedDecl {
                effect_name,
                handler_name,
                line: line_no,
            }),
            index + 1,
        ));
    }

    if trimmed.starts_with("type ") {
        let ty_decl = parse_type_declaration_line(trimmed).ok_or_else(|| ParseError {
            line: line_no,
            col: 1,
            message: "invalid type declaration".to_string(),
        })?;
        return Ok((TopLevelItem::Type(ty_decl), index + 1));
    }

    if trimmed.starts_with("effect ") && !trimmed.contains('=') {
        let (effect_decl, next_index) = parse_effect_declaration(lines, index)?;
        return Ok((TopLevelItem::Effect(effect_decl), next_index));
    }

    if starts_with_keyword_token(trimmed, "handler") {
        return Err(ParseError {
            line: line_no,
            col: 1,
            message:
                "legacy top-level `handler ... for ...` is no longer supported; use `handler` expressions with `with`"
                    .to_string(),
        });
    }

    let (declaration, next_index) = parse_declaration_header(lines, index)?;
    Ok((TopLevelItem::Declaration(declaration), next_index))
}

fn parse_effect_declaration(
    lines: &[&str],
    start: usize,
) -> Result<(EffectDecl, usize), ParseError> {
    let trimmed = strip_line_comment(lines[start]).trim_end().trim();
    let line_no = start + 1;
    let effect_header = trimmed["effect ".len()..].trim();
    if effect_header.is_empty() {
        return Err(ParseError {
            line: line_no,
            col: 1,
            message: "effect declaration requires a name".to_string(),
        });
    }

    let mut header_parts = effect_header.split_whitespace();
    let effect_name = header_parts
        .next()
        .expect("effect header should have first token")
        .to_string();
    if !is_camel_case_identifier(&effect_name) {
        return Err(ParseError {
            line: line_no,
            col: 1,
            message: "effect declaration name must be CamelCase".to_string(),
        });
    }

    let mut type_params = Vec::new();
    let mut seen_type_params = HashSet::new();
    for param in header_parts {
        if !is_type_parameter_identifier(param) {
            return Err(ParseError {
                line: line_no,
                col: 1,
                message: "effect type parameter must start with a lowercase letter or `_`"
                    .to_string(),
            });
        }
        if !seen_type_params.insert(param) {
            return Err(ParseError {
                line: line_no,
                col: 1,
                message: format!("duplicate effect type parameter `{}`", param),
            });
        }
        type_params.push(param.to_string());
    }

    let mut members = Vec::new();
    let mut index = start + 1;
    while index < lines.len() {
        let member_line = lines[index];
        let member_trimmed = strip_line_comment(member_line).trim_end();
        let member_trimmed_str = member_trimmed.trim();
        if member_trimmed_str.is_empty() || member_trimmed_str.starts_with('#') {
            index += 1;
            continue;
        }
        if !is_indented(member_line) {
            break;
        }
        let Some((name_raw, ty_raw)) = member_trimmed_str.split_once(':') else {
            return Err(ParseError {
                line: index + 1,
                col: 1,
                message: "invalid effect member signature: expected `name: Type`".to_string(),
            });
        };
        let name = name_raw.trim();
        let ty = ty_raw.trim();
        if !is_non_reserved_identifier(name) {
            return Err(ParseError {
                line: index + 1,
                col: 1,
                message: "effect member name must be a non-reserved identifier".to_string(),
            });
        }
        if ty.is_empty() {
            return Err(ParseError {
                line: index + 1,
                col: 1,
                message: "effect member type must not be empty".to_string(),
            });
        }
        members.push(EffectMember {
            name: name.to_string(),
            type_annotation: ty.to_string(),
        });
        index += 1;
    }

    Ok((
        EffectDecl {
            name: effect_name,
            type_params,
            members,
        },
        index,
    ))
}

fn parse_declaration_header(
    lines: &[&str],
    start: usize,
) -> Result<(DeclarationParts, usize), ParseError> {
    let line = strip_line_comment(lines[start]).trim_end();
    let line_no = start + 1;
    let mut annotated_name: Option<&str> = None;
    let mut type_annotation = None;
    let decl_line = line_no;
    let mut index = start;

    if let Some((name, ty)) = split_top_level_type(line) {
        if name.is_empty() || ty.is_empty() {
            return Err(ParseError {
                line: line_no,
                col: 1,
                message: "invalid type annotation".to_string(),
            });
        }
        annotated_name = Some(name);
        type_annotation = Some(ty.to_string());
        index += 1;
        index = skip_blank_and_comment_lines(lines, index);
        if index >= lines.len() {
            return Err(ParseError {
                line: decl_line,
                col: 1,
                message: "missing declaration body after type annotation".to_string(),
            });
        }
    }

    let body_line = strip_line_comment(lines[index]).trim_end();
    let (name, params, mut body) =
        split_top_level_definition(body_line).ok_or_else(|| ParseError {
            line: index + 1,
            col: 1,
            message: "expected top-level definition (`name ... = ...`)".to_string(),
        })?;
    if !is_lowercase_start_identifier(name) {
        return Err(ParseError {
            line: index + 1,
            col: 1,
            message: "declaration name must start with a lowercase letter".to_string(),
        });
    }
    if is_reserved_keyword(name) {
        return Err(ParseError {
            line: index + 1,
            col: 1,
            message: format!("`{name}` is a reserved keyword"),
        });
    }
    if let Some(annotated_name) = annotated_name
        && annotated_name != name
    {
        return Err(ParseError {
            line: index + 1,
            col: 1,
            message: format!(
                "type annotation name `{}` does not match definition name `{}`",
                annotated_name, name
            ),
        });
    }

    let next_index = collect_indented_body(lines, index + 1, &mut body);
    Ok((
        DeclarationParts {
            name: name.to_string(),
            type_annotation,
            params,
            body,
            line: decl_line,
        },
        next_index,
    ))
}

fn parse_import_line(line: &str) -> Option<ImportDecl> {
    let rest = line.strip_prefix("import ")?.trim();
    if rest.is_empty() {
        return None;
    }

    if let Some(open_idx) = rest.find('(') {
        if !rest.ends_with(')') {
            return None;
        }
        let module_path = rest[..open_idx].trim();
        if !is_module_path(module_path) {
            return None;
        }
        let inner = rest[open_idx + 1..rest.len() - 1].trim();
        if inner.is_empty() {
            return None;
        }
        let names = split_top_level_commas(inner);
        let mut symbols = Vec::new();
        for name in names {
            let name = name.trim();
            if !is_non_reserved_identifier(name) {
                return None;
            }
            symbols.push(name.to_string());
        }
        return Some(ImportDecl {
            module_path: module_path.to_string(),
            kind: ImportKind::Selective(symbols),
        });
    }

    if let Some((module_path, alias)) = rest.split_once(" as ") {
        let module_path = module_path.trim();
        let alias = alias.trim();
        if !is_module_path(module_path) || !is_non_reserved_identifier(alias) {
            return None;
        }
        return Some(ImportDecl {
            module_path: module_path.to_string(),
            kind: ImportKind::Alias(alias.to_string()),
        });
    }

    if !is_module_path(rest) {
        return None;
    }
    Some(ImportDecl {
        module_path: rest.to_string(),
        kind: ImportKind::Plain,
    })
}

fn parse_embed_line(line: &str) -> Result<(String, String), String> {
    let rest = line
        .strip_prefix("@embed")
        .map(str::trim_start)
        .ok_or_else(|| "invalid @embed declaration".to_string())?;
    if rest.is_empty() {
        return Err(
            "invalid @embed declaration: expected `@embed <EffectName> <HandlerName>`".to_string(),
        );
    }

    if rest.starts_with("effect ") {
        return Err(
            "legacy `@embed effect <EffectName>` is no longer supported; use `@embed <EffectName> <HandlerName>`".to_string(),
        );
    }

    let mut tokens = rest.split_whitespace();
    let Some(effect_name) = tokens.next() else {
        return Err(
            "invalid @embed declaration: expected `@embed <EffectName> <HandlerName>`".to_string(),
        );
    };
    let Some(handler_name) = tokens.next() else {
        return Err("embedded handler name is missing".to_string());
    };
    if tokens.next().is_some() {
        return Err(
            "invalid @embed declaration: expected `@embed <EffectName> <HandlerName>`".to_string(),
        );
    }

    if !is_identifier(effect_name) {
        return Err("invalid embedded effect name".to_string());
    }
    if !is_identifier(handler_name) {
        return Err("invalid embedded handler name".to_string());
    }
    Ok((effect_name.to_string(), handler_name.to_string()))
}

fn parse_type_declaration_line(line: &str) -> Option<TypeDeclaration> {
    let rest = line.strip_prefix("type ")?.trim();
    let (name, rhs) = rest.split_once('=')?;
    let name = name.trim();
    let rhs = rhs.trim();
    if !is_camel_case_identifier(name) || rhs.is_empty() {
        return None;
    }

    if let Some(parts) = split_top_level_pipes(rhs) {
        let constructors: Option<Vec<String>> = parts
            .into_iter()
            .map(str::trim)
            .map(|ctor| is_camel_case_identifier(ctor).then(|| ctor.to_string()))
            .collect();
        let constructors = constructors?;
        if constructors.is_empty() {
            return None;
        }
        return Some(TypeDeclaration::Union {
            name: name.to_string(),
            constructors,
        });
    }

    if let Some((constructor, inner)) = split_record_constructor_shape(rhs) {
        if !is_camel_case_identifier(constructor) {
            return None;
        }
        let fields = if inner.is_empty() {
            Vec::new()
        } else {
            let parts = split_top_level_commas(inner);
            let parsed_fields: Option<Vec<_>> = parts
                .iter()
                .map(|part| parse_record_field(part.trim()))
                .collect();
            parsed_fields?
        };
        return Some(TypeDeclaration::Record {
            name: name.to_string(),
            constructor: constructor.to_string(),
            fields,
        });
    }

    Some(TypeDeclaration::Alias {
        name: name.to_string(),
        target: rhs.to_string(),
    })
}
