use anyhow::{Context, Result, bail, ensure};
use std::collections::HashMap;
use thiserror::Error;

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Boolean,
    Number,
    Function {
        parameters: Vec<Parameter>,
        returning: Box<Self>,
    },
    Object {
        properties: Vec<TypeProperty>,
    },
}

impl Type {
    pub fn boolean() -> Self {
        Self::Boolean
    }

    pub fn number() -> Self {
        Self::Number
    }

    pub fn function(parameters: Vec<Parameter>, returning: Self) -> Self {
        Self::Function {
            parameters,
            returning: Box::new(returning),
        }
    }

    pub fn object(properties: Vec<TypeProperty>) -> Self {
        let mut properties = properties;
        properties.sort_by(|a, b| a.name.cmp(&b.name));
        Self::Object { properties }
    }

    pub fn is_subtype(&self, other: &Self) -> bool {
        match (self, other) {
            (
                Self::Function {
                    parameters: parameters1,
                    returning: returning1,
                },
                Self::Function {
                    parameters: parameters2,
                    returning: returning2,
                },
            ) => {
                if parameters1.len() != parameters2.len() {
                    return false;
                }

                for (parameter1, parameter2) in parameters1.iter().zip(parameters2.iter()) {
                    if !parameter2.typing.is_subtype(&parameter1.typing) {
                        return false;
                    }
                }

                if !returning1.is_subtype(returning2) {
                    return false;
                }

                true
            }
            (
                Self::Object {
                    properties: properties1,
                },
                Self::Object {
                    properties: properties2,
                },
            ) => {
                for property2 in properties2 {
                    if !properties1.iter().any(|property1| {
                        property1.name == property2.name
                            && property1.typing.is_subtype(&property2.typing)
                    }) {
                        return false;
                    }
                }

                true
            }
            (type1, type2) => type1 == type2,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Parameter {
    name: String,
    typing: Type,
}

impl Parameter {
    pub fn new(name: &str, typing: Type) -> Self {
        Self {
            name: name.into(),
            typing,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypeProperty {
    name: String,
    typing: Type,
}

impl TypeProperty {
    pub fn new(name: &str, typing: Type) -> Self {
        Self {
            name: name.into(),
            typing,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Term {
    TrueLiteral,
    FalseLiteral,
    Condition {
        condition: Box<Self>,
        consequent: Box<Self>,
        alternative: Box<Self>,
    },
    NumberLiteral {
        value: i64,
    },
    Addition {
        left: Box<Self>,
        right: Box<Self>,
    },
    Variable {
        name: String,
    },
    Function {
        parameters: Vec<Parameter>,
        body: Box<Self>,
    },
    Call {
        function: Box<Self>,
        arguments: Vec<Self>,
    },
    Sequence {
        first: Box<Self>,
        second: Box<Self>,
    },
    Constant {
        name: String,
        value: Box<Self>,
        next: Box<Self>,
    },
    ObjectLiteral {
        properties: Vec<TermProperty>,
    },
    PropertyAccess {
        object: Box<Self>,
        name: String,
    },
    RecursiveFunction {
        name: String,
        parameters: Vec<Parameter>,
        returning: Type,
        body: Box<Self>,
        next: Box<Self>,
    },
}

impl Term {
    pub fn true_literal() -> Self {
        Self::TrueLiteral
    }

    pub fn false_literal() -> Self {
        Self::FalseLiteral
    }

    pub fn condition(condition: Self, consequent: Self, alternative: Self) -> Self {
        Self::Condition {
            condition: Box::new(condition),
            consequent: Box::new(consequent),
            alternative: Box::new(alternative),
        }
    }

    pub fn number_literal(value: i64) -> Self {
        Self::NumberLiteral { value }
    }

    pub fn addition(left: Self, right: Self) -> Self {
        Self::Addition {
            left: Box::new(left),
            right: Box::new(right),
        }
    }

    pub fn variable(name: &str) -> Self {
        Self::Variable { name: name.into() }
    }

    pub fn function(parameters: Vec<Parameter>, body: Self) -> Self {
        Self::Function {
            parameters,
            body: Box::new(body),
        }
    }

    pub fn call(function: Self, arguments: Vec<Self>) -> Self {
        Self::Call {
            function: Box::new(function),
            arguments,
        }
    }

    pub fn sequence(first: Self, second: Self) -> Self {
        Self::Sequence {
            first: Box::new(first),
            second: Box::new(second),
        }
    }

    pub fn constant(name: &str, value: Self, next: Self) -> Self {
        Self::Constant {
            name: name.into(),
            value: Box::new(value),
            next: Box::new(next),
        }
    }

    pub fn object_literal(properties: Vec<TermProperty>) -> Self {
        let mut properties = properties;
        properties.sort_by(|a, b| a.name.cmp(&b.name));
        Self::ObjectLiteral { properties }
    }

    pub fn property_access(object: Self, name: &str) -> Self {
        Self::PropertyAccess {
            object: Box::new(object),
            name: name.into(),
        }
    }

    pub fn recursive_function(
        name: &str,
        parameters: Vec<Parameter>,
        returning: Type,
        body: Self,
        next: Self,
    ) -> Self {
        Self::RecursiveFunction {
            name: name.into(),
            parameters,
            returning,
            body: Box::new(body),
            next: Box::new(next),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct TermProperty {
    name: String,
    term: Term,
}

impl TermProperty {
    pub fn new(name: &str, term: Term) -> Self {
        Self {
            name: name.into(),
            term,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Error)]
pub enum TypeCheckError {
    #[error("unexpected type")]
    UnexpectedType,
    #[error("mismatched types")]
    MismatchedTypes,
    #[error("unknown variable")]
    UnknownVariable,
    #[error("invalid arity")]
    InvalidArity,
    #[error("unknown property")]
    UnknownProperty,
}

pub type TypeEnvironment = HashMap<String, Type>;

pub struct TypeChecker {
    term: Term,
    environment: TypeEnvironment,
}

impl TypeChecker {
    pub fn new(term: Term) -> Self {
        Self {
            term,
            environment: TypeEnvironment::new(),
        }
    }

    pub fn with_environment(mut self, environment: TypeEnvironment) -> Self {
        self.environment = environment;
        self
    }

    pub fn run(mut self) -> Result<Type> {
        type_check(self.term, &mut self.environment)
    }
}

fn type_check(term: Term, environment: &mut TypeEnvironment) -> Result<Type> {
    match term {
        Term::TrueLiteral | Term::FalseLiteral => Ok(Type::boolean()),
        Term::Condition {
            condition,
            consequent,
            alternative,
        } => {
            ensure!(
                type_check(*condition, environment)? == Type::boolean(),
                TypeCheckError::UnexpectedType,
            );

            match (
                type_check(*consequent, environment)?,
                type_check(*alternative, environment)?,
            ) {
                (consequent, alternative) if consequent == alternative => Ok(consequent),
                _ => bail!(TypeCheckError::MismatchedTypes),
            }
        }
        Term::NumberLiteral { .. } => Ok(Type::number()),
        Term::Addition { left, right } => {
            ensure!(
                type_check(*left, environment)? == Type::number(),
                TypeCheckError::UnexpectedType,
            );
            ensure!(
                type_check(*right, environment)? == Type::number(),
                TypeCheckError::UnexpectedType,
            );

            Ok(Type::number())
        }
        Term::Variable { name } => environment
            .get(&name)
            .cloned()
            .context(TypeCheckError::UnknownVariable),
        Term::Function { parameters, body } => {
            let mut new_environment = environment.clone();

            for parameter in parameters.iter() {
                new_environment.insert(parameter.name.clone(), parameter.typing.clone());
            }

            Ok(Type::function(
                parameters,
                type_check(*body, &mut new_environment)?,
            ))
        }
        Term::Call {
            function,
            arguments,
        } => match type_check(*function, environment)? {
            Type::Function {
                parameters,
                returning,
            } => {
                ensure!(
                    arguments.len() == parameters.len(),
                    TypeCheckError::InvalidArity,
                );

                for (argument, parameter) in arguments.iter().zip(parameters.iter()) {
                    ensure!(
                        type_check(argument.clone(), environment)?.is_subtype(&parameter.typing),
                        TypeCheckError::MismatchedTypes,
                    );
                }

                Ok(*returning)
            }
            _ => bail!(TypeCheckError::UnexpectedType),
        },
        Term::Sequence { first, second } => {
            type_check(*first, environment)?;
            type_check(*second, environment)
        }
        Term::Constant { name, value, next } => {
            let value = type_check(*value, environment)?;
            environment.insert(name, value);
            type_check(*next, environment)
        }
        Term::ObjectLiteral {
            properties: term_properties,
        } => {
            let mut type_properties = vec![];

            for term_property in term_properties {
                type_properties.push(TypeProperty::new(
                    &term_property.name,
                    type_check(term_property.term, environment)?,
                ));
            }

            Ok(Type::object(type_properties))
        }
        Term::PropertyAccess { object, name } => match type_check(*object, environment)? {
            Type::Object { properties } => properties
                .iter()
                .find_map(|property| {
                    if property.name == name {
                        Some(property.typing.clone())
                    } else {
                        None
                    }
                })
                .context(TypeCheckError::UnknownProperty),
            _ => bail!(TypeCheckError::UnexpectedType),
        },
        Term::RecursiveFunction {
            name,
            parameters,
            returning,
            body,
            next,
        } => {
            let function = Type::function(parameters.clone(), returning.clone());

            let mut new_environment = environment.clone();

            for parameter in parameters {
                new_environment.insert(parameter.name, parameter.typing);
            }

            new_environment.insert(name.clone(), function.clone());

            ensure!(
                type_check(*body, &mut new_environment)? == returning,
                TypeCheckError::UnexpectedType,
            );

            let mut new_environment = environment.clone();
            new_environment.insert(name, function);

            type_check(*next, &mut new_environment)
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        Parameter, Term, TermProperty, Type, TypeCheckError, TypeChecker, TypeEnvironment,
        TypeProperty,
    };

    #[test]
    fn test_true_literal() {
        assert_eq!(
            TypeChecker::new(Term::true_literal()).run().unwrap(),
            Type::boolean(),
        );
    }

    #[test]
    fn test_false_literal() {
        assert_eq!(
            TypeChecker::new(Term::false_literal()).run().unwrap(),
            Type::boolean(),
        );
    }

    #[test]
    fn test_condition() {
        assert_eq!(
            TypeChecker::new(Term::condition(
                Term::true_literal(),
                Term::number_literal(1),
                Term::number_literal(2),
            ))
            .run()
            .unwrap(),
            Type::number(),
        );

        assert_eq!(
            TypeChecker::new(Term::condition(
                Term::number_literal(1),
                Term::true_literal(),
                Term::false_literal(),
            ))
            .run()
            .unwrap_err()
            .downcast::<TypeCheckError>()
            .unwrap(),
            TypeCheckError::UnexpectedType,
        );

        assert_eq!(
            TypeChecker::new(Term::condition(
                Term::true_literal(),
                Term::number_literal(1),
                Term::false_literal(),
            ))
            .run()
            .unwrap_err()
            .downcast::<TypeCheckError>()
            .unwrap(),
            TypeCheckError::MismatchedTypes,
        );

        assert_eq!(
            TypeChecker::new(Term::condition(
                Term::true_literal(),
                Term::false_literal(),
                Term::number_literal(1),
            ))
            .run()
            .unwrap_err()
            .downcast::<TypeCheckError>()
            .unwrap(),
            TypeCheckError::MismatchedTypes,
        );
    }

    #[test]
    fn test_number_literal() {
        assert_eq!(
            TypeChecker::new(Term::number_literal(1)).run().unwrap(),
            Type::number(),
        );
    }

    #[test]
    fn test_addition() {
        assert_eq!(
            TypeChecker::new(Term::addition(
                Term::number_literal(1),
                Term::number_literal(2),
            ))
            .run()
            .unwrap(),
            Type::number(),
        );

        assert_eq!(
            TypeChecker::new(Term::addition(
                Term::true_literal(),
                Term::number_literal(1),
            ))
            .run()
            .unwrap_err()
            .downcast::<TypeCheckError>()
            .unwrap(),
            TypeCheckError::UnexpectedType,
        );

        assert_eq!(
            TypeChecker::new(Term::addition(
                Term::number_literal(1),
                Term::true_literal(),
            ))
            .run()
            .unwrap_err()
            .downcast::<TypeCheckError>()
            .unwrap(),
            TypeCheckError::UnexpectedType,
        );
    }

    #[test]
    fn test_variable() {
        let mut environment = TypeEnvironment::new();
        environment.insert("a".into(), Type::boolean());
        assert_eq!(
            TypeChecker::new(Term::variable("a"))
                .with_environment(environment)
                .run()
                .unwrap(),
            Type::boolean(),
        );

        assert_eq!(
            TypeChecker::new(Term::variable("a"))
                .run()
                .unwrap_err()
                .downcast::<TypeCheckError>()
                .unwrap(),
            TypeCheckError::UnknownVariable,
        );
    }

    #[test]
    fn test_function() {
        assert_eq!(
            TypeChecker::new(Term::function(
                vec![
                    Parameter::new("a", Type::boolean()),
                    Parameter::new("b", Type::boolean()),
                ],
                Term::true_literal(),
            ))
            .run()
            .unwrap(),
            Type::function(
                vec![
                    Parameter::new("a", Type::boolean()),
                    Parameter::new("b", Type::boolean()),
                ],
                Type::boolean(),
            ),
        );
    }

    #[test]
    fn test_call() {
        assert_eq!(
            TypeChecker::new(Term::call(
                Term::function(
                    vec![
                        Parameter::new("a", Type::boolean()),
                        Parameter::new("b", Type::boolean()),
                    ],
                    Term::true_literal(),
                ),
                vec![Term::true_literal(), Term::false_literal()],
            ))
            .run()
            .unwrap(),
            Type::boolean(),
        );

        assert_eq!(
            TypeChecker::new(Term::call(
                Term::function(
                    vec![Parameter::new(
                        "a",
                        Type::object(vec![TypeProperty::new("a", Type::boolean())]),
                    )],
                    Term::property_access(Term::variable("a"), "a"),
                ),
                vec![Term::object_literal(vec![
                    TermProperty::new("a", Term::true_literal()),
                    TermProperty::new("b", Term::number_literal(1)),
                ])],
            ))
            .run()
            .unwrap(),
            Type::boolean(),
        );

        assert_eq!(
            TypeChecker::new(Term::call(Term::true_literal(), vec![]))
                .run()
                .unwrap_err()
                .downcast::<TypeCheckError>()
                .unwrap(),
            TypeCheckError::UnexpectedType,
        );

        assert_eq!(
            TypeChecker::new(Term::call(
                Term::function(
                    vec![
                        Parameter::new("a", Type::boolean()),
                        Parameter::new("b", Type::boolean()),
                    ],
                    Term::true_literal(),
                ),
                vec![Term::true_literal()],
            ))
            .run()
            .unwrap_err()
            .downcast::<TypeCheckError>()
            .unwrap(),
            TypeCheckError::InvalidArity,
        );

        assert_eq!(
            TypeChecker::new(Term::call(
                Term::function(
                    vec![
                        Parameter::new("a", Type::boolean()),
                        Parameter::new("b", Type::boolean()),
                    ],
                    Term::true_literal(),
                ),
                vec![Term::true_literal(), Term::number_literal(1)],
            ))
            .run()
            .unwrap_err()
            .downcast::<TypeCheckError>()
            .unwrap(),
            TypeCheckError::MismatchedTypes,
        );

        assert_eq!(
            TypeChecker::new(Term::call(
                Term::function(
                    vec![Parameter::new(
                        "a",
                        Type::object(vec![
                            TypeProperty::new("a", Type::boolean()),
                            TypeProperty::new("b", Type::number()),
                        ]),
                    )],
                    Term::property_access(Term::variable("a"), "a"),
                ),
                vec![Term::object_literal(vec![TermProperty::new(
                    "a",
                    Term::true_literal(),
                )])],
            ))
            .run()
            .unwrap_err()
            .downcast::<TypeCheckError>()
            .unwrap(),
            TypeCheckError::MismatchedTypes,
        );
    }

    #[test]
    fn test_sequence() {
        assert_eq!(
            TypeChecker::new(Term::sequence(
                Term::true_literal(),
                Term::number_literal(1),
            ))
            .run()
            .unwrap(),
            Type::number(),
        );
    }

    #[test]
    fn test_constant() {
        assert_eq!(
            TypeChecker::new(Term::constant(
                "a",
                Term::true_literal(),
                Term::variable("a"),
            ))
            .run()
            .unwrap(),
            Type::boolean(),
        );
    }

    #[test]
    fn test_object_literal() {
        assert_eq!(
            TypeChecker::new(Term::object_literal(vec![
                TermProperty::new("a", Term::true_literal()),
                TermProperty::new("b", Term::number_literal(1)),
            ]))
            .run()
            .unwrap(),
            Type::object(vec![
                TypeProperty::new("a", Type::boolean()),
                TypeProperty::new("b", Type::number()),
            ]),
        );
    }

    #[test]
    fn test_property_access() {
        assert_eq!(
            TypeChecker::new(Term::property_access(
                Term::object_literal(vec![
                    TermProperty::new("a", Term::true_literal()),
                    TermProperty::new("b", Term::number_literal(1)),
                ]),
                "a",
            ))
            .run()
            .unwrap(),
            Type::boolean(),
        );

        assert_eq!(
            TypeChecker::new(Term::property_access(Term::true_literal(), "a"))
                .run()
                .unwrap_err()
                .downcast::<TypeCheckError>()
                .unwrap(),
            TypeCheckError::UnexpectedType,
        );

        assert_eq!(
            TypeChecker::new(Term::property_access(
                Term::object_literal(vec![
                    TermProperty::new("a", Term::true_literal()),
                    TermProperty::new("b", Term::number_literal(1)),
                ]),
                "c",
            ))
            .run()
            .unwrap_err()
            .downcast::<TypeCheckError>()
            .unwrap(),
            TypeCheckError::UnknownProperty,
        );
    }

    #[test]
    fn test_recursive_function() {
        assert_eq!(
            TypeChecker::new(Term::recursive_function(
                "f",
                vec![Parameter::new("a", Type::number())],
                Type::number(),
                Term::call(Term::variable("f"), vec![Term::variable("a")]),
                Term::variable("f"),
            ))
            .run()
            .unwrap(),
            Type::function(vec![Parameter::new("a", Type::number())], Type::number()),
        );

        assert_eq!(
            TypeChecker::new(Term::recursive_function(
                "f",
                vec![Parameter::new("a", Type::number())],
                Type::number(),
                Term::true_literal(),
                Term::variable("f"),
            ))
            .run()
            .unwrap_err()
            .downcast::<TypeCheckError>()
            .unwrap(),
            TypeCheckError::UnexpectedType,
        );
    }
}
