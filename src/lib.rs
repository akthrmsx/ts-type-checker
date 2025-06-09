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
}

#[derive(Debug, Clone)]
pub struct Parameter {
    name: String,
    typing: Type,
}

impl PartialEq for Parameter {
    fn eq(&self, other: &Self) -> bool {
        self.typing == other.typing
    }
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
    True,
    False,
    Condition {
        condition: Box<Self>,
        consequent: Box<Self>,
        alternative: Box<Self>,
    },
    Number {
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
    Object {
        properties: Vec<TermProperty>,
    },
    PropertyAccess {
        object: Box<Self>,
        name: String,
    },
}

impl Term {
    pub fn condition(condition: Self, consequent: Self, alternative: Self) -> Self {
        Self::Condition {
            condition: Box::new(condition),
            consequent: Box::new(consequent),
            alternative: Box::new(alternative),
        }
    }

    pub fn number(value: i64) -> Self {
        Self::Number { value }
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

    pub fn object(properties: Vec<TermProperty>) -> Self {
        let mut properties = properties;
        properties.sort_by(|a, b| a.name.cmp(&b.name));
        Self::Object { properties }
    }

    pub fn property_access(object: Self, name: &str) -> Self {
        Self::PropertyAccess {
            object: Box::new(object),
            name: name.into(),
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
        Term::True | Term::False => Ok(Type::Boolean),
        Term::Condition {
            condition,
            consequent,
            alternative,
        } => {
            ensure!(
                type_check(*condition, environment)? == Type::Boolean,
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
        Term::Number { .. } => Ok(Type::Number),
        Term::Addition { left, right } => {
            ensure!(
                type_check(*left, environment)? == Type::Number,
                TypeCheckError::UnexpectedType,
            );
            ensure!(
                type_check(*right, environment)? == Type::Number,
                TypeCheckError::UnexpectedType,
            );

            Ok(Type::Number)
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
                        type_check(argument.clone(), environment)? == parameter.typing,
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
        Term::Object {
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
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        Parameter, Term, TermProperty, Type, TypeCheckError, TypeChecker, TypeEnvironment,
        TypeProperty,
    };

    #[test]
    fn test_boolean() {
        assert_eq!(TypeChecker::new(Term::True).run().unwrap(), Type::Boolean);

        assert_eq!(TypeChecker::new(Term::False).run().unwrap(), Type::Boolean);
    }

    #[test]
    fn test_condition() {
        assert_eq!(
            TypeChecker::new(Term::condition(
                Term::True,
                Term::number(1),
                Term::number(2),
            ))
            .run()
            .unwrap(),
            Type::Number,
        );

        assert_eq!(
            TypeChecker::new(Term::condition(Term::number(1), Term::True, Term::False))
                .run()
                .unwrap_err()
                .downcast::<TypeCheckError>()
                .unwrap(),
            TypeCheckError::UnexpectedType,
        );

        assert_eq!(
            TypeChecker::new(Term::condition(Term::True, Term::number(1), Term::False))
                .run()
                .unwrap_err()
                .downcast::<TypeCheckError>()
                .unwrap(),
            TypeCheckError::MismatchedTypes,
        );

        assert_eq!(
            TypeChecker::new(Term::condition(Term::True, Term::False, Term::number(1)))
                .run()
                .unwrap_err()
                .downcast::<TypeCheckError>()
                .unwrap(),
            TypeCheckError::MismatchedTypes,
        );
    }

    #[test]
    fn test_number() {
        assert_eq!(
            TypeChecker::new(Term::number(1)).run().unwrap(),
            Type::Number,
        );
    }

    #[test]
    fn test_addition() {
        assert_eq!(
            TypeChecker::new(Term::addition(Term::number(1), Term::number(2)))
                .run()
                .unwrap(),
            Type::Number,
        );

        assert_eq!(
            TypeChecker::new(Term::addition(Term::True, Term::number(1)))
                .run()
                .unwrap_err()
                .downcast::<TypeCheckError>()
                .unwrap(),
            TypeCheckError::UnexpectedType,
        );

        assert_eq!(
            TypeChecker::new(Term::addition(Term::number(1), Term::True))
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
        environment.insert("a".into(), Type::Boolean);
        assert_eq!(
            TypeChecker::new(Term::variable("a"))
                .with_environment(environment)
                .run()
                .unwrap(),
            Type::Boolean,
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
                    Parameter::new("a", Type::Boolean),
                    Parameter::new("b", Type::Boolean),
                ],
                Term::True,
            ))
            .run()
            .unwrap(),
            Type::function(
                vec![
                    Parameter::new("a", Type::Boolean),
                    Parameter::new("b", Type::Boolean),
                ],
                Type::Boolean,
            ),
        );
    }

    #[test]
    fn test_call() {
        assert_eq!(
            TypeChecker::new(Term::call(
                Term::function(
                    vec![
                        Parameter::new("a", Type::Boolean),
                        Parameter::new("b", Type::Boolean),
                    ],
                    Term::True,
                ),
                vec![Term::True, Term::False],
            ))
            .run()
            .unwrap(),
            Type::Boolean,
        );

        assert_eq!(
            TypeChecker::new(Term::call(Term::True, vec![]))
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
                        Parameter::new("a", Type::Boolean),
                        Parameter::new("b", Type::Boolean),
                    ],
                    Term::True,
                ),
                vec![Term::True],
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
                        Parameter::new("a", Type::Boolean),
                        Parameter::new("b", Type::Boolean),
                    ],
                    Term::True,
                ),
                vec![Term::True, Term::number(1)],
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
            TypeChecker::new(Term::sequence(Term::True, Term::number(1)))
                .run()
                .unwrap(),
            Type::Number,
        );
    }

    #[test]
    fn test_constant() {
        assert_eq!(
            TypeChecker::new(Term::constant("a", Term::True, Term::variable("a")))
                .run()
                .unwrap(),
            Type::Boolean,
        );
    }

    #[test]
    fn test_object() {
        assert_eq!(
            TypeChecker::new(Term::object(vec![
                TermProperty::new("a", Term::True),
                TermProperty::new("b", Term::number(1)),
            ]))
            .run()
            .unwrap(),
            Type::object(vec![
                TypeProperty::new("a", Type::Boolean),
                TypeProperty::new("b", Type::Number),
            ]),
        );
    }

    #[test]
    fn test_property_access() {
        assert_eq!(
            TypeChecker::new(Term::property_access(
                Term::object(vec![
                    TermProperty::new("a", Term::True),
                    TermProperty::new("b", Term::number(1)),
                ]),
                "a",
            ))
            .run()
            .unwrap(),
            Type::Boolean,
        );

        assert_eq!(
            TypeChecker::new(Term::property_access(Term::True, "a"))
                .run()
                .unwrap_err()
                .downcast::<TypeCheckError>()
                .unwrap(),
            TypeCheckError::UnexpectedType,
        );

        assert_eq!(
            TypeChecker::new(Term::property_access(
                Term::object(vec![
                    TermProperty::new("a", Term::True),
                    TermProperty::new("b", Term::number(1)),
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
}
