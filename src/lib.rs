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
}

impl Type {
    pub fn function(parameters: Vec<Parameter>, returning: Self) -> Self {
        Self::Function {
            parameters,
            returning: Box::new(returning),
        }
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
}

pub type TypeEnvironment = HashMap<String, Type>;

pub fn type_check(term: Term, environment: &mut TypeEnvironment) -> Result<Type> {
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
    }
}

#[cfg(test)]
mod tests {
    use crate::{Parameter, Term, Type, TypeCheckError, TypeEnvironment, type_check};

    #[test]
    fn test_boolean() {
        let mut environment = TypeEnvironment::new();
        assert_eq!(
            type_check(Term::True, &mut environment).unwrap(),
            Type::Boolean,
        );

        let mut environment = TypeEnvironment::new();
        assert_eq!(
            type_check(Term::False, &mut environment).unwrap(),
            Type::Boolean,
        );
    }

    #[test]
    fn test_condition() {
        let mut environment = TypeEnvironment::new();
        assert_eq!(
            type_check(
                Term::condition(Term::True, Term::number(1), Term::number(2)),
                &mut environment,
            )
            .unwrap(),
            Type::Number,
        );

        let mut environment = TypeEnvironment::new();
        assert_eq!(
            type_check(
                Term::condition(Term::number(1), Term::True, Term::False),
                &mut environment,
            )
            .unwrap_err()
            .downcast::<TypeCheckError>()
            .unwrap(),
            TypeCheckError::UnexpectedType,
        );

        let mut environment = TypeEnvironment::new();
        assert_eq!(
            type_check(
                Term::condition(Term::True, Term::number(1), Term::False),
                &mut environment,
            )
            .unwrap_err()
            .downcast::<TypeCheckError>()
            .unwrap(),
            TypeCheckError::MismatchedTypes,
        );

        let mut environment = TypeEnvironment::new();
        assert_eq!(
            type_check(
                Term::condition(Term::True, Term::False, Term::number(1)),
                &mut environment,
            )
            .unwrap_err()
            .downcast::<TypeCheckError>()
            .unwrap(),
            TypeCheckError::MismatchedTypes,
        );
    }

    #[test]
    fn test_number() {
        let mut environment = TypeEnvironment::new();
        assert_eq!(
            type_check(Term::number(1), &mut environment).unwrap(),
            Type::Number,
        );
    }

    #[test]
    fn test_addition() {
        let mut environment = TypeEnvironment::new();
        assert_eq!(
            type_check(
                Term::addition(Term::number(1), Term::number(2)),
                &mut environment,
            )
            .unwrap(),
            Type::Number,
        );

        let mut environment = TypeEnvironment::new();
        assert_eq!(
            type_check(
                Term::addition(Term::True, Term::number(1)),
                &mut environment,
            )
            .unwrap_err()
            .downcast::<TypeCheckError>()
            .unwrap(),
            TypeCheckError::UnexpectedType,
        );

        let mut environment = TypeEnvironment::new();
        assert_eq!(
            type_check(
                Term::addition(Term::number(1), Term::True),
                &mut environment,
            )
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
            type_check(Term::variable("a"), &mut environment).unwrap(),
            Type::Boolean,
        );

        let mut environment = TypeEnvironment::new();
        assert_eq!(
            type_check(Term::variable("a"), &mut environment)
                .unwrap_err()
                .downcast::<TypeCheckError>()
                .unwrap(),
            TypeCheckError::UnknownVariable,
        );
    }

    #[test]
    fn test_function() {
        let mut environment = TypeEnvironment::new();
        assert_eq!(
            type_check(
                Term::function(
                    vec![
                        Parameter::new("a", Type::Boolean),
                        Parameter::new("b", Type::Boolean),
                    ],
                    Term::True,
                ),
                &mut environment,
            )
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
        let mut environment = TypeEnvironment::new();
        assert_eq!(
            type_check(
                Term::call(
                    Term::function(
                        vec![
                            Parameter::new("a", Type::Boolean),
                            Parameter::new("b", Type::Boolean),
                        ],
                        Term::True,
                    ),
                    vec![Term::True, Term::False],
                ),
                &mut environment,
            )
            .unwrap(),
            Type::Boolean,
        );

        let mut environment = TypeEnvironment::new();
        assert_eq!(
            type_check(Term::call(Term::True, vec![]), &mut environment)
                .unwrap_err()
                .downcast::<TypeCheckError>()
                .unwrap(),
            TypeCheckError::UnexpectedType,
        );

        let mut environment = TypeEnvironment::new();
        assert_eq!(
            type_check(
                Term::call(
                    Term::function(
                        vec![
                            Parameter::new("a", Type::Boolean),
                            Parameter::new("b", Type::Boolean),
                        ],
                        Term::True,
                    ),
                    vec![Term::True],
                ),
                &mut environment,
            )
            .unwrap_err()
            .downcast::<TypeCheckError>()
            .unwrap(),
            TypeCheckError::InvalidArity,
        );

        let mut environment = TypeEnvironment::new();
        assert_eq!(
            type_check(
                Term::call(
                    Term::function(
                        vec![
                            Parameter::new("a", Type::Boolean),
                            Parameter::new("b", Type::Boolean),
                        ],
                        Term::True,
                    ),
                    vec![Term::True, Term::number(1)],
                ),
                &mut environment,
            )
            .unwrap_err()
            .downcast::<TypeCheckError>()
            .unwrap(),
            TypeCheckError::MismatchedTypes,
        );
    }
}
