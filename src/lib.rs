use anyhow::{Result, ensure};
use thiserror::Error;

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Boolean,
    Number,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Term {
    True,
    False,
    Condition {
        condition: Box<Term>,
        consequent: Box<Term>,
        alternative: Box<Term>,
    },
    Number {
        value: i64,
    },
    Addition {
        left: Box<Term>,
        right: Box<Term>,
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
}

#[derive(Debug, Clone, PartialEq, Error)]
pub enum TypeCheckError {
    #[error("unexpected type")]
    UnexpectedType,
    #[error("mismatched types")]
    MismatchedTypes,
}

pub fn type_check(term: Term) -> Result<Type> {
    match term {
        Term::True | Term::False => Ok(Type::Boolean),
        Term::Condition {
            condition,
            consequent,
            alternative,
        } => {
            let condition = type_check(*condition)?;
            ensure!(condition == Type::Boolean, TypeCheckError::UnexpectedType);

            let consequent = type_check(*consequent)?;
            let alternative = type_check(*alternative)?;
            ensure!(consequent == alternative, TypeCheckError::MismatchedTypes);

            Ok(consequent)
        }
        Term::Number { .. } => Ok(Type::Number),
        Term::Addition { left, right } => {
            let left = type_check(*left)?;
            ensure!(left == Type::Number, TypeCheckError::UnexpectedType);

            let right = type_check(*right)?;
            ensure!(right == Type::Number, TypeCheckError::UnexpectedType);

            Ok(Type::Number)
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{Term, Type, TypeCheckError, type_check};

    #[test]
    fn test_boolean() {
        assert_eq!(type_check(Term::True).unwrap(), Type::Boolean);
        assert_eq!(type_check(Term::False).unwrap(), Type::Boolean);
    }

    #[test]
    fn test_condition() {
        assert_eq!(
            type_check(Term::condition(
                Term::True,
                Term::number(1),
                Term::number(2),
            ))
            .unwrap(),
            Type::Number,
        );
        assert_eq!(
            type_check(Term::condition(Term::number(1), Term::True, Term::False))
                .unwrap_err()
                .downcast::<TypeCheckError>()
                .unwrap(),
            TypeCheckError::UnexpectedType,
        );
        assert_eq!(
            type_check(Term::condition(Term::True, Term::number(1), Term::False))
                .unwrap_err()
                .downcast::<TypeCheckError>()
                .unwrap(),
            TypeCheckError::MismatchedTypes,
        );
        assert_eq!(
            type_check(Term::condition(Term::True, Term::False, Term::number(1)))
                .unwrap_err()
                .downcast::<TypeCheckError>()
                .unwrap(),
            TypeCheckError::MismatchedTypes,
        );
    }

    #[test]
    fn test_number() {
        assert_eq!(type_check(Term::number(1)).unwrap(), Type::Number);
    }

    #[test]
    fn test_addition() {
        assert_eq!(
            type_check(Term::addition(Term::number(1), Term::number(2))).unwrap(),
            Type::Number,
        );
        assert_eq!(
            type_check(Term::addition(Term::True, Term::number(1)))
                .unwrap_err()
                .downcast::<TypeCheckError>()
                .unwrap(),
            TypeCheckError::UnexpectedType,
        );
        assert_eq!(
            type_check(Term::addition(Term::number(1), Term::True))
                .unwrap_err()
                .downcast::<TypeCheckError>()
                .unwrap(),
            TypeCheckError::UnexpectedType,
        );
    }
}
