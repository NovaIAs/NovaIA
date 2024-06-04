```rust
use std::collections::HashMap;
use std::io::{self, Write};
use std::ops::{Add, Sub, Mul, Div, Neg};

// Définissez un trait pour les opérations arithmétiques.
trait Arithmétique<T> {
    // Ajoutez deux valeurs.
    fn add(&self, rhs: T) -> T;
    // Soustrayez deux valeurs.
    fn sub(&self, rhs: T) -> T;
    // Multipliez deux valeurs.
    fn mul(&self, rhs: T) -> T;
    // Divisez deux valeurs.
    fn div(&self, rhs: T) -> T;
    // Prenez le négatif d'une valeur.
    fn neg(&self) -> T;
}

// Implémentez le trait Arithmétique pour le type i32.
impl Arithmétique<i32> for i32 {
    fn add(&self, rhs: i32) -> i32 { *self + rhs }
    fn sub(&self, rhs: i32) -> i32 { *self - rhs }
    fn mul(&self, rhs: i32) -> i32 { *self * rhs }
    fn div(&self, rhs: i32) -> i32 { *self / rhs }
    fn neg(&self) -> i32 { -*self }
}

// Implémentez le trait Arithmétique pour le type f64.
impl Arithmétique<f64> for f64 {
    fn add(&self, rhs: f64) -> f64 { *self + rhs }
    fn sub(&self, rhs: f64) -> f64 { *self - rhs }
    fn mul(&self, rhs: f64) -> f64 { *self * rhs }
    fn div(&self, rhs: f64) -> f64 { *self / rhs }
    fn neg(&self) -> f64 { -*self }
}

// Définissez une structure pour représenter une expression.
struct Expression {
    gauche: Box<Expression>,
    droite: Box<Expression>,
    opérateur: Opérateur,
}

// Définissez une énumération pour représenter les opérateurs.
enum Opérateur {
    Addition,
    Soustraction,
    Multiplication,
    Division,
    Négation,
}

// Implémentez le trait Arithmétique pour le type Expression.
impl Arithmétique<i32> for Expression {
    fn add(&self, rhs: Expression) -> Expression {
        Expression {
            gauche: Box::new(*self),
            droite: Box::new(rhs),
            opérateur: Opérateur::Addition,
        }
    }
    fn sub(&self, rhs: Expression) -> Expression {
        Expression {
            gauche: Box::new(*self),
            droite: Box::new(rhs),
            opérateur: Opérateur::Soustraction,
        }
    }
    fn mul(&self, rhs: Expression) -> Expression {
        Expression {
            gauche: Box::new(*self),
            droite: Box::new(rhs),
            opérateur: Opérateur::Multiplication,
        }
    }
    fn div(&self, rhs: Expression) -> Expression {
        Expression {
            gauche: Box::new(*self),
            droite: Box::new(rhs),
            opérateur: Opérateur::Division,
        }
    }
    fn neg(&self) -> Expression {
        Expression {
            gauche: Box::new(*self),
            droite: Box::new(Expression {
                gauche: Box::new(Expression {
                    gauche: Box::new(*self),
                    droite: Box::new(Expression {
                        gauche: Box::new(Expression {
                            gauche: Box::new(*self),
                            droite: Box::new(Expression {
                                gauche: Box::new(*self),
                                droite: Box::new(Expression {
                                    gauche: Box::new(*self),
                                    droite: Box::new(Expression {
                                        gauche: Box::new(*self),
                                        droite: Box::new(Expression {
                                            gauche: Box::new(*self),
                                            droite: Box::new(Expression {
                                                gauche: Box::new(*self),
                                                droite: Box::new(Expression {
                                                    gauche: Box::new(*self),
                                                    droite: Box::new(Expression {
                                                        gauche: Box::new(*self),
                                                        droite: Box::new(Expression {
                                                            gauche: Box::new(*self),
                                                            droite: Box::new(Expression {
                                                                gauche: Box::new(*self),
                                                                droite: Box::new(Expression {
                                                                    gauche: Box::new(*self),
                                                                    droite: Box::new(Expression {
                                                                        gauche: Box::new(*self),
                                                                        droite: Box::new(Expression {
                                                                            gauche: Box::