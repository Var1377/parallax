use crate::runtime::types::*;
use crate::runtime::runtime::ParallelRuntime;

use super::Port;

pub trait ReductionEngine {
    fn reduce_call(&mut self, redex: Port, runtime: &ParallelRuntime) -> bool;
    fn reduce_erase(&mut self, redex: Port, runtime: &ParallelRuntime) -> bool;
    fn reduce_compute(&mut self, redex: Port, runtime: &ParallelRuntime) -> bool;
    fn reduce_switch(&mut self, redex: Port, runtime: &ParallelRuntime) -> bool;
    fn reduce_annihilate(&mut self, redex: Port, runtime: &ParallelRuntime) -> bool;
    fn reduce_commute(&mut self, redex: Port, runtime: &ParallelRuntime) -> bool;
}