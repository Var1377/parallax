use super::*;

/// Interaction rule for Number ~ Number.
/// This interaction is considered invalid/unreachable in the expected net configurations.
pub unsafe fn number_number(_n1: Port, _n2: Port, _read_guard: &parking_lot::RwLockReadGuard<AllPartitions>) {
    unreachable!("Number ~ Number interaction should not occur");
}

pub unsafe fn number_switch(n: Port, sw: Port, read_guard: &parking_lot::RwLockReadGuard<AllPartitions>) { unimplemented!() }
pub unsafe fn number_pointer(n: Port, p: Port, read_guard: &parking_lot::RwLockReadGuard<AllPartitions>) { unimplemented!() }
