use super::*;

pub unsafe fn async_async(a1: Port, a2: Port, read_guard: &parking_lot::RwLockReadGuard<AllPartitions>) { unimplemented!() }
pub unsafe fn async_pointer(a: Port, p: Port, read_guard: &parking_lot::RwLockReadGuard<AllPartitions>) { unimplemented!() }
pub unsafe fn async_constructor(a: Port, c: Port, read_guard: &parking_lot::RwLockReadGuard<AllPartitions>) { unreachable!() }
pub unsafe fn async_number(a: Port, n: Port, read_guard: &parking_lot::RwLockReadGuard<AllPartitions>) { unreachable!() }
pub unsafe fn async_switch(a: Port, sw: Port, read_guard: &parking_lot::RwLockReadGuard<AllPartitions>) { unreachable!() }
pub unsafe fn async_static(a: Port, s: Port, read_guard: &parking_lot::RwLockReadGuard<AllPartitions>) { unreachable!() }
