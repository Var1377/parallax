use std::sync::atomic::{AtomicUsize, Ordering};
use std::num::NonZeroUsize;

// Thread-local storage for thread ID
thread_local! {
    static THREAD_ID: AtomicUsize = AtomicUsize::new(0);
}

pub fn init_thread_id(id: NonZeroUsize) {
    THREAD_ID.with(|tid| {
        tid.store(id.get(), Ordering::SeqCst);
    });
}

pub fn current_thread_id() -> u32 {
    const MAX_THREADS: u32 = 256;
    THREAD_ID.with(|tid| {
        tid.load(Ordering::SeqCst) as u32 % MAX_THREADS
    })
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Port(pub u64);

impl Port {
    pub fn new() -> Self {
        Port(0)
    }

    pub fn destination_agent_type(&self) -> u8 {
        (self.0 & 0xF) as u8
    }

    pub fn destination_port(&self) -> u8 {
        ((self.0 >> 4) & 0xF) as u8
    }

    pub fn thread_id(&self) -> u8 {
        ((self.0 >> 8) & 0xFF) as u8
    }

    pub fn local_index(&self) -> u64 {
        self.0 >> 16
    }

    pub fn set_destination_agent_type(&mut self, val: u8) {
        self.0 = (self.0 & !0xF) | (val as u64 & 0xF);
    }

    pub fn set_destination_port(&mut self, val: u8) {
        self.0 = (self.0 & !(0xF << 4)) | ((val as u64 & 0xF) << 4);
    }

    pub fn set_thread_id(&mut self, val: u8) {
        self.0 = (self.0 & !(0xFF << 8)) | ((val as u64 & 0xFF) << 8);
    }

    pub fn set_local_index(&mut self, val: u64) {
        self.0 = (self.0 & 0xFFFF) | (val << 16);
    }
} 