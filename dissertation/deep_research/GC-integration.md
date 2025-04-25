Technical Report: Implementation Strategies for Garbage Collection in the Parallax Project
Date: October 26, 2023
Prepared For: Parallax Development Team
Subject: Analysis of Garbage Collector Integration for JIT-Compiled Native Code
1. Introduction
1.1. Purpose and Scope
This report details an analysis of implementation strategies for integrating a garbage collector (GC) into the Parallax system. The primary objective is to manage memory allocated by or for native code generated via the Cranelift Just-In-Time (JIT) compiler. This includes runtime objects such as closure environments, boxed values, and other heap-allocated data structures required by the JIT-compiled code.
Crucially, the scope of this GC is strictly limited to the memory associated with the Cranelift JIT execution environment. The interaction net runtime (parallax-net), which utilizes Slab allocation for its nodes, is explicitly out of scope for this GC mechanism. However, the GC must handle references originating from parallax-net, specifically via parallax-net::node::Ref nodes storing AtomicU64 values that may point into the GC-managed heap.
The initial focus is on leveraging an existing Rust GC crate implementing a Stop-The-World (STW) collection algorithm (e.g., Mark-Sweep or Mark-Compact).
1.2. Background: The Parallax System
Parallax operates as a hybrid execution environment, combining two distinct components:
1. parallax-net: A high-performance interaction net runtime responsible for graph reduction. It employs Slab allocation for efficient management of its node structures.
2. Cranelift JIT: A code generator used to compile parts of the Parallax workload into native machine code for performance enhancement.
The need for a dedicated GC arises from the memory allocation requirements of the JIT-compiled native code. While parallax-net manages its own memory domain, the native code requires a mechanism to automatically reclaim heap-allocated objects like closures and boxed primitives.
1.3. Key Challenges
Integrating a GC in this hybrid environment presents several challenges:
* Selecting an appropriate Rust GC crate: Balancing maturity, performance, ease of integration, and feature set.
* Root Finding: Identifying all references (roots) into the GC heap originating from the native code (stacks, registers, globals) and the parallax-net runtime (Ref nodes).
* Cross-Boundary Interaction: Safely managing references from parallax-net (AtomicU64 in Ref nodes) into the GC heap, requiring robust tagging or identification schemes.
* Synchronization: Implementing a reliable STW mechanism that pauses both native JIT threads and parallax-net worker threads consistently.
* JIT Integration: Handling the complexities of GC interaction with dynamically generated code, including object layout, tracing, and potential performance impacts.
This report analyzes these challenges and provides recommendations for a viable implementation path.
2. Analysis of Rust Garbage Collection Crates
Several Rust crates offer garbage collection capabilities. This section analyzes potential candidates suitable for an initial STW implementation within Parallax, focusing on crates providing tracing GC algorithms.
2.1. Candidate Crates
Based on availability, maintenance status, and feature sets described in documentation and community discussions, the following crates are considered primary candidates:
* shredder: Provides Gc<T> smart pointers with a focus on concurrency and ergonomics.1
* gc (Manishearth/rust-gc): A simple, single-threaded mark-and-sweep collector.2
* broom: An ergonomic mark-and-sweep collector, primarily aimed at language runtimes.3
* safe-gc: A GC implementation notable for its complete absence of unsafe code, prioritizing safety over performance.4
* rsgc: A semi-conservative, concurrent mark-and-sweep GC inspired by Shenandoah, designed for language runtimes.5
* refuse: An incremental, multi-threaded, mark-and-sweep GC using Root<T> and Ref<T> types.7
Other crates exist (e.g., rust-cc 8, bacon_rajan_cc 9, gc-arena 10, dumpster 11), but often focus on cycle collection for Rc-like types or arena-based approaches, which may be less directly applicable to managing a general heap for JIT code requiring STW collection initially.
2.2. Core Algorithms and Concurrency
* shredder: Implements a tracing GC with background collection and destruction, aiming for limited STW pauses. It is designed for multi-threaded contexts and provides AtomicGc.1 The exact algorithm (Mark-Sweep vs. Mark-Compact) is unclear, but potential "bloat during collection" suggests it might not be compacting currently.1
* gc: Explicitly uses a Mark-Sweep algorithm.2 It is currently single-threaded and STW, although an experimental concurrent branch exists.2
* broom: Implements Mark-Sweep.3 The documentation doesn't explicitly confirm STW or thread-safety, but planned "partial cleans" suggest potential future improvements for reducing pauses.3
* safe-gc: The specific tracing algorithm isn't detailed, but it operates on distinct Heap instances. Concurrency details are not provided, suggesting a likely single-threaded, STW model.4
* rsgc: Implements a Concurrent Mark-Sweep algorithm. It features multiple GC modes (Concurrent, Degenerated STW, Full STW) triggered by heuristics.5 It is explicitly multi-threaded and designed for concurrency.5
* refuse: Implements an incremental Mark-Sweep algorithm. It is explicitly multi-threaded.7 While incremental, it still requires pausing threads briefly for collection phases.7
For Parallax's initial STW requirement, gc, broom, safe-gc, rsgc (in its STW modes), and refuse are potentially suitable. shredder's limited STW model might also fit, depending on the pause characteristics. rsgc and refuse offer the clearest paths towards future concurrent or incremental improvements.
2.3. Tracing Mechanisms
Tracing involves identifying reachable objects starting from roots. GC crates typically require user types to implement a specific trait for this.
* shredder: Uses a Scan trait, derivable with #. It appears designed for safety without requiring unsafe user code for tracing.1
* gc: Requires types to implement Trace and Finalize, often via #. The Trace trait itself is unsafe to implement manually, as incorrect implementations can lead to memory unsafety.2 Provides #[unsafe_ignore_trace] for fields known not to contain Gc pointers, but misuse is unsafe.2
* broom: Uses a Trace trait. The example implementation involves calling tracer.trace(handle) for contained Handles. An earlier version required unsafe, but this was removed for safety, potentially at some performance cost.3
* safe-gc: Uses a Trace trait where implementations call collector.edge(gc_ptr). Notably, implementing Trace is safe due to the crate's forbid(unsafe_code) guarantee.4 Incorrect implementations lead to "undesirable behavior" (like leaks) rather than UB.15
* rsgc: The documentation emphasizes the need for objects to be "properly traced" but doesn't specify a user-facing trait like Trace. It likely relies on internal mechanisms, possibly combined with the write barrier, for heap tracing.5 This requires further investigation of its source code.
* refuse: Requires types to implement a Collectable trait. The documentation asserts that implementing this trait incorrectly does not lead to UB, implying the trait definition itself is safe.7 A derive macro is not mentioned.7 Source code investigation is needed for specifics.17
The need for unsafe in Trace implementations (like in gc) increases the risk profile. safe-gc offers the highest safety guarantee here. shredder, broom, and refuse also aim for safe tracing interfaces. rsgc's approach needs clarification.
2.4. API Analysis
Key API aspects include root registration, allocation, and GC control.
* Root Registration:
   * shredder: Claims "no need to manually manage roots".1 Rooting seems implicit based on live Gc pointers, potentially managed via scopes like run_with_gc_cleanup.1 No explicit API for stack/global/external roots is mentioned.
   * gc: Rooting seems tied to GcCell usage internally.2 Explicit APIs for stack/global roots are not detailed in the overview.2
   * broom: Uses heap.insert() for rooted objects and heap.insert_temp() for temporary ones.3 Roots are explicitly managed via these allocation calls. No specific API for stack/global roots mentioned.3
   * safe-gc: Uses Root<T> to hold onto roots.4 Details on stack/global registration are absent.4
   * rsgc: Claims automatic stack rooting ("GC is able to discover objects on stack automatically").5 Provides Heap::add_global_root for explicit global/external roots.5 No mention of custom iterators.6
   * refuse: Uses Root<T> which automatically registers as a root.7 Requires a CollectionGuard to create roots.7 No explicit API for custom external root sources mentioned.7
* Allocation:
   * shredder: Gc::new(value).1
   * gc: Gc::new(value).2
   * broom: heap.insert(value) (rooted), heap.insert_temp(value) (temporary).3
   * safe-gc: heap.alloc(value).4
   * rsgc: Allocation happens in regions, with TLABs. API details not in overview, likely involves heap/thread handles.5
   * refuse: Root::new(value, &guard), Ref::new(value, &guard).7
* GC Control:
   * shredder: Background collection, likely automatic. run_with_gc_cleanup may trigger final collection.1
   * gc: Triggering mechanism not specified, likely automatic based on allocation thresholds.2
   * broom: Manual triggering via heap.clean().3
   * safe-gc: Automatic triggering, manual via heap.gc().4
   * rsgc: Triggered by heuristics (adaptive, compact) or potentially manually. Multiple GC modes (Concurrent, Degenerated, Full).5
   * refuse: Incremental collection runs automatically or can be yielded to via CollectorGuard::yield_to_collector() or triggered via refuse::collect().7
* Mutability:
   * shredder: Requires RefCell or similar for interior mutability, possibly using guards (DerefGc).1
   * gc: Requires GcCell (similar to RefCell).2
   * broom: Provides heap.get_mut(handle) for safe mutable access.3
   * safe-gc: Allows mutable access via &mut Heap.4
   * rsgc: Requires explicit Thread::write_barrier after mutation for concurrency safety.5
   * refuse: Root<T> implements Deref, implying standard interior mutability patterns (e.g., Mutex, RwLock) can be used, but requires care with CollectionGuard and locks.7
rsgc stands out with its automatic stack rooting and explicit global root API, which aligns well with the needs of a JIT environment. refuse's Root<T> is explicit but clear. shredder's automatic rooting needs validation. broom and safe-gc offer straightforward allocation and mutability APIs tied to the heap object.
2.5. Recommendations
* Primary Candidates: rsgc and refuse appear most promising due to their explicit support for multi-threading, concurrency features (even if STW is used initially), and mechanisms relevant to JIT integration (stack scanning/rooting, safepoints/guards).
   * rsgc: Strengths include semi-conservative stack scanning (reducing initial implementation burden compared to precise maps), explicit global root support, and built-in safepoint/write barrier mechanisms designed for runtimes.5 Weaknesses include potential complexity and the need to verify its tracing mechanism details.6
   * refuse: Strengths include its focus on safety, multi-threading, and incremental collection.7 The CollectionGuard mechanism provides explicit control points. Weaknesses include the potential overhead of CollectionGuard and the explicitness of Root<T> management.7
* Secondary Candidate: shredder could be considered if its "limited STW" proves sufficient and its automatic rooting is reliable for JIT-generated code.1 Its focus on ergonomics is appealing, but the lack of explicit control over roots might be a drawback.
* Less Suitable (Initially):
   * gc: Primarily single-threaded currently, limiting future scalability.2 The unsafe Trace requirement adds risk.
   * broom: Lacks explicit concurrency features in documentation and performance is not a primary goal.3
   * safe-gc: Explicitly not performance-oriented, which is likely unsuitable for a JIT GC.4
The final choice should involve prototyping, particularly focusing on the integration complexity of rooting and synchronization mechanisms with Cranelift and parallax-net.
3. Native Code Root Finding
A critical task for the GC is identifying all pointers (roots) into the GC heap that originate from the native code execution environment (JIT-compiled code). These roots can reside on the stack, in registers, or in global variables.
3.1. Conservative Stack Scanning
Conservative scanning treats any value on the stack or in registers that looks like a pointer into the GC heap as a potential root.18
* Techniques & Libraries:
   * The core idea is to iterate through the memory range occupied by a thread's stack and potentially its register save area. Each word-sized value is checked against the bounds of the GC heap.
   * Libraries like stackscanner exist in the C/C++ world (e.g., Boehm GC uses this 18), but mature, dedicated Rust crates specifically for standalone conservative stack scanning seem less common or were not identified in the provided snippets (searches for stackscanner yielded unrelated crates like rustscan (port scanner) 21 or cargo-scan (auditing tool) 23).
   * GC crates like rsgc implement this internally ("GC is able to discover objects on stack automatically").5 The Alloy GC project also uses the Boehm-Demers-Weiser (BDWGC) conservative collector underneath.20
* Register Handling: Registers holding potential pointers must also be scanned. This typically involves saving the register state to the stack at safepoints before scanning that stack area. The exact mechanism depends on the architecture and calling conventions.
* Accuracy Limitations: Conservatism's main drawback is potential false positives: non-pointer data (e.g., integers, floating-point numbers) might coincidentally have bit patterns matching valid heap addresses.18 This can lead to retaining garbage objects unnecessarily (memory leaks) and increased GC work tracing dead objects.19 This can be particularly problematic on 32-bit systems or with certain data structures.25
* Performance Overhead: Stack scanning involves reading potentially large memory regions, performing pointer checks, and validating potential pointers. This adds overhead during the GC pause. However, it avoids the compile-time and runtime overhead associated with generating and maintaining precise stack maps.19 Some argue conservative scanning can even be faster in certain scenarios due to avoiding runtime map lookups and potentially allowing more compiler optimizations.19
* Safety Concerns: While generally safe in Rust if implemented correctly within the GC library (avoiding direct dereferencing of potential pointers until validated), the main risk is retaining dead objects (leaks), not memory corruption from misinterpreting data as pointers during the scan itself. However, techniques like NaN-boxing or pointer tagging can be incompatible with naive conservative scanning if the GC isn't aware of the encoding.18
3.2. Precise Stack Scanning (Stack Maps)
Precise scanning relies on metadata (stack maps) generated by the compiler, which explicitly lists the locations (stack slots, registers) containing live GC pointers at specific points in the code (safepoints).26
* Requirements:
   * The compiler (Cranelift) must be modified or configured to generate these stack maps alongside the native code.
   * Stack maps associate specific program counter (PC) locations (safepoints) with information about live GC references in stack slots and registers at that point.26
* Cranelift/Wasmtime Support:
   * Wasmtime, which uses Cranelift, has support for stack maps, driven by the needs of the WebAssembly GC proposal.26
   * Cranelift has undergone evolution in its stack map generation. An older approach generated maps during register allocation, but this proved complex and interfered with optimizations.26
   * The current approach uses "User Stack Maps".26 The frontend (the code translating from a higher-level representation to Cranelift IR) is responsible for explicitly spilling GC references to virtual stack slots before a safepoint and annotating the safepoint instruction (e.g., a call) with metadata listing which slots contain live GC references.26
   * Cranelift provides IR constructs like ir::UserStackMap and ir::UserStackMapEntry 28 and API methods like MachBuffer::push_user_stack_map 28 to support this.
   * Wasmtime stores this information in a custom ELF section (.wasmtime.stackmap).30
* Feasibility & Complexity:
   * Leveraging User Stack Maps avoids needing deep changes throughout the Cranelift optimizer, placing the burden on the frontend generating the IR.26
   * However, correctly generating the spills, reloads, and annotations in the frontend is non-trivial and requires careful management of GC reference lifetimes.
   * Debugging stack map generation can be challenging, and bugs can lead to subtle GC correctness issues (use-after-free) if roots are missed.31
   * It requires significant integration effort within the Parallax JIT infrastructure that translates the source language or bytecode into Cranelift IR.
* Benefits: Precise scanning eliminates false positives, leading to more accurate garbage collection and potentially better memory efficiency compared to conservative scanning.24 It is essential for moving collectors (like compacting or generational GCs) as the GC needs to reliably update all references to moved objects.26
3.3. Global Root Registration
GC roots can also exist in global or static variables. These need to be explicitly registered with the GC.
* Standard Practices: Most GC systems provide an API to register global roots.
   * The GC maintains an internal list of these registered locations.
   * During collection, the GC iterates through this list, treating the values stored at these locations as roots.
* Rust GC Crate APIs:
   * rsgc: Provides Heap::add_global_root(object).5
   * pgc (related to rsgc): Explicitly requires add_root(gc_ptr) and remove_root(gc_ptr) as it cannot scan the stack.33
   * refuse: Uses Root<T> which automatically registers.7 Static Root<T> instances would likely serve as global roots.
   * Other crates (shredder, gc, broom, safe-gc): Specific APIs for global roots are not highlighted in the provided summaries but are likely present or achievable via their root types (e.g., static Root<T> in safe-gc).
* Implementation: Parallax would need to identify all global variables in the JIT runtime environment that can hold Gc<T> pointers and use the chosen GC crate's API to register them upon initialization. Unregistration might be needed if globals are dynamically managed. The concept of "global registration" in Rust is also being discussed for broader use cases, potentially offering future language-level mechanisms.34
3.4. Comparison and Recommendation
* Conservative Scanning: Simpler initial implementation, especially if using a crate like rsgc that provides it. Avoids complex compiler integration. Main risks are potential memory leaks due to false positives and incompatibility with moving collectors. Performance overhead exists but avoids stack map generation costs.
* Precise Scanning (Stack Maps): More complex to implement, requiring significant frontend work with Cranelift's User Stack Map API. Eliminates false positives, enabling better memory efficiency and compatibility with moving collectors. Higher initial development cost but potentially better long-term performance and accuracy.
Recommendation: Start with conservative stack scanning, leveraging a crate like rsgc if chosen. This minimizes initial complexity. Profile carefully to assess the impact of false positives and scanning overhead. If accuracy or performance becomes a significant issue, or if a moving collector is desired later, plan for a potential migration to precise scanning using Cranelift User Stack Maps, acknowledging the substantial R&D effort required. For global variables, use the explicit registration API provided by the chosen GC library.
4. Ref Node Interaction and Tagging
A key challenge is managing references from the parallax-net runtime into the GC heap. These references are stored as AtomicU64 values within parallax-net::node::Ref nodes. The GC needs to identify these AtomicU64 values that represent GC pointers during its root scanning phase.
4.1. Tagging Strategies for AtomicU64
Since AtomicU64 can store arbitrary 64-bit data (GC pointers, numbers, potentially other encoded values like Ports), a mechanism is needed to distinguish GC pointers. Tagging involves using some bits within the u64 to encode the type of data it holds.
* Low-Bit Tagging:
   * Concept: Assumes GC pointers are aligned to at least 2, 4, or 8 bytes. The lower 1, 2, or 3 bits of a valid pointer will always be zero. These bits can be used as tags. For example, a 0 in the lowest bit could signify a GC pointer, while a 1 could signify an immediate integer (potentially shifted).
   * Pros: Simple to implement; checking the tag is a fast bitwise operation. Pointer access requires masking off the tag bits. Common in Lisp systems.36 Compatible with atomic operations if care is taken.
   * Cons: Requires the allocator for GC objects to guarantee alignment. Reduces the range of immediate integers that can be stored if using lower bits for tags. The number of available tag bits is small (typically 1-3).
* High-Bit Tagging:
   * Concept: On modern 64-bit architectures (like x86-64, ARM64), canonical addresses often don't use all 64 bits. Typically, only the lower 48 or 57 bits are significant, and the upper bits must be sign-extended from the highest significant bit.36 These unused high bits can potentially be used for tagging.
   * Pros: Potentially offers more tag bits (e.g., 16 bits if using 48-bit pointers).36 Allows storing full 64-bit integers untagged if pointers have a specific high-bit tag.
   * Cons: Less portable, as the number of unused bits varies by architecture and OS configuration. Future architectures might use more address bits, breaking the scheme.36 Pointer access requires masking/sign-extension logic. May interfere with CPU address validation features.
* NaN-Boxing (for f64):
   * Concept: Exploits the fact that IEEE 754 double-precision floats have many bit patterns representing Not-a-Number (NaN). Pointers (typically up to 48-52 bits) and other small values (like integers, booleans) can be encoded within these NaN patterns.36
   * Pros: Allows storing f64 values directly without tagging overhead. Widely used in JavaScript engines.39
   * Cons: Not directly applicable to AtomicU64. This technique is specific to f64 representation. Applying it would require storing values as f64 in the Ref nodes, which is not the current design. Also involves complex bit manipulation for encoding/decoding and potential performance overhead on non-float operations.36 Requires careful handling of actual NaN values.
* Descriptor-Based Tagging:
   * Concept: Instead of tagging the pointer/value itself, the u64 could store a pointer to a descriptor object. The descriptor contains the actual value/pointer and its type information.
   * Pros: Highly flexible, allows storing arbitrary types. Avoids bit-fiddling with the primary value.
   * Cons: Introduces an extra level of indirection for every access, significantly impacting performance. Requires managing the lifecycle of descriptor objects. Not suitable for AtomicU64 if the goal is to store pointers or immediate values directly.
Recommendation for Parallax: Given the use of AtomicU64, low-bit tagging appears the most practical and portable approach, assuming the GC allocator used for JIT objects can guarantee sufficient alignment (e.g., 8-byte alignment provides 3 tag bits). This avoids architecture-specific assumptions of high-bit tagging.
4.2. Tagging and Atomic Operations
Using tagged values within AtomicU64 requires careful consideration of atomic operations.
* Atomicity: Standard atomic operations (load, store, compare_exchange, swap, etc.) operate on the entire u64 value.41 They will work correctly on the tagged u64 as a whole.
* Safety:
   * Correctness: Code using atomic operations must correctly encode/decode tags before/after the operation if it needs to interpret the value. For example, a compare_exchange must compare against the fully tagged expected value.
   * ABA Problem: Tagging does not inherently solve the ABA problem. If a location changes from tagged pointer A -> tagged integer X -> tagged pointer A, a compare_exchange might incorrectly succeed. This is a general concurrency issue, not specific to tagging itself.
   * Performance: Tagging/untagging (masking, shifting) adds overhead around atomic operations. However, atomic operations themselves are relatively expensive, especially under contention, so the bit manipulation overhead might be minor in comparison.42
* Example (compare_exchange with low-bit tag):


Rust




use std::sync::atomic::{AtomicU64, Ordering};

const POINTER_TAG: u64 = 0b00; // Assuming 2 tag bits, pointer has lower 2 bits zero
const INTEGER_TAG: u64 = 0b01; // Example tag for integer
const TAG_MASK: u64 = 0b11;

fn is_gc_pointer(value: u64) -> bool {
   (value & TAG_MASK) == POINTER_TAG
}

fn tag_pointer(ptr_addr: usize) -> u64 {
   // Assumes ptr_addr is already aligned, lower bits are 0
   ptr_addr as u64 // Implicitly has POINTER_TAG if aligned
}

fn untag_pointer(tagged_value: u64) -> Option<usize> {
   if is_gc_pointer(tagged_value) {
       Some(tagged_value as usize) // Assumes tag bits were zero
   } else {
       None
   }
}

fn tag_integer(int_val: i64) -> u64 {
   // Example: shift integer left, add tag
   ((int_val << 2) as u64) | INTEGER_TAG
}

// --- Atomic Operation Example ---
fn atomic_update_if_pointer(atomic_val: &AtomicU64, expected_ptr: usize, new_val: u64) -> Result<u64, u64> {
   let expected_tagged = tag_pointer(expected_ptr);
   // CAS operates on the full tagged value
   atomic_val.compare_exchange(
       expected_tagged,
       new_val, // new_val must also be correctly tagged
       Ordering::SeqCst,
       Ordering::SeqCst,
   )
}

4.3. Scanning Ref Nodes for Roots
During an STW pause, the GC must scan all parallax-net::node::Ref nodes across all runtime partitions to find GC pointers.
* Challenge: Potentially millions of Ref nodes could exist across many partitions/slabs managed by parallax-net. Iterating and checking each one individually during a GC pause could be prohibitively slow.
* Efficient Scanning Strategies:
   * parallax-net Cooperation: The most viable approach requires explicit support from parallax-net. It needs to provide an efficient way for the GC to iterate only over the Ref nodes (or potentially just their AtomicU64 storage) during a pause.
   * Specialized Data Structures: parallax-net could maintain auxiliary data structures (e.g., per-partition lists or bitmaps) specifically tracking which slab slots contain Ref nodes holding potential GC pointers. This would allow the GC to skip scanning irrelevant nodes. Updates to this structure would be needed whenever a Ref node is created, destroyed, or potentially modified to hold/release a GC pointer. This adds overhead to parallax-net operations but speeds up GC scanning.
   * Batching/Parallelism: If multiple parallax-net partitions exist and can be accessed independently, the scanning process could potentially be parallelized across GC worker threads during the pause.
* Scanning Logic:
   1. Initiate STW pause, synchronizing all JIT and parallax-net threads.
   2. Obtain access to parallax-net's node storage (slabs/partitions).
   3. Iterate efficiently through all potential Ref node locations (using parallax-net's optimized iteration support).
   4. For each AtomicU64 found:
      * Load the value atomically (e.g., Ordering::Relaxed is likely sufficient during STW).
      * Check the tag bits to see if it represents a GC pointer.
      * If it is a GC pointer, untag it to get the raw address.
      * Validate the address (ensure it falls within the GC heap bounds).
      * If valid, pass the pointer to the GC's marking routine (e.g., gc_mark(ptr)).
   5. Complete other root scanning (stacks, globals).
   6. Perform GC mark and sweep/compact phases.
   7. Resume all threads.
Recommendation: Collaborate closely with the parallax-net team to design and implement an efficient iteration mechanism specifically for scanning Ref node contents during a GC pause. Relying on naive iteration over all nodes is unlikely to be performant. Investigate auxiliary tracking structures within parallax-net if direct iteration proves too slow.
5. Stop-The-World (STW) Synchronization
A fundamental requirement for the initial GC implementation is a robust Stop-The-World (STW) mechanism. This involves safely pausing all application threads (mutators) that could potentially interact with the GC heap before collection begins and resuming them afterwards. In Parallax, this includes both native threads executing JIT-compiled code and the worker threads managed by parallax-net.
5.1. Synchronization Techniques
Achieving STW requires coordinating threads to reach a safe state (a "safepoint") where they are not actively manipulating GC-managed objects or pointers.
* Global Lock/Flag: A simple approach involves a global atomic flag or mutex. The GC thread sets the flag to request a pause. Mutator threads periodically check this flag at designated safepoints. If the flag is set, they block (e.g., on a condition variable) until the GC thread clears the flag after collection.
* Signal-Based Pausing (e.g., POSIX signals): On Unix-like systems, signals (like SIGUSR1 or SIGSTOP/SIGCONT) could potentially be used to interrupt and pause threads. However, this is complex to implement correctly and safely, especially ensuring threads are paused at consistent states and handling signal interactions with other system operations. It's generally less portable and more intrusive than cooperative polling.
* Dedicated Safepoint Libraries: Crates might exist to abstract STW synchronization, although none were explicitly identified as mature solutions for this specific cross-runtime scenario in the provided snippets. rsgc includes its own safepoint mechanism (Thread::safepoint) 5, suggesting GCs often bundle this logic. Searches for "safepoint library crate" yielded general crate/module information 43 or the rsgc implementation.5
5.2. Defining and Triggering Safepoints
Mutator threads must check for pending GC pauses at well-defined points (safepoints).
* Safepoint Locations: Common locations include:
   * Allocation Points: Checking before allocating memory on the GC heap is natural, as allocation often triggers GC anyway.
   * Function Prologues/Epilogues: Checking upon entering or exiting functions.
   * Loop Back-Edges: Checking within long-running loops to prevent indefinite delays in reaching a safepoint.5
   * Blocking Calls: Before making potentially blocking system calls or interacting with external systems.
* Implementation in JIT Code:
   * Cranelift needs to emit safepoint checks (e.g., loading a global flag and conditionally branching to a pause handler) at appropriate locations in the generated native code. This requires integration with the JIT frontend or potentially Cranelift backend modifications if not using a GC-aware crate like rsgc.
   * The rsgc crate requires explicit calls to Thread::safepoint() within the code that will be JIT-compiled (or the code calling it).5
* Implementation in parallax-net:
   * The parallax-net worker threads must also incorporate safepoint checks within their main execution loops (e.g., before processing a new interaction rule or batch of work).
   * Integration with the parallax-net scheduler or event loop is crucial.
* Triggering Mechanism:
   * The GC decides when to trigger a pause (e.g., based on allocation thresholds, timers, or manual requests).
   * It sets the global "pause requested" flag.
   * It waits until all registered mutator threads acknowledge the pause by blocking at a safepoint.
   * Once all threads are paused, the GC performs its collection cycle.
   * After collection, the GC clears the flag and signals waiting threads to resume.
5.3. Challenges in a Hybrid Environment
* Heterogeneous Threads: Synchronizing native JIT threads (potentially managed directly or via a pool) and parallax-net's worker threads requires a unified mechanism that both types of threads participate in.
* Safepoint Latency: Ensuring all threads reach a safepoint promptly is critical to minimize STW pause times. Long-running computations without safepoint checks can significantly delay GC.
* Correctness: Ensuring threads pause at states where GC object graphs are consistent and no pointers are held in ways invisible to the root-finding mechanism (e.g., only partially updated pointers).
Recommendation: Leverage the safepoint mechanism provided by the chosen GC crate if available (e.g., rsgc's Thread::safepoint and associated synchronization). If building a custom mechanism, use a cooperative polling approach with an AtomicBool flag and condition variables for blocking/waking threads. Carefully instrument both JIT-generated code (via the frontend or backend) and parallax-net worker loops with safepoint checks at appropriate locations (allocations, loop back-edges, function entries).
6. Integration Challenges and Best Practices
Integrating a third-party GC library, especially into a custom runtime involving JIT compilation and interactions with another runtime like parallax-net, presents several potential pitfalls and requires adherence to best practices.
6.1. Common Pitfalls
* Incorrect Rooting: Failing to identify all roots (on stacks, in registers, globals, or external references like Ref nodes) is a primary cause of bugs. This can lead to premature collection of live objects and subsequent use-after-free errors.15 Conservative scanning might mitigate missing roots but can cause leaks.19
* Mismatched Finalization/Drop Semantics: Assuming Rust's Drop trait will work seamlessly for GC objects can lead to resource leaks (if Drop doesn't run when expected) or incorrect behavior (if Drop runs non-deterministically or accesses already collected objects).2 Relying on GC-specific finalization mechanisms (Finalize trait) is often necessary but requires careful implementation.2
* Write Barrier Issues (Concurrent GC): For concurrent or incremental GCs, failing to correctly use write barriers when mutating GC object fields can lead to the GC missing newly created references and incorrectly collecting live objects.5
* Safepoint Issues: Insufficient safepoint placement can lead to long STW pauses.48 Incorrect synchronization logic can cause deadlocks or race conditions between mutators and the GC.7
* Data Races with Interior Mutability: Using standard Rust cell types (RefCell, Mutex) within GC objects requires care. Accessing data via these cells must often be coordinated with GC operations (e.g., holding a CollectionGuard in refuse 7) to prevent races or deadlocks. Some GCs provide their own cell types (gc's GcCell 2).
* JIT Compiler Interaction: Ensuring the JIT compiler (Cranelift) generates code compatible with the GC's assumptions (e.g., regarding object layout, pointer representation, safepoint checks, stack map generation if used) is complex.26 Miscompilations can lead to subtle GC bugs.31
* Cross-Heap Pointers: Incorrectly managing pointers between the GC heap and other memory regions (like parallax-net's slabs or the native stack/heap) can cause crashes or corruption. Tagging helps manage pointers stored in Ref nodes.
6.2. Interfacing and Allocation Wrappers
* Allocator Interface: JIT-compiled code needs a way to allocate memory on the GC heap. This typically involves:
   * Defining functions callable from JIT code (e.g., gc_alloc(size, type_info) -> *mut GcObject).
   * These functions wrap the chosen GC crate's allocation API (e.g., heap.alloc(), Gc::new()).
   * The wrapper might need to handle object initialization, potentially storing type metadata needed by the GC.
* GC Pointer Representation: The JIT needs to understand how to represent and manipulate GC pointers (Gc<T>, Handle, Root<T>, etc.). This might involve treating them as opaque pointers (*mut u8 or usize) in the JIT IR and ensuring they are correctly passed to/from runtime functions and handled by the root finding mechanism.
* Lifetime Management: While the GC handles reclamation, the JIT runtime must still manage the logical lifetime of references, ensuring roots are maintained as long as an object might be accessed via native code.
6.3. Tracing Complex Native Objects (Closures)
Implementing the Trace/Scan/Collectable trait for complex objects generated or used by the JIT, such as closure environments, requires careful attention.
* Challenge: Rust closures are problematic because their internal layout (captured variables) is not stable or introspectable, making automatic tracing difficult or impossible.50 If a Rust closure captures a Gc<T> pointer, generic GC libraries often cannot find it unless special care is taken.50
* Implementation Strategies:
   * Avoid Capturing Gc<T>: The simplest approach is to design the JIT interface so that closures do not capture Gc<T> pointers directly. Required GC-managed data should be passed as arguments when the closure is called.
   * Custom Closure Structures: Define custom structs within the JIT runtime to represent closure environments. These structs would have explicit fields for captured values (including any Gc<T> pointers). The Trace trait can then be implemented (manually or potentially derived) for these custom structs, explicitly tracing the Gc<T> fields.50
   * Manual Rooting/Leaking: If Rust closures must capture Gc<T>, a potential (but risky) workaround is to ensure the captured Gc<T> remains rooted elsewhere (e.g., in a global list) for the closure's lifetime. This often leads to memory leaks if not managed perfectly.50 Using unsafe_ignore_trace on closure fields is highly dangerous.50
* Best Practice: Define explicit, traceable structures for closure environments used by the JIT code if they need to contain GC pointers. Avoid relying on the GC to trace inside standard Rust closures.
6.4. Finalization vs. Drop
A critical aspect often overlooked is the difference between Rust's deterministic Drop trait and the non-deterministic nature of GC finalization. Integrating a GC often involves deciding how object cleanup is handled, and simply relying on Drop being called by the GC can be problematic.20
* The Problem: Drop in Rust guarantees resource cleanup at a predictable point (end of scope). GC finalization happens at an unpredictable time after an object becomes unreachable.20 Furthermore, the environment during finalization might be constrained (e.g., other objects may already be collected, locks might be held). Premature finalization (before the object is truly dead from the program's perspective) is also a known issue in GC systems.20
* Implications: If GC-managed objects hold resources requiring deterministic cleanup (files, sockets, mutex guards), relying on the GC to call Drop is unsafe. The resource might be held indefinitely, or cleanup might fail due to the finalization environment.
* Solutions:
   1. Avoid Drop for GC types: Some GC crates (gc) explicitly prevent Drop on traced types and require a separate Finalize trait.2
   2. Safe Drop Integration: Some crates (safe-gc) claim safe Drop integration, likely by ensuring Drop cannot access the GC heap or resurrect objects.4
   3. Explicit Finalization: Use the GC's provided finalization mechanism (Finalize trait, etc.) for cleanup logic, understanding its non-deterministic nature.
   4. Manual Resource Management: Do not store resource handles directly within GC objects. Manage them separately using standard Rust RAII patterns outside the GC heap, potentially linking them via non-GC pointers or IDs if necessary. The Alloy GC project explores advanced techniques like finalizer elision and running finalizers on separate threads to mitigate some issues.20
Recommendation: Be extremely cautious about resources held within GC objects. Avoid placing objects implementing Drop for critical resource cleanup directly into Gc<T>. If cleanup is needed, use the specific finalization mechanism provided by the chosen GC crate and design it to be robust against non-determinism. Prefer managing sensitive resources outside the GC heap.
7. Comparative Analysis and Recommendations for Parallax
Synthesizing the analysis of GC crates, root-finding methods, and integration challenges allows for a comparative evaluation and specific recommendations for the Parallax project.
7.1. Evaluation Criteria Revisited
The key criteria for selecting and implementing a GC strategy for Parallax are:
1. STW Suitability (Initial): Must support a Stop-The-World collection model initially.
2. Performance: Must be efficient enough for a JIT environment, minimizing pause times and overhead. Potential for future concurrent/incremental operation is a plus.
3. Integration Complexity (Cranelift): Effort required to integrate root finding (stack, registers, globals) with Cranelift-generated code.
4. Integration Complexity (parallax-net): Effort required to handle Ref node roots and STW synchronization with parallax-net workers.
5. Rooting Accuracy: Precision of root finding (conservative vs. precise) impacting memory usage and compatibility with moving collectors.
6. Memory Efficiency: Overhead of GC metadata, potential for fragmentation (Mark-Sweep) vs. compaction (Mark-Compact).
7. Safety/Risk Profile: Guarantees against memory unsafety, complexity of unsafe code required for integration (especially tracing).
8. Maturity/Maintenance: Stability, documentation, community support, and active development.
7.2. Evaluation Matrix
The following table evaluates potential strategies based on the criteria above. Performance and Complexity ratings are estimates based on design principles and documentation. (Ratings: Low, Medium, High; Higher is better for Performance/Accuracy/Efficiency/Maturity; Lower is better for Complexity/Risk).
Strategy
	STW Suitability
	Est. Performance
	Integ. Complex. (Cranelift)
	Integ. Complex. (parallax-net)
	Rooting Accuracy
	Memory Efficiency
	Safety/Risk Profile
	Maturity
	rsgc + Conservative Scan
	High
	Medium
	Medium
	Medium
	Medium
	Medium
	Medium
	Medium
	refuse + Root<T>
	High
	Medium-High
	Low
	Medium
	High
	Medium
	Low
	Medium
	shredder + Auto-Rooting
	Medium
	Medium-High
	Low?
	Medium
	Medium?
	Medium?
	Low
	Medium
	Precise Scan + User Maps
	High
	High
	High
	Medium
	High
	High
	Medium
	Low*
	Notes:
* Precise Scan + User Maps: Represents a hypothetical strategy using a suitable STW or incremental crate combined with implementing precise rooting via Cranelift User Stack Maps. Maturity is "Low" as it requires significant custom integration. Rooting Accuracy/Memory Efficiency are potentially High, but depend heavily on implementation quality. Safety/Risk is Medium due to the complexity of stack map generation.
* rsgc: Conservative scanning simplifies Cranelift integration initially but lowers accuracy/efficiency vs. precise. Requires careful implementation of safepoints/barriers. Risk profile is Medium due to likely internal unsafe and complexity.
* refuse: Explicit Root<T> simplifies rooting logic but requires manual management. CollectionGuard adds overhead but provides clear synchronization points. Safety focus is a plus. Incremental nature is good for future latency.
* shredder: Automatic rooting needs validation in a JIT context. "Limited STW" and concurrency focus are positives. Memory efficiency concern noted ("bloat").
* parallax-net Complexity: Assumed Medium for all, as Ref node scanning and STW synchronization require significant parallax-net cooperation regardless of the GC crate.
7.3. Recommended GC Crate(s)
Based on the analysis and evaluation:
1. Primary Recommendation: rsgc 5
   * Rationale: Designed for language runtimes, explicitly supports concurrent/multi-threaded operation (good for future), provides built-in semi-conservative stack scanning (lowering initial barrier to entry vs. precise maps), and includes necessary primitives like safepoints and write barriers needed for JIT integration. Its explicit Heap::add_global_root API is suitable.
   * Caveats: Requires careful integration of its Thread context, safepoints, and write barriers. Documentation on internal tracing details seems sparse, requiring source code investigation. Performance characteristics need benchmarking in the Parallax context.
2. Alternative Recommendation: refuse 7
   * Rationale: Strong safety focus, multi-threaded and incremental design (good latency potential). Explicit Root<T> and CollectionGuard provide clear control points, potentially simplifying reasoning about lifetimes and synchronization compared to implicit mechanisms.
   * Caveats: Requires manual root management via Root<T>. The performance overhead of CollectionGuard needs evaluation, especially for frequent access to Ref<T> data from JIT code. Lacks automatic stack scanning.
shredder remains a possibility if its automatic rooting proves sufficient and performance is acceptable, but rsgc and refuse offer more explicit mechanisms suited to the complexities of a JIT runtime integration.
7.4. Recommended Implementation Strategy
* Root Finding:
   * Initial: Use conservative stack scanning (provided by rsgc if chosen, or implement based on its principles if using refuse or another crate). This avoids the high initial cost of precise maps.
   * Long-term: If conservative scanning proves inadequate (performance bottlenecks, excessive memory retention due to false positives), invest in precise stack maps using Cranelift User Stack Maps. This requires significant R&D but offers the best accuracy and efficiency.
   * Globals: Use explicit registration via the chosen GC's API (e.g., rsgc::Heap::add_global_root).
   * Ref Nodes: Implement efficient scanning in collaboration with parallax-net, potentially using auxiliary tracking structures.
* Tagging: Use low-bit tagging for the AtomicU64 in Ref nodes. Ensure the JIT's GC allocator guarantees sufficient pointer alignment (e.g., 8 bytes). Implement careful tag/untag logic around atomic operations.
* Synchronization: Leverage the chosen GC's safepoint mechanism (rsgc's Thread::safepoint or refuse's CollectionGuard interaction points). Ensure safepoints are inserted diligently in both JIT-generated code (via frontend instrumentation) and parallax-net worker loops.
* Tracing: Implement the required Trace/Collectable trait for all GC-managed types used by the JIT. Pay special attention to custom, traceable structures for closure environments if they need to capture Gc<T> pointers. Avoid relying on tracing standard Rust closures.
* Finalization: Strictly avoid placing objects with critical Drop logic into Gc<T>. Use the GC's specific finalization mechanism if necessary for non-critical cleanup, or manage resources outside the GC heap.
8. Potential Challenges and Required R&D
Implementing the recommended GC strategy involves significant technical challenges and areas requiring further investigation or prototyping.
8.1. Major Implementation Hurdles
* Root Finding Implementation:
   * Precise: Correctly generating User Stack Map annotations in the JIT frontend for all relevant GC pointers across all code paths is complex and error-prone.26 Requires deep understanding of both the source language semantics and Cranelift IR.
   * Conservative: Achieving acceptable performance and accuracy requires careful implementation, especially handling register values and potential interactions with Cranelift optimizations that might obscure pointers.19
* Ref Node Scanning Performance: Iterating potentially millions of Ref nodes during an STW pause is a major performance risk. Designing an efficient scanning mechanism with parallax-net is critical and non-trivial.
* STW Synchronization Robustness: Ensuring all JIT and parallax-net threads reliably reach safepoints without deadlocks or excessive latency, especially under heavy load or during complex interactions, is challenging.5
* Closure Tracing: Developing a robust and performant solution for handling captured Gc<T> pointers in JIT closure environments, likely involving custom data structures, is a significant design task.50
* Integration Testing: Thoroughly testing the GC integration across diverse workloads, including edge cases involving concurrency, JIT compilation, and parallax-net interaction, will be essential to ensure correctness and stability.
8.2. Areas for Further Research or Prototyping
* Stack Map Feasibility: If precise scanning is considered long-term, build a small prototype integrating Cranelift User Stack Map generation for a representative subset of JIT code constructs to gauge complexity and feasibility.
* Conservative Scanning Benchmarks: Implement and benchmark a conservative stack scanner (potentially adapting rsgc's logic) with realistic JIT code generated by Cranelift. Measure pause times and estimate memory retention overhead due to false positives.
* Tagging Overhead: Benchmark the performance impact of low-bit tagging/untagging on AtomicU64 operations within Ref nodes under simulated concurrent access patterns.
* Ref Node Scan Prototype: Develop and benchmark the proposed efficient Ref node scanning mechanism in coordination with parallax-net.
* Safepoint Strategy Evaluation: Experiment with different safepoint placement strategies (e.g., allocation points vs. loop back-edges vs. function entries) in JIT code and parallax-net loops to measure their impact on STW pause latency and application throughput.
* GC Crate Evaluation: Prototype basic integration with the top candidate crates (rsgc, refuse) to better understand their APIs, complexity, and suitability for the Parallax architecture.
8.3. Illustrative Pseudo-code Snippets


Rust




// --- Tagging Check (Low-bit, 2 tag bits) ---
const POINTER_TAG: u64 = 0b00;
const TAG_MASK: u64 = 0b11;

// Represents the opaque type managed by the GC
struct GcObject;

fn is_gc_pointer(value: u64) -> bool {
   (value & TAG_MASK) == POINTER_TAG
}

fn get_gc_ptr_from_value(value: u64) -> Option<*mut GcObject> {
   if is_gc_pointer(value) {
       // Assumes tag bits are 0 for valid pointers due to alignment
       Some(value as *mut GcObject)
   } else {
       None
   }
}

// --- Conceptual Root Scanning Loop ---
fn scan_roots<GC: GarbageCollector>(gc: &mut GC, parallax_net: &ParallaxNetRuntime) {
   // 1. Scan Stacks (Conservative or Precise via Stack Maps)
   for thread_stack in get_all_mutator_thread_stacks() {
       scan_stack_for_roots(gc, thread_stack);
   }

   // 2. Scan Globals
   for global_root_location in get_registered_global_roots() {
       let value = unsafe { std::ptr::read_volatile(global_root_location) }; // Read potential Gc<T>
       if let Some(ptr) = get_gc_ptr_from_value(value_to_u64(value)) { // Assuming value_to_u64 handles Gc<T>
            if gc.is_valid_gc_pointer(ptr) {
                gc.mark_root(ptr);
            }
       }
   }

   // 3. Scan parallax-net Ref Nodes
   // Assumes parallax_net provides an efficient iterator over Ref node values
   for ref_node_value in parallax_net.iterate_ref_node_values() {
       let value = ref_node_value.load(Ordering::Relaxed); // Load AtomicU64
       if let Some(ptr) = get_gc_ptr_from_value(value) {
           if gc.is_valid_gc_pointer(ptr) {
               gc.mark_root(ptr);
           }
       }
   }
}

// Placeholder for GC interface
trait GarbageCollector {
   fn mark_root(&mut self, ptr: *mut GcObject);
   fn is_valid_gc_pointer(&self, ptr: *mut GcObject) -> bool;
   //... other GC methods
}

// Placeholders for platform/runtime specifics
fn get_all_mutator_thread_stacks() -> Vec<StackMemoryRegion> { /*... */ }
fn scan_stack_for_roots<GC: GarbageCollector>(gc: &mut GC, stack: StackMemoryRegion) { /*... */ }
fn get_registered_global_roots() -> Vec<*const u64> { /*... */ } // Assuming globals store tagged u64
fn value_to_u64<T>(value: T) -> u64 { /*... */ } // Convert Gc<T> or other root types to u64
struct StackMemoryRegion { /*... */ }
struct ParallaxNetRuntime;
impl ParallaxNetRuntime {
   fn iterate_ref_node_values(&self) -> impl Iterator<Item = &AtomicU64> { /*... */ }
}


// --- Conceptual Write Barrier (for Concurrent/Incremental GC) ---
// Assuming a GC crate provides these hooks
trait ConcurrentGC {
   fn write_barrier_executed(&mut self, target_object: *mut GcObject, updated_field_value: *mut GcObject);
}

// Called after a field write: obj.field = new_gc_ptr;
fn perform_write_barrier<GC: ConcurrentGC>(gc: &mut GC, target: &mut GcObject, new_value_ptr: *mut GcObject) {
  // Simplified: actual barriers are more complex (e.g., snapshot-at-beginning)
  gc.write_barrier_executed(target as *mut _, new_value_ptr);
}


9. Conclusion
Integrating a garbage collector into the Parallax project to manage memory for Cranelift JIT-compiled code is feasible but presents significant technical challenges. The analysis indicates that several Rust GC crates could serve as a foundation, with rsgc and refuse emerging as the most promising candidates due to their multi-threading capabilities and features relevant to runtime integration (automatic stack scanning, explicit rooting, safepoints/guards).
A key decision point lies in the root-finding strategy. Starting with conservative stack scanning (potentially provided by rsgc) offers lower initial complexity, while investing in precise stack maps via Cranelift User Stack Maps provides better long-term accuracy and efficiency, albeit at a much higher development cost. Managing roots originating from parallax-net's Ref nodes requires careful AtomicU64 tagging (low-bit tagging recommended) and an efficient scanning mechanism developed in collaboration with the parallax-net team. Robust Stop-The-World synchronization across JIT and parallax-net threads is essential, best achieved using the chosen GC's primitives or a cooperative polling mechanism.
Significant challenges remain, particularly around the performance of Ref node scanning, the complexity of precise stack mapping (if pursued), and the safe and efficient tracing of complex JIT structures like closure environments. The potential pitfalls associated with finalization semantics also require careful design to avoid resource leaks or incorrect behavior.
Recommendations:
1. Select rsgc or refuse as the base GC crate after targeted prototyping.
2. Implement root finding starting with conservative stack scanning and explicit global registration. Plan for potential future migration to precise stack maps.
3. Use low-bit tagging for AtomicU64 Ref nodes and develop an efficient scanning mechanism with parallax-net.
4. Implement STW synchronization using the GC's safepoint mechanism or cooperative polling, ensuring diligent safepoint placement.
5. Handle JIT closure tracing via custom, traceable environment structures.
6. Manage resource cleanup carefully, avoiding reliance on Drop for GC objects holding critical resources.
Next Steps:
* Conduct focused prototyping to evaluate candidate GC crates (rsgc, refuse) and key mechanisms (conservative scanning, tagging, Ref node iteration).
* Begin detailed design of the JIT runtime interface for allocation and GC interaction.
* Collaborate with the parallax-net team on the Ref node scanning and STW synchronization strategy.
Successful integration will require careful design, rigorous testing, and potentially significant R&D effort, particularly if opting for precise stack map generation. However, leveraging existing Rust GC crates provides a solid foundation for building this critical component of the Parallax system.
Works cited
1. Others/shredder: Garbage collected smart pointers for Rust - GitHub, accessed on April 17, 2025, https://github.com/Others/shredder
2. Manishearth/rust-gc: Simple tracing (mark and sweep) garbage collector for Rust - GitHub, accessed on April 17, 2025, https://github.com/Manishearth/rust-gc
3. zesterer/broom: An ergonomic tracing garbage collector ... - GitHub, accessed on April 17, 2025, https://github.com/zesterer/broom
4. fitzgen/safe-gc: A garbage collection library for Rust with zero unsafe code - GitHub, accessed on April 17, 2025, https://github.com/fitzgen/safe-gc
5. rsgc - crates.io: Rust Package Registry, accessed on April 17, 2025, https://crates.io/crates/rsgc
6. playXE/rsgc - GitHub, accessed on April 17, 2025, https://github.com/playXE/rsgc
7. khonsulabs/refuse: An easy-to-use, incremental, multi ... - GitHub, accessed on April 17, 2025, https://github.com/khonsulabs/refuse
8. frengor/rust-cc: A fast garbage collector based on cycle collection for Rust programs., accessed on April 17, 2025, https://github.com/frengor/rust-cc
9. bacon_rajan_cc - crates.io: Rust Package Registry, accessed on April 17, 2025, https://crates.io/crates/bacon_rajan_cc
10. kyren/gc-arena: Incremental garbage collection from safe Rust - GitHub, accessed on April 17, 2025, https://github.com/kyren/gc-arena
11. dumpster : A cycle-tracking garbage collector for Rust - Crates.io, accessed on April 17, 2025, https://crates.io/crates/dumpster
12. A Tour of Safe Tracing GC Designs in Rust - In Pursuit of Laziness - Manish Goregaokar, accessed on April 17, 2025, https://manishearth.github.io/blog/2021/04/05/a-tour-of-safe-tracing-gc-designs-in-rust/
13. Broom: An ergonomic, easy to use garbage collector for your toy programming languages : r/rust - Reddit, accessed on April 17, 2025, https://www.reddit.com/r/rust/comments/fnscsx/broom_an_ergonomic_easy_to_use_garbage_collector/
14. safe-gc - crates.io: Rust Package Registry, accessed on April 17, 2025, https://crates.io/crates/safe-gc
15. Garbage Collection Without Unsafe Code : r/rust - Reddit, accessed on April 17, 2025, https://www.reddit.com/r/rust/comments/1akdg37/garbage_collection_without_unsafe_code/
16. refuse - crates.io: Rust Package Registry, accessed on April 17, 2025, https://crates.io/crates/refuse
17. accessed on January 1, 1970, https://github.com/khonsulabs/refuse/blob/main/src/lib.rs
18. garbage collection - Is it possible to make GC a library?, accessed on April 17, 2025, https://langdev.stackexchange.com/questions/2554/is-it-possible-to-make-gc-a-library
19. conservative gc can be faster than precise gc - wingolog, accessed on April 17, 2025, https://wingolog.org/archives/2024/09/07/conservative-gc-can-be-faster-than-precise-gc
20. Garbage Collection for Rust: The Finalizer Frontier - arXiv, accessed on April 17, 2025, https://arxiv.org/html/2504.01841v1
21. rustscan - crates.io: Rust Package Registry, accessed on April 17, 2025, https://crates.io/crates/rustscan
22. rustscan - Rust - Docs.rs, accessed on April 17, 2025, https://docs.rs/rustscan
23. PLSysSec/cargo-scan: A tool for auditing Rust crates - GitHub, accessed on April 17, 2025, https://github.com/PLSysSec/cargo-scan
24. How does Go's precise GC work? - Stack Overflow, accessed on April 17, 2025, https://stackoverflow.com/questions/26422896/how-does-gos-precise-gc-work
25. Precise vs conservative garbage collectors : r/Compilers - Reddit, accessed on April 17, 2025, https://www.reddit.com/r/Compilers/comments/1d2o0ut/precise_vs_conservative_garbage_collectors/
26. New Stack Maps for Wasmtime and Cranelift - Bytecode Alliance, accessed on April 17, 2025, https://bytecodealliance.org/articles/new-stack-maps-for-wasmtime
27. New Stack Maps for Wasmtime and Cranelift - rust - Reddit, accessed on April 17, 2025, https://www.reddit.com/r/rust/comments/1fdjpmx/new_stack_maps_for_wasmtime_and_cranelift/
28. "StackMap" Search - Rust - Docs.rs, accessed on April 17, 2025, https://docs.rs/cranelift-codegen/latest/i686-pc-windows-msvc/cranelift_codegen/?search=StackMap
29. cranelift_codegen::ir - Rust - Wasmtime, accessed on April 17, 2025, https://docs.wasmtime.dev/api/cranelift_codegen/ir/index.html
30. ELF_WASMTIME_STACK_MAP in wasmtime_environ::obj - Rust, accessed on April 17, 2025, https://docs.wasmtime.dev/api/wasmtime_environ/obj/constant.ELF_WASMTIME_STACK_MAP.html
31. Use After Free with `externref`s in Wasmtime - GitHub, accessed on April 17, 2025, https://github.com/bytecodealliance/wasmtime/security/advisories/GHSA-5fhj-g3p3-pq9g
32. Shifgrethor: garbage collection as a Rust library, accessed on April 17, 2025, https://internals.rust-lang.org/t/shifgrethor-garbage-collection-as-a-rust-library/8597
33. playXE/pgc: Preciese garbage collector in Rust. - GitHub, accessed on April 17, 2025, https://github.com/playXE/pgc
34. On `#![feature(global_registration)]` : r/rust - Reddit, accessed on April 17, 2025, https://www.reddit.com/r/rust/comments/1e379v1/on_featureglobal_registration/
35. Global Registration (a kind of pre-rfc) - language design - Rust Internals, accessed on April 17, 2025, https://internals.rust-lang.org/t/global-registration-a-kind-of-pre-rfc/20813
36. Benchmarks or analysis of pointer tagging : r/ProgrammingLanguages - Reddit, accessed on April 17, 2025, https://www.reddit.com/r/ProgrammingLanguages/comments/qopk1d/benchmarks_or_analysis_of_pointer_tagging/
37. A Tour of Safe Tracing GC Designs in Rust - Hacker News, accessed on April 17, 2025, https://news.ycombinator.com/item?id=27559239
38. NaN Boxing, Pointer Tagging, and 64-bit pointers : r/ProgrammingLanguages - Reddit, accessed on April 17, 2025, https://www.reddit.com/r/ProgrammingLanguages/comments/whwe4g/nan_boxing_pointer_tagging_and_64bit_pointers/
39. Optimize Option
40. Use object-biased NaN boxing instead of double-biased on x86-64 - Bugzilla@Mozilla, accessed on April 17, 2025, https://bugzilla.mozilla.org/show_bug.cgi?id=1401624
41. AtomicU64 in std::sync::atomic - Rust Documentation, accessed on April 17, 2025, https://doc.rust-lang.org/std/sync/atomic/struct.AtomicU64.html
42. Which is more efficient, basic mutex lock or atomic integer? - Stack Overflow, accessed on April 17, 2025, https://stackoverflow.com/questions/15056237/which-is-more-efficient-basic-mutex-lock-or-atomic-integer
43. Packages and Crates - The Rust Programming Language, accessed on April 17, 2025, https://doc.rust-lang.org/book/ch07-01-packages-and-crates.html
44. Crates and Modules - The Rust Programming Language - MIT, accessed on April 17, 2025, https://web.mit.edu/rust-lang_v1.25/arch/amd64_ubuntu1404/share/doc/rust/html/book/first-edition/crates-and-modules.html
45. crates.io: Rust Package Registry, accessed on April 17, 2025, https://crates.io/
46. GC and Rust Part 2: The Roots of the Problem - The {pnk}f(eli)x Blog, accessed on April 17, 2025, http://blog.pnkfx.org/blog/2016/01/01/gc-and-rust-part-2-roots-of-the-problem/
47. Load/store R32/R64 types · Issue #1146 · bytecodealliance/wasmtime - GitHub, accessed on April 17, 2025, https://github.com/bytecodealliance/cranelift/issues/1176
48. What are the disadvantages in using Garbage Collection? [closed] - Stack Overflow, accessed on April 17, 2025, https://stackoverflow.com/questions/3214980/what-are-the-disadvantages-in-using-garbage-collection
49. Whose baseline compiler is it anyway? - arXiv, accessed on April 17, 2025, https://arxiv.org/pdf/2305.13241
50. GC isn't closure friendly. · Issue #50 · Manishearth/rust-gc - GitHub, accessed on April 17, 2025, https://github.com/Manishearth/rust-gc/issues/50
51. Garbage collector · Issue #20 · CeleritasCelery/rune - GitHub, accessed on April 17, 2025, https://github.com/CeleritasCelery/rune/issues/20