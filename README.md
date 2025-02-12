# shuffling-allocator.zig

The **ShufflingAllocator** package provides a type that wraps an existing allocator and shuffles the order of heap allocations, effectively randomizing the placement of heap-allocated memory. This randomization is useful for testing, benchmarking, and performance evaluation, allowing you to decouple the effects of memory locality from performance metrics during code optimization or profiling.

## Features

- **Heap Allocation Shuffling**: Randomizes the placement of heap allocations to avoid accidental memory locality, reducing cache effects during performance testing.
- **Concurrency Support**: Designed to support multi-threaded environments while maintaining proper synchronization using mutexes.
- **Customizable Randomization**: Uses a global random state to shuffle allocations across different size classes, enhancing the reliability of performance tests.
- **Raw Memory Management**: Provides custom alloc, free, resize, and remap methods adhering to the Zig standard library's allocator interface.

## Use Cases

- **Performance Testing**: Separate the impact of memory locality from other performance factors when benchmarking code changes.
- **Memory Management Research**: Experiment with different allocation patterns and their impact on system performance.
- **Randomization for Debugging**: Randomize heap allocations to test for latent bugs or memory-related issues that depend on specific memory layouts.

### **Note on Security Use Cases**
While randomizing heap allocation addresses certain memory access patterns that could influence performance, this package is **not** designed for security purposes (e.g., as a substitute for Address Space Layout Randomization (ASLR)). If you're looking for a solution focused on security hardening, this package may not meet your needs due to design choices that prioritize performance over security.

## Installation

To use this package in your Zig project, you can simply import it:

```zig
const ShufflingAllocator = @import("shuffling_allocator").ShufflingAllocator;
```

## API Reference
This is the primary type in this package, implementing the standard `std.mem.Allocator` interface, with the following methods:

`alloc`: Allocates memory with randomized placement. If the memory size is too large for shuffling, it falls back to standard allocation.
`free`: Frees the memory, also randomizing its location in the heap.
`resize`: Resizes the allocated memory without applying shuffling.
`remap`: Remaps the memory without applying shuffling.

Inspired by https://github.com/fitzgen/shuffling-allocator
