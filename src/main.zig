//! A multi-threaded "shuffling" allocator for Zig,
//! implementing the standard `std.mem.Allocator` interface.
const std = @import("std");

pub const ShufflingAllocator = struct {
    /// The Allocator we expose to user code.
    /// .ptr is `*ShufflingAllocator`, .vtable are the function pointers below.
    base: std.mem.Allocator,

    /// The underlying allocator we rely on for real memory requests.
    underlying: std.mem.Allocator,

    /// Global random state for picking shuffle indices in [0..256).
    /// Accessed only while holding the `global_mutex`.
    rng_state: u64,

    /// One shuffle array per size class.
    size_classes: [NUM_SIZE_CLASSES]ShuffleArray,

    /// A mutex protecting the random state, and also each size class’s
    /// shuffle array has its own sub‐mutex. This struct ensures we can lock
    /// a global mutex for the RNG, but a separate array of per‐class locks
    /// for concurrency on the shuffle arrays. So we do:
    ///
    ///   - Lock `global_mutex` while reading/writing `rng_state`.
    ///   - Lock `size_classes[i].mutex` while accessing that shuffle array.
    ///
    global_mutex: std.Thread.Mutex,
    size_class_mutexes: [NUM_SIZE_CLASSES]std.Thread.Mutex,

    pub fn create(
        underlying: std.mem.Allocator,
        seed: u64,
    ) ShufflingAllocator {
        var self = ShufflingAllocator{
            // We’ll fill the vtable in a moment, and fix .ptr last:
            .base = .{ .ptr = undefined, .vtable = &ShufflingAllocator.vtable },
            .underlying = underlying,
            .rng_state = seed,
            .size_classes = undefined,
            .global_mutex = .{},
            .size_class_mutexes = undefined,
        };

        // Zero-init each ShuffleArray (and each sub-mutex).
        inline for (0..NUM_SIZE_CLASSES) |i| {
            self.size_classes[i].init();
            self.size_class_mutexes[i] = .{};
        }

        // Finally, set our own pointer. This must be done after we
        // fill all fields.
        self.base.ptr = &self;

        return self;
    }

    /// The standard VTable with function pointers for Allocator calls.
    const vtable: std.mem.Allocator.VTable = .{
        .alloc = allocFn,
        .resize = resizeFn,
        .remap = remapFn,
        .free = freeFn,
    };

    /// Actual .alloc method. Must return ?[]u8 or null on OOM.
    fn allocFn(
        self_ptr: *anyopaque,
        len: usize,
        alignment: std.mem.Alignment,
        ret_addr: usize,
    ) ?[*]u8 {
        std.debug.assert((@intFromPtr(self_ptr) % @alignOf(ShufflingAllocator)) == 0);
        const tmp: *align(@alignOf(ShufflingAllocator)) anyopaque = @alignCast(self_ptr);
        const self: *ShufflingAllocator = @ptrCast(tmp);

        // Tiger Style: we assert function arguments as needed:
        // But std.mem.Allocator doesn’t promise len>0, so we do a quick check.
        if (len == 0) return &[_]u8{}; // Zero-length slice.

        // If alignment > size_of(usize), skip shuffling => fallback:
        if (alignment.toByteUnits() > @alignOf(usize)) {
            return std.mem.Allocator.rawAlloc(
                self.underlying,
                len,
                alignment,
                ret_addr,
            );
        }

        const idx_opt = sizeClassIndex(len);
        if (idx_opt == null) {
            // out-of-range => skip shuffling
            return std.mem.Allocator.rawAlloc(
                self.underlying,
                len,
                alignment,
                ret_addr,
            );
        }

        const class_index = idx_opt.?;
        // Lock the shuffle array:
        self.size_class_mutexes[class_index].lock();
        defer self.size_class_mutexes[class_index].unlock();

        // Also lock our global mutex for rng. We keep these separate
        // so multiple threads can operate on different size classes
        // concurrently, but still protect the single rng_state:
        self.global_mutex.lock();
        const rand_i = randomIndex(&self.rng_state);
        self.global_mutex.unlock();

        const sc = &self.size_classes[class_index];
        sc.activateIfNeeded(len);

        // Allocate from underlying:
        const new_ptr = std.mem.Allocator.rawAlloc(
            self.underlying,
            len,
            alignment,
            ret_addr,
        ) orelse return null;

        // Swap with the random slot:
        const old_ptr = sc.ptrs[rand_i]; // type is `?[*]u8`
        if (old_ptr == null) {
            return new_ptr; // also `[*]u8`
        } else {
            // unwrap
            const non_null_ptr = old_ptr.?;
            return non_null_ptr; // type is `[*]u8`
        }

        sc.ptrs[rand_i] = new_ptr;

        // If that slot was empty, we return the new pointer.
        if (old_ptr == null) {
            return new_ptr;
        }

        // If that slot was not empty, we return that old pointer’s slice.
        // We do not free the new_ptr. We effectively “shuffle” them.
        // Ensure the returned slice is exactly `len`.
        return old_ptr[0..len];
    }

    /// .free method.
    fn freeFn(
        self_ptr: *anyopaque,
        memory: []u8,
        alignment: std.mem.Alignment,
        ret_addr: usize,
    ) void {
        std.debug.assert((@intFromPtr(self_ptr) % @alignOf(ShufflingAllocator)) == 0);
        const tmp: *align(@alignOf(ShufflingAllocator)) anyopaque = @alignCast(self_ptr);
        const self: *ShufflingAllocator = @ptrCast(tmp);

        // Zero-length means nothing to free.
        if (memory.len == 0) return;

        // If alignment is large, skip shuffle:
        if (alignment.toByteUnits() > @alignOf(usize)) {
            std.mem.Allocator.rawFree(
                self.underlying,
                memory,
                alignment,
                ret_addr,
            );
            return;
        }

        const idx_opt = sizeClassIndex(memory.len);
        if (idx_opt == null) {
            // out-of-range => skip shuffle
            std.mem.Allocator.rawFree(
                self.underlying,
                memory,
                alignment,
                ret_addr,
            );
            return;
        }

        const class_index = idx_opt.?;
        self.size_class_mutexes[class_index].lock();
        defer self.size_class_mutexes[class_index].unlock();

        // Lock rng:
        self.global_mutex.lock();
        const rand_i = randomIndex(&self.rng_state);
        self.global_mutex.unlock();

        const sc = &self.size_classes[class_index];

        std.debug.assert(sc.active);
        std.debug.assert(sc.size_class == memory.len);

        // Swap with random index:
        const old_ptr = sc.ptrs[rand_i];
        sc.ptrs[rand_i] = memory.ptr;

        // If old_ptr was non-null, free it:
        if (old_ptr != null) {
            const p = old_ptr.?;
            std.mem.Allocator.rawFree(
                self.underlying,
                p[0..sc.size_class],
                alignment,
                ret_addr,
            );
        }
    }

    /// .resize method: pass-through, do not shuffle on resize.
    fn resizeFn(
        self_ptr: *anyopaque,
        memory: []u8,
        alignment: std.mem.Alignment,
        new_len: usize,
        ret_addr: usize,
    ) bool {
        std.debug.assert((@intFromPtr(self_ptr) % @alignOf(ShufflingAllocator)) == 0);
        const tmp: *align(@alignOf(ShufflingAllocator)) anyopaque = @alignCast(self_ptr);
        const self: *ShufflingAllocator = @ptrCast(tmp);
        return std.mem.Allocator.rawResize(
            self.underlying,
            memory,
            alignment,
            new_len,
            ret_addr,
        );
    }

    /// .remap method: pass-through.
    fn remapFn(
        self_ptr: *anyopaque,
        memory: []u8,
        alignment: std.mem.Alignment,
        new_len: usize,
        ret_addr: usize,
    ) ?[*]u8 {
        std.debug.assert((@intFromPtr(self_ptr) % @alignOf(ShufflingAllocator)) == 0);
        const tmp: *align(@alignOf(ShufflingAllocator)) anyopaque = @alignCast(self_ptr);
        const self: *ShufflingAllocator = @ptrCast(tmp);
        return std.mem.Allocator.rawRemap(
            self.underlying,
            memory,
            alignment,
            new_len,
            ret_addr,
        );
    }
};

/// ShuffleArray keeps up to 256 pointers for each size class.
const ShuffleArray = struct {
    active: bool = false,
    size_class: usize = 0,
    ptrs: [SHUFFLE_CAPACITY]?[*]u8 = [_]?[*]u8{null} ** SHUFFLE_CAPACITY,

    fn init(self: *ShuffleArray) void {
        self.active = false;
        self.size_class = 0;
        // Zero out the pointer array:
        inline for (0..SHUFFLE_CAPACITY) |i| {
            self.ptrs[i] = null;
        }
    }

    fn activateIfNeeded(self: *ShuffleArray, size: usize) void {
        if (!self.active) {
            self.active = true;
            self.size_class = size;
        } else {
            std.debug.assert(self.size_class == size);
        }
    }
};

/// Constants: 32 size classes, 256 shuffle capacity, etc.
const NUM_SIZE_CLASSES = 32;
const SHUFFLE_CAPACITY = 256;

/// Decide which size class to use, or return null if it doesn't fit.
fn sizeClassIndex(len: usize) ?usize {
    var c: usize = 8;
    var i: usize = 0;
    while (i < NUM_SIZE_CLASSES) : (i += 1) {
        if (len <= c) return i;
        c <<= 1; // next power-of-two
    }
    return null;
}

/// A linear congruential generator. Tiger Style: we handle random state
/// with a single function to keep it minimal. We do not do recursion or
/// fancy abstractions, just a direct approach.
fn randomIndex(state_ptr: *u64) u8 {
    const mul = @mulWithOverflow(state_ptr.*, 6364136223846793005)[0];
    const add = @addWithOverflow(mul, 1)[0];
    state_ptr.* = add;

    return @truncate(add);
}

test "multi-threaded shuffling allocator example usage" {
    // Just a simple single-thread test. For multi-thread usage,
    // you can launch threads that do `alloc` / `free` concurrently.
    const gpa = std.heap.page_allocator;
    const shuffler = ShufflingAllocator.create(gpa, 12345);
    const alloc = shuffler.base;

    const ptr = try alloc.alloc(u8, 16);
    defer alloc.free(ptr);

    for (ptr, 0..) |*b, i| {
        b.* = @truncate(i);
    }

    for (ptr, 0..) |b, i| std.debug.assert(b == i);
}
