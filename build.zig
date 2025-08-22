const std = @import("std");

const version = std.SemanticVersion{ .major = 0, .minor = 2, .patch = 0 };

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    _ = b.addModule("shuffling-allocator", .{ .root_source_file = b.path("src/shuffling-allocator.zig") });

    const test_module = b.createModule(.{
        .root_source_file = b.path("src/shuffling-allocator.zig"),
        .target = target,
        .optimize = optimize,
    });

    const lib_unit_tests = b.addTest(.{
        .root_module = test_module,
    });

    const run_lib_unit_tests = b.addRunArtifact(lib_unit_tests);
    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&run_lib_unit_tests.step);
}
