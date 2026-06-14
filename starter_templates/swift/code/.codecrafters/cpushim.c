/*
 * CPU-count shim (loaded via LD_PRELOAD by swift_build.sh).
 *
 * SwiftPM bakes the host's CPU count into every whole-module Swift compile
 * command as `-num-threads <ProcessInfo.activeProcessorCount>` (and `-j`). The
 * CodeCrafters image is built on a host with more cores than the test runner,
 * so the command lines—and therefore llbuild's command signatures—differ
 * between build and run. llbuild then considers every Swift command stale and
 * rebuilds the entire module graph on each run (~30s). C targets compile via
 * clang, for which SwiftPM doesn't use `-num-threads`, so they are spared from
 * the issue.
 *
 * This shim overrides the CPU count lookups that SwiftPM reads
 * (`activeProcessorCount` resolves to `sysconf(_SC_NPROCESSORS_ONLN)` on Linux)
 * so that it always sees a constant count (FAKE_NPROC, default 4). The commands
 * Swift PM emits then match in both environments (image build vs. test runner),
 * and runs stay incremental. There is no SwiftPM/swiftc flag that pins
 * `-num-threads`: it is appended unconditionally in the whole-module branch, so
 * intercepting sysconf is the only lever that works.
 */
#define _GNU_SOURCE
#include <dlfcn.h>
#include <unistd.h>
#include <stdlib.h>

static int fake_nprocs(void) {
    const char *env = getenv("FAKE_NPROC");
    int num_cpus = env ? atoi(env) : 4;
    return num_cpus > 0 ? num_cpus : 4;
}

static long (*real_sysconf)(int);

long sysconf(int name) {
    if (!real_sysconf) {
        real_sysconf = dlsym(RTLD_NEXT, "sysconf");
    }
    if (name == _SC_NPROCESSORS_ONLN || name == _SC_NPROCESSORS_CONF) {
        return fake_nprocs();
    }
    return real_sysconf(name);
}

int get_nprocs(void) { return fake_nprocs(); }
int get_nprocs_conf(void) { return fake_nprocs(); }
