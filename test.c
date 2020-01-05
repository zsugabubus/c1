/*
 * Copyright (C) 2019,2020 zsugabubus
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */
#define _GNU_SOURCE
#define _DEFAULT_SOURCE
#ifndef TEST_NOLIBSEGFAULT
# include <dlfcn.h> /* dlopen */
#endif
#include <execinfo.h> /* backtrace, backtrace_symbols_fd */
#include <limits.h> /* LONG_MAX */
#include <signal.h> /* signal */
#include <sys/mman.h> /* memfd_create */
#include <time.h> /* clock_gettime */

#include <stdio.h>

#include "test.h"

#define OUTPUT_HUMAN_SUITE_MINLEN (5 + 4)
#define OUTPUT_HUMAN_TEST_MINLEN (5 + 5 + 11)

#define FREE __attribute__((cleanup(varfree)))

extern char __start_test;
extern char __stop_test;

struct test_suite_info *currsuite = NULL;
struct test_test_info *currtest = NULL;
struct test_case_info *currcase = NULL;

/* Free variable. */
__attribute__((nonnull, unused)) static
void varfree(char *const*const var) {
	free(*var);
}

#define US_NS 1000ul
#define MS_NS 1000000ul
#define SEC_NS 1000000000ul

#ifndef TEST_NOFORK
int test_fork(int *result) {
	switch (fork()) {
	case 0: return 0;
	case -1:
		perror("fork");
		exit(EXIT_FAILURE);
	default: {
		int stat_val;
		wait(&stat_val);
		*result = WIFEXITED(stat_val) ? WEXITSTATUS(stat_val) : EXIT_FAILURE;
		return 1;
	}
	}
}
#endif

void test_exit(int code) {
#ifndef TEST_NOFORK
	exit(code);
#else
	*(currcase ? &currcase->result : &currtest->result) = code;
	longjmp(currcase ? currcase->env : test_test_env, code + 1);
#endif
}

#ifdef TEST_NOFORK
static struct {
	jmp_buf env;
} fake_test_env;
#endif

/** Write content of `fd` to stderr. */
static
void cat(int fd) {
	char buf[(1 << 13)];
	ssize_t len = 0;
	ssize_t plen;
	off_t offset;

	for (offset = 0;plen = len, (len = pread(fd, buf, sizeof(buf), offset)) > 0;offset += len)
		write(STDERR_FILENO, buf, len);

	if (0 == plen || buf[plen - 1] != '\n')
		fputs("\x1b[7m$\x1b[0m\n", stderr);
	fputc('\n', stderr);
}

/** Return a time unit that makes sense for human beings. */
__attribute__((const)) static
unsigned long nstohtime(unsigned long ns) {
	if (ns < 10 * US_NS)
		return ns;
	else if (ns < 10 * MS_NS)
		return ns / US_NS;
	else if (ns < 10 * SEC_NS)
		return ns / MS_NS;
	else
		return ns / SEC_NS;
}

/** Return time value that matches with human time unit. */
 __attribute__((returns_nonnull, const)) static
char const *nstohunit(unsigned long ns) {
	if (ns < 10 * US_NS)
		return "ns";
	else if (ns < 10 * MS_NS)
		return "us";
	else if (ns < 10 * SEC_NS)
		return "ms";
	else
		return "s";
}

/** Return time in nanoseconds. */
__attribute__((const)) static
unsigned long tstons(struct timespec const *const ts) {
	return ts->tv_sec * SEC_NS + ts->tv_nsec;
}

/** Compute elapsed time from `start` time to `end` time. Write result into `result`. */
static
void tsdiff(struct timespec const *end, struct timespec const *start, struct timespec *result) {
	result->tv_sec  = end->tv_sec  - start->tv_sec  + (end->tv_nsec < start->tv_nsec);
	result->tv_nsec = end->tv_nsec - start->tv_nsec + (end->tv_nsec < start->tv_nsec ? SEC_NS : 0);
}

/** Print a `n` character wide horizontal separator using `ch` character. */
static
void print_hline(int n, char ch) {
	int i;

	for (i = 0;i < n;++i)
		fputc(ch, stdout);
	fputc('\n', stdout);
}

void test_print_suite_path(void) {
	fprintf(stderr, "suite " "\x1b[1m" "%s" "\x1b[0m" " (%s):\n",
		currtest->name, currtest->file);
}

void test_print_test_path(void) {
	fprintf(stderr, "\x1b[1m" "test " "%s" "\x1b[0m" " (%s:%u):\n",
		currtest->name, currtest->file, currtest->line);
}

void test_print_case_path(void) {
	struct test_case_info *p;

	fprintf(stdout, "\x1b[1m" "case" "\x1b[0m" " %s/", currtest->name);
	for (p = currcase;p->parent;p = p->parent)
		;

	for (;p->child;p = p->child)
		fprintf(stdout, "%s" "\x1b[0m" "/",
			p->name);

	fprintf(stdout, "\x1b[1m" "%s" "\x1b[0m" " (%s:%u):\n",
		p->name, p->file, p->line);
}

/** Default signal handler. */
static
void sighandler(int sig, siginfo_t *info, void *ucontext) {
	void *array[50];
	int size;

	psiginfo(info, "Unhandled signal");

	size = backtrace(array, 50);

	fprintf(stderr, "\nBacktrace:\n");
	backtrace_symbols_fd(array, size, STDERR_FILENO);

	test_exit(EXIT_FAILURE);
}

/** Initialize signal handling. */
static
void sighandler_init(void) {
	static struct sigaction sa;
	sa.sa_sigaction = sighandler;
	sa.sa_flags = SA_RESTART | SA_SIGINFO;
	sigemptyset(&sa.sa_mask);

#define SIG(name) \
	sigaction(SIG##name, &sa, NULL);

	SIG(ABRT  ) SIG(ALRM  ) SIG(BUS   ) SIG(FPE   ) SIG(HUP   )
	SIG(ILL   ) SIG(INT   ) SIG(KILL  ) SIG(PIPE  ) SIG(POLL  )
	SIG(PROF  ) SIG(QUIT  ) SIG(SEGV  ) SIG(STOP  ) SIG(TSTP  )
	SIG(SYS   ) SIG(TERM  ) SIG(TRAP  ) SIG(TTIN  ) SIG(TTOU  )
	SIG(USR1  ) SIG(USR2  ) SIG(VTALRM) SIG(XCPU  ) SIG(XFSZ  )

#undef SIG
}

union object_ptr {
	void *ptr;
	struct test_info_header *header;
	struct test_suite_info *suite;
	struct test_hook_info *hook;
	struct test_test_info *test;
};

static
void fire_event(int event) {
	union object_ptr p;
	for (p.ptr = (void*)&__start_test;p.test < currtest;) {
		switch (p.header->type) {
		case test_object_id_suite:
			++p.suite;
			break;
		case test_object_id_hook:
			if (2/*global*/ == p.hook->scope ||
			   (1/*suite */ == p.hook->scope && p.suite >= currsuite))
				p.hook->hook(event);
			++p.hook;
			continue;
		case test_object_id_test:
			++p.test;
			break;
		default:
			__builtin_unreachable();
		}
	}
}

int main(int argc, char **argv) {
	enum { test_suite, test_total, test_passed, test_failed, test_skipped, test_result_count };
	unsigned stat[test_result_count] = {0};
	char **X = isatty(STDOUT_FILENO) ? TERMINAL : NORMAL;
	union object_ptr p;
	int width = 0;
#ifndef TEST_NOGATHEROUTPUT
	int gathered = 0;
#endif
	unsigned long *shm_total_ns;
	struct timespec ts_start;
	struct timespec ts_end;
	int nullfd;
#ifndef TEST_NOLIBSEGFAULT
	void *dlhandle; /* Handle fo libSegFault */
#endif

	sighandler_init();

#ifndef TEST_NOLIBSEGFAULT
	dlhandle = dlopen("/lib/libSegFault.so", RTLD_LAZY);
	if (!dlhandle)
		/* Don't treat it fatal error. */
		perror("dlopen");
#endif

	if (-1 == (nullfd = open("/dev/null", O_WRONLY)))
		perror("open");

	clock_gettime(CLOCK_MONOTONIC, &ts_start);

	for (p.ptr = (void*)&__start_test;p.ptr < &__stop_test;) {
		int len;

		switch (p.header->type) {
		case test_object_id_suite:
			currsuite = p.suite;
			++stat[test_suite];

			len = strlen(currsuite->name) + strlen(currsuite->file) + OUTPUT_HUMAN_SUITE_MINLEN;
			if (len > width)
				width = len;

			++p.suite;
			break;
		case test_object_id_hook:
			++p.hook;
			continue;
		case test_object_id_test:
			++stat[test_total];

			/* Count tests in suites. */
			if (currsuite)
				++currsuite->num_tests;

			len = strlen(p.test->name) + OUTPUT_HUMAN_TEST_MINLEN;
			if (len > width)
				width = len;

			++p.test;
			break;
		default:
			__builtin_unreachable();
		}
	}

	if (width > 72)
		width = 72;

	fprintf(stdout, "running %u tests\n",
			stat[test_total]);

	shm_total_ns = mmap(NULL, sizeof(*shm_total_ns),
		PROT_READ | PROT_WRITE,
		MAP_SHARED | MAP_ANONYMOUS, -1, 0);

	if (MAP_FAILED == shm_total_ns) {
		perror("mmap");
		exit(EXIT_FAILURE);
	}

	currsuite = NULL;
	for (p.ptr = (void*)&__start_test;p.ptr < &__stop_test;) {
		int argi;
		int skip;
#ifdef TEST_NOFORK
		int oldstderr;
		int oldstdout;
#endif

		switch (p.header->type) {
		case test_object_id_suite:
			/* Suite start marker. */
			currsuite = p.suite;
			fprintf(stdout, "\nsuite " "\x1b[1m" "%s" "\x1b[0m" " (%s):\n",
					currsuite->name, currsuite->file);
			print_hline(width, '-');
			++p.suite;
			goto next_test;
		case test_object_id_hook:
			++p.hook;
			continue;
		case test_object_id_test:
			currtest = p.test;
			++p.test;
			break;
		default:
			__builtin_unreachable();
		}

		fprintf(stdout, "test " "\x1b[1m" "%s" "\x1b[0m" " ... %*s",
			currtest->name,
			width - OUTPUT_HUMAN_TEST_MINLEN - (int)strlen(currtest->name), "");
		fflush(stdout);

		for (skip = 0, argi = 1;argi < argc;++argi) {
			char *filter = argv[argi];
			int should_skip = 0;

			switch (filter[0]) {
			case '+':
				++filter;
				break;
			case '-':
				++filter;
				should_skip = 1;
				break;
			}

			if (1 == argi)
				skip = !should_skip;

			if (strstr(currtest->name, filter))
				skip = should_skip;
		}

		if (skip || 0 == currtest->iters) {
test_skip:
			++stat[test_skipped];
			fprintf(stdout, "\x1b[1;34m" "skipped" "\x1b[0m" "\n");
			goto drop_output;
		}

		currtest->outfd = memfd_create(currtest->name, 0);
		if (-1 == currtest->outfd) {
			perror("memfd_create");
			exit(EXIT_FAILURE);
		}

		*shm_total_ns = -1;

		if (!currsuite->setup_ran) {
			currsuite->setup_ran = 1;
			fire_event(test_event_setup_suite);
		}

		fire_event(test_event_setup_test);

#ifdef TEST_NOFORK
		oldstderr = dup(STDERR_FILENO);
		oldstdout = dup(STDOUT_FILENO);
#endif

#ifndef TEST_NOFORK
		if (!test_fork_(*currtest)) {
#else
		if (!test_fork_(fake_test_env)) {
#endif
			struct timespec ts_test_start;
			struct timespec ts_test_end;
			unsigned iters = currtest->iters;

			dup2(currtest->outfd, STDERR_FILENO);
			setvbuf(stderr, NULL, _IONBF, 0);
			dup2(currtest->outfd, STDOUT_FILENO);
			setvbuf(stdout, NULL, _IONBF, 0);
			clock_gettime(CLOCK_MONOTONIC, &ts_test_start);

			while (iters-- > 0)
				currtest->run();

			clock_gettime(CLOCK_MONOTONIC, &ts_test_end);
			tsdiff(&ts_test_end, &ts_test_start, &ts_test_start);
			*shm_total_ns = tstons(&ts_test_start);

			test_exit(currtest->result);
		} else {
#ifdef TEST_NOFORK
			dup2(oldstderr, STDERR_FILENO);
			dup2(oldstdout, STDOUT_FILENO);
#endif

			switch (currtest->result) {
			case EXIT_SUCCESS: {
				char info[66];
				if (-1 == *shm_total_ns)
					goto test_failed;

				currtest->total_ns = *shm_total_ns;

				if (currtest->iters == 1)
					snprintf(info, sizeof info, "%4lu %s",
						nstohtime(currtest->total_ns),
						nstohunit(currtest->total_ns));
				else
					snprintf(info, sizeof info, "%4lu %s / %9lu iters = %4lu %s/iter",
						nstohtime(currtest->total_ns),
						nstohunit(currtest->total_ns),
						currtest->iters,
						nstohtime((currtest->total_ns + currtest->iters - 1) / currtest->iters),
						nstohunit((currtest->total_ns + currtest->iters - 1) / currtest->iters));

				++stat[test_passed];
				fprintf(stdout, "\x1b[1;32m" "ok" "\x1b[0m" ", %s\n", info);
					goto drop_output;
			}
			case EXIT_SKIP: /* Special exit code. */
				goto test_skip;
			default: { /* Others. */
			test_failed:
				fprintf(stdout, "\x1b[1;31m" "FAILED" "\x1b[0m" "\n");
					++stat[test_failed];

#ifdef TEST_NOGATHEROUTPUT
					cat(currtest->outfd);
#else
					goto next_test; /* Keep output. */
#endif
			}
			}
		}

drop_output:
		close(currtest->outfd);
		currtest->outfd = -1;
next_test:
		fire_event(test_event_teardown_test);

		if (currsuite) {
			if (0 == currsuite->num_tests) {
				if (currsuite->setup_ran)
					fire_event(test_event_teardown_suite);
			} else {
				--currsuite->num_tests;
			}
		}
	}

	munmap(shm_total_ns, sizeof(*shm_total_ns));

#ifndef TEST_NOGATHEROUTPUT
	print_hline(width, '=');
	for (p.ptr = (void*)&__start_test;p.ptr < &__stop_test;) {
		switch (p.header->type) {
		case test_object_id_suite:
			++p.suite;
			break;
		case test_object_id_hook:
			++p.hook;
			break;
		case test_object_id_test:
			if (-1 != p.test->outfd) {
				gathered = 1;
				currtest = p.test;
				test_print_test_path();
				cat(p.test->outfd);

				close(p.test->outfd);
				p.test->outfd = -1;
			}
			++p.test;
			break;
		default:
			__builtin_unreachable();
		}

	}
#endif

	clock_gettime(CLOCK_MONOTONIC, &ts_end);
	tsdiff(&ts_end, &ts_start, &ts_start);

	if (gathered)
		print_hline(width, '=');
	fprintf(stdout,
		"\x1b[1m" "test result" "\x1b[0m" ": " "\x1b[1m" "%s" "\x1b[0m" ". "
		"\x1b[32m" "%u passed"  "\x1b[0m" "; "
		"\x1b[31m" "%u failed"  "\x1b[0m" "; "
		"\x1b[34m" "%u skipped" "\x1b[0m" "\n",
		0 == stat[test_failed] ? "\x1b[32m" "ok" : "\x1b[31m" "FAILED",
		stat[test_passed],
		stat[test_failed],
		stat[test_skipped]);

#ifndef TEST_NOLIBSEGFAULT
	dlclose(dlhandle);
#endif

	exit(0 == stat[test_failed] ? EXIT_SUCCESS : EXIT_FAILURE);
}
/* vim:set ft=c ts=4 sw=4 noet: */
