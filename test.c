/*
 * Copyright (C) 2019 zsugabubus
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

#ifdef TEST_OUTPUT_LIBCHECKXML
# include <libgen.h> /* dirname, basename */
# define OUTPUT_LIBCHECK_TEMPLATE_START \
	"<?xml version=\"1.0\"?>\n" \
	"<?xml-stylesheet type=\"text/xsl\"" \
	" href=\"http://check.sourceforge.net/xml/check_unittest.xslt\"?>\n" \
	"<testsuites xmlns=\"http://check.sourceforge.net/ns\">\n" \
	"  <datetime>%s</datetime>\n",
# define OUTPUT_LIBCHECK_TEMPLATE_SUITE_START \
	"  <suite>\n" \
	"    <title>%s<title>\n"
# define OUTPUT_LIBCHECK_TEMPLATE_TEST \
	"    <test result=\"%s\">\n" \
	"      <path>%s</path>\n" \
	"      <fn>%s:%u</fn>\n" \
	"      <id>%s</id>\n" \
	"      <iteration>%ld</iteration>\n" \
	"      <duration>%.6f</duration>\n" \
	"      <description>%s</description>\n" \
	"      <message>%s</message>\n" \
	"    </test>\n"
# define OUTPUT_LIBCHECK_TEMPLATE_SUITE_END \
	"  </suite>\n"
# define OUTPUT_LIBCHECK_TEMPLATE_END \
	"  <duration>%.6f</duration>\n" \
	"</testsuites>\n"
#endif

#define OUTPUT_HUMAN_SUITE_MINLEN (5 + 4)
#define OUTPUT_HUMAN_TEST_MINLEN (5 + 5 + 11)

#define FREE __attribute__((cleanup(varfree)))
#define CONST __attribute__((const))
#define UNUSED __attribute__((unused))

extern struct test_test_info __start_test, __stop_test;

int test_case_depth = 0;
struct test_case_info test_case_info[7];
struct test_test_info *currtest;
struct test_test_info *currsuite;

/* Free variable. */
UNUSED static
void varfree(char *const*const var) {
	free(*var);
}

#define US_NS 1000ul
#define MS_NS 1000000ul
#define SEC_NS 1000000000ul

/** Write content of `fd` to stderr. */
static
void cat(int fd) {
	char buf[(1 << 12)];
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
CONST static
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
CONST static
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
CONST static
unsigned long tstons(struct timespec const *const ts) {
	return ts->tv_sec * SEC_NS + ts->tv_nsec;
}

/** Compute elapsed time from `start` time to `end` time. Write result into `result`. */
static
void tsdiff(struct timespec const *end, struct timespec const *start, struct timespec *result) {
	result->tv_sec  = end->tv_sec  - start->tv_sec  + (end->tv_nsec < start->tv_nsec);
	result->tv_nsec = end->tv_nsec - start->tv_nsec + (end->tv_nsec < start->tv_nsec ? SEC_NS : 0);
}

#ifdef TEST_OUTPUT_HUMAN
/** Print a `n` character wide horizontal separator using `ch` character. */
static
void print_hline(int n, char ch) {
	int i;

	for (i = 0;i < n;++i)
		fputc(ch, stdout);
	fputc('\n', stdout);
}
#endif

#if TEST_OUTPUT_XML
/** XML escape `text`. */
static
char *xmlesc(char const *const text) {
	char const *inp;
	char *esctext = NULL;
	char *outp = 0;

#define EMIT(e) \
	if (esctext) \
		outp = memcpy(outp, e, sizeof(e) - sizeof(char)) + sizeof(e) / sizeof(char) - 1; \
	else \
		outp += sizeof(e) / sizeof(char) - 1; \
	break;

escape:
	/* Computes length in the first pass, writes in the second. */
	for (inp = text - 1;;) {
		switch (*++inp) {
		default:
			if (esctext)
				*outp++ = *inp;
			else
				++outp;
			break;
		case '"':  EMIT("&quot;")
		case '&':  EMIT("&amp;");
		case '\'': EMIT("&apos;");
		case '<':  EMIT("&lt;");
		case '>':  EMIT("&gt;");
		case '\0': 
			if (esctext)
				*outp = '\0';
			else
				++outp;
			goto end_loop;
		}
	}
end_loop:
#undef EMIT

	if (!esctext) {
		esctext = outp = malloc((size_t)outp);
		if (!esctext) {
			fprintf(stderr, "malloc: unable to allocate %lu bytes",
				(size_t)outp);
			exit(EXIT_FAILURE);
		}
		goto escape;
	}

	return esctext;
}
#endif

UNUSED static
char signame[NSIG][7];

/** Default signal handler. */
static
void sighandler(int sig, siginfo_t *info, void *ucontext) {
	void *array[50];
	int size;

	psiginfo(info, "Unhandled signal");

	size = backtrace(array, 50);

	fprintf(stderr, "\nBacktrace:\n");
	backtrace_symbols_fd(array, size, STDERR_FILENO);

	signal(sig, SIG_DFL);
	raise(sig);
}

/** Initialize signal handling. */
static
void sighandler_init(void) {
	int sig;

	static struct sigaction sa;
	sa.sa_sigaction = sighandler;
	sa.sa_flags = SA_RESTART | SA_SIGINFO;
	sigemptyset(&sa.sa_mask);

	/* Use pretty signal names if action is Core, Stop or Term and then attach
	 * a signal handler for it. Otherwise just use its number. */

#define SIG(name) \
    strncpy(signame[SIG##name], #name, sizeof(*signame));
	
	SIG(ABRT  ) SIG(ALRM  ) SIG(BUS   ) SIG(FPE   ) SIG(HUP   )
	SIG(ILL   ) SIG(INT   ) SIG(KILL  ) SIG(PIPE  ) SIG(POLL  )
	SIG(PROF  ) SIG(QUIT  ) SIG(SEGV  ) SIG(STOP  ) SIG(TSTP  )
	SIG(SYS   ) SIG(TERM  ) SIG(TRAP  ) SIG(TTIN  ) SIG(TTOU  )
	SIG(USR1  ) SIG(USR2  ) SIG(VTALRM) SIG(XCPU  ) SIG(XFSZ  )

#undef SIG

	for (sig = 1; sig < NSIG; ++sig) {
		if (signame[sig][0])
			sigaction(sig, &sa, NULL);
		else
			snprintf((char*)(signame + sig), sizeof(*signame), "(%02d)", sig);
	}
}

static
void fire_event(int event) {
	struct test_test_info *tti;

	for (tti = &__start_test;tti < currtest;++tti) {
		unsigned const scope = tti->line;

		if (tti->file)
			continue;

		if (2/*global*/ == scope ||
		   (1/*suite */ == scope && tti > currsuite))
			((void(*)(int))tti->run)(event); /* TODO: Make an union. */
	}
}

int main(int argc, char **argv) {

	enum { test_suite, test_total, test_passed, test_failed, test_skipped, test_result_count };
	unsigned stat[test_result_count] = {0};
	int width = 0;
	int gathered UNUSED = 0;
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

	for (currtest = &__start_test;currtest < &__stop_test;++currtest) {
		int len;
		
		/* One struct is enough for everything. */
		if (currtest->run) {
			if (!currtest->file)
				/* Runnable. */
				continue;

			++stat[test_total];

			/* Count tests in suites. */
			if (currsuite)
				++currsuite->line;

			if (!currtest->desc)
				continue;

			len = strlen(currtest->desc) + OUTPUT_HUMAN_TEST_MINLEN;
			if (len > width)
				width = len;

		} else {
			currsuite = currtest;
			++stat[test_suite];

			len = strlen(currtest->desc) + strlen(currtest->file) + OUTPUT_HUMAN_SUITE_MINLEN;
			if (len > width)
				width = len;
		}
	}

	if (width > 72)
		width = 72;

#ifdef TEST_OUTPUT_HUMAN
	fprintf(stdout, "running %u tests\n",
		stat[test_total]);
#elif defined(TEST_OUTPUT_LIBCHECKXML)
	{
		char datetime[23];
		time_t now = time(NULL);
		struct tm *localnow = localtime(&now);
		if (!localnow) {
			perror("localtime");
			exit(EXIT_FAILURE);
		}

		strftime(datetime, sizeof(datetime), "%Y-%m-%d %T", localnow);
		fprintf(stdout, OUTPUT_LIBCHECK_TEMPLATE_START
			datetime);
	}
#endif

	shm_total_ns = mmap(NULL, sizeof(*shm_total_ns),
		PROT_READ | PROT_WRITE,
		MAP_SHARED | MAP_ANONYMOUS, -1, 0);

	if (MAP_FAILED == shm_total_ns) {
		perror("mmap");
		exit(EXIT_FAILURE);
	}

	currsuite = NULL;
	for (currtest = &__start_test;currtest < &__stop_test;++currtest) {
		int argi;
		int skip;

		if (!currtest->run) {
			/* Suite start marker. */
			currsuite = currtest;
#ifdef TEST_OUTPUT_HUMAN
			fprintf(stdout, "\nsuite " ANSI_BOLD "%s" ANSI_RESET " (%s):\n",
				currtest->desc, currtest->file);
			print_hline(width, '-');
#elif defined(TEST_OUTPUT_LIBCHECKXML)
			fprintf(stdout, OUTPUT_LIBCHECK_TEMPLATE_SUITE_START,
				currtest->desc);
#endif
			fire_event(test_hook_setup_suite);
			goto next_test;
		}

		if (!currtest->file)
			/* Runnable. */
			continue;

#ifdef TEST_OUTPUT_HUMAN
		fprintf(stdout, "test " ANSI_BOLD "%s" ANSI_RESET " ... %*s",
			currtest->desc,
			width - OUTPUT_HUMAN_TEST_MINLEN - (int)strlen(currtest->desc), "");
		fflush(stdout);
#endif

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

			if (strstr(currtest->desc, filter))
				skip = should_skip;
		}

		if (skip || 0 == currtest->iters) {
		test_skip:
			++stat[test_skipped];
#ifdef TEST_OUTPUT_HUMAN
			fprintf(stdout, ANSI_BLUE ANSI_BOLD "skipped" ANSI_RESET "\n");
#endif
			goto drop_output;
		}

		currtest->outfd = memfd_create(currtest->desc, 0);
		if (-1 == currtest->outfd) {
			perror("memfd_create");
			exit(EXIT_FAILURE);
		}

		*shm_total_ns = -1;

		fire_event(test_hook_setup_test);

		switch (TEST_FORK_IMPL_) {
		case -1:
			perror("fork");
			exit(EXIT_FAILURE);
		case 0: {
			struct timespec ts_test_start;
			struct timespec ts_test_end;
			unsigned iters = currtest->iters;
#ifdef TEST_NOFORK
			int result = EXIT_SUCCESS;
			int oldstderr = dup(STDERR_FILENO);
			int oldstdout = dup(STDOUT_FILENO);
#endif
			dup2(currtest->outfd, STDERR_FILENO);
			setvbuf(stderr, NULL, _IONBF, 0);
#ifdef TEST_OUTPUT_HUMAN
			dup2(currtest->outfd, STDOUT_FILENO);
			setvbuf(stdout, NULL, _IONBF, 0);
#elif defined(TEST_OUTPUT_LIBCHECKXML)
			dup2(nullfd, STDOUT_FILENO);
#endif

			clock_gettime(CLOCK_MONOTONIC, &ts_test_start);
#ifndef TEST_NOFORK
			while (iters-- > 0)
				currtest->run();
#else
			test_case_depth = 0;
			while (iters-- > 0 && EXIT_SUCCESS != (currtest->run(&result), result))
				;
#endif
			clock_gettime(CLOCK_MONOTONIC, &ts_test_end);

			tsdiff(&ts_test_end, &ts_test_start, &ts_test_start);
			*shm_total_ns = tstons(&ts_test_start);

#ifndef TEST_NOFORK
			exit(EXIT_SUCCESS);
#else
			dup2(oldstderr, STDERR_FILENO);
			dup2(oldstdout, STDOUT_FILENO);

			if (EXIT_SUCCESS == result)
				goto test_passed;
			else
				goto test_failed;
#endif
		}
		default: {
			int stat_val;
			/* Wait test to finish */
			wait(&stat_val);
			/* Exited normally. */
			if (WIFEXITED(stat_val)) {
				switch (WEXITSTATUS(stat_val)) {
				case EXIT_SUCCESS: {
					char info[66];

#ifdef TEST_NOFORK
				test_passed:
#endif
					if (-1 == *shm_total_ns) {
#ifdef TEST_OUTPUT_HUMAN
						fprintf(stdout, ANSI_RED ANSI_BOLD "not ok" ANSI_RESET "\n");
						goto test_fail_action;
#elif defined(TEST_OUTPUT_LIBCHECKXML)
						goto test_failed;
#endif
					}

					currtest->total_ns = *shm_total_ns;

					if (currtest->iters == 1)
						snprintf(info, sizeof(info), "%4lu %s",
							nstohtime(currtest->total_ns),
							nstohunit(currtest->total_ns));
					else
						snprintf(info, sizeof(info), "%4lu %s / %9lu iters = %4lu %s/iter",
							nstohtime(currtest->total_ns),
							nstohunit(currtest->total_ns),
							currtest->iters,
							nstohtime((currtest->total_ns + currtest->iters - 1) / currtest->iters),
							nstohunit((currtest->total_ns + currtest->iters - 1) / currtest->iters));

					++stat[test_passed];
#ifdef TEST_OUTPUT_HUMAN
					fprintf(stdout, ANSI_GREEN ANSI_BOLD "ok" ANSI_RESET ", %s\n", info);
#elif defined(TEST_OUTPUT_LIBCHECKXML)
					{
						char *path FREE = strdup(currtest->file);
						char *dname FREE = xmlesc(dirname(path));
						char *bname FREE = xmlesc(basename(path));
						char *escdesc FREE = xmlesc(currtest->desc);
						fprintf(stdout, OUTPUT_LIBCHECK_TEMPLATE_TEST,
							"success", dname, bname, currtest->line,
							escdesc,
							currtest->iters - 1l,
							(float)currtest->total_ns / SEC_NS,
							"Core", "Passed");
					}
#endif
					goto drop_output;
				}
				case 77: /* Special exit code. */
					goto test_skip;
				default: /* Others. */
#ifdef TEST_OUTPUT_HUMAN
					fprintf(stdout, ANSI_RED ANSI_BOLD "FAILED" ANSI_RESET "\n");
#elif defined(TEST_OUTPUT_LIBCHECKXML)
				test_failed:
					{
						char *path FREE = strdup(currtest->file);
						char *dname FREE = xmlesc(dirname(path));
						char *bname FREE = xmlesc(basename(path));
						char *escerr FREE = NULL;
						off_t errlen;

						write(currtest->outfd, "\0", sizeof(char));
						errlen = lseek(currtest->outfd, 0, SEEK_CUR);
						if (errlen > 1) {
							char *errtext = mmap(NULL, errlen * sizeof(char),
									PROT_READ, MAP_SHARED,
									currtest->outfd, 0);
							escerr = xmlesc(errtext);
							munmap(errtext, errlen * sizeof(char));
						}

						fprintf(stdout, OUTPUT_LIBCHECK_TEMPLATE_TEST,
							"failure", dname, bname, currtest->line,
							currtest->desc, -1l, -1.0,
							"Core", escerr ? escerr : "Failure");
					}
#endif
					goto test_fail_action;
				}
			} else {
#ifdef TEST_OUTPUT_HUMAN

				fprintf(stdout, ANSI_RED ANSI_BOLD "SIG%s" ANSI_RESET "\n",
					signame[WTERMSIG(stat_val)]);
#elif defined(TEST_OUTPUT_LIBCHECKXML)
				{
					char *path FREE = strdup(currtest->file);
					char *dname FREE = xmlesc(dirname(path));
					char *bname FREE = xmlesc(basename(path));
					char *escdesc FREE = xmlesc(currtest->desc);
					fprintf(stdout, OUTPUT_LIBCHECK_TEMPLATE_TEST,
						"failure", dname, bname, currtest->line,
						escdesc, -1l, -1.0,
						"Core", strsignal(WTERMSIG(stat_val)));
				}
#endif
			test_fail_action:
				++stat[test_failed];

#ifdef TEST_NOGATHEROUTPUT
				cat(currtest->outfd);
#elif defined(TEST_OUTPUT_HUMAN)
				goto next_test; /* Keep output. */
#endif
			}
		}
		}

		fire_event(test_hook_teardown_test);

	drop_output:
		close(currtest->outfd);
		currtest->outfd = -1;
	next_test:
		if (currsuite) {
			if (0 == currsuite->line) {
#ifdef TEST_OUTPUT_LIBCHECKXML
				fprintf(stdout, OUTPUT_LIBCHECK_TEMPLATE_SUITE_END);
#endif
				fire_event(test_hook_teardown_suite);
			} else {
				--currsuite->line;
			}
		}
	}

	munmap(shm_total_ns, sizeof(*shm_total_ns));

#ifndef TEST_NOGATHEROUTPUT
# ifdef TEST_OUTPUT_HUMAN
	print_hline(width, '=');
# endif
	for (currtest = &__start_test;currtest < &__stop_test;++currtest) {
		if (-1 == currtest->outfd)
			continue;

		gathered = 1;
		fprintf(stderr, "test " ANSI_BOLD "%s" ANSI_RESET " (%s:%u):\n",
			currtest->desc, currtest->file, currtest->line);
		cat(currtest->outfd);

		close(currtest->outfd);
		currtest->outfd = -1;
	}
#endif

	clock_gettime(CLOCK_MONOTONIC, &ts_end);
	tsdiff(&ts_end, &ts_start, &ts_start);

#ifdef TEST_OUTPUT_HUMAN
	if (gathered)
		print_hline(width, '=');
	fprintf(stdout,
		ANSI_BOLD "test result" ANSI_RESET ": " ANSI_BOLD "%s" ANSI_RESET ". "
		ANSI_GREEN "%u passed"  ANSI_RESET "; "
		ANSI_RED   "%u failed"  ANSI_RESET "; "
		ANSI_BLUE  "%u skipped" ANSI_RESET "\n",
		0 == stat[test_failed] ? ANSI_GREEN "ok" : ANSI_RED "FAILED",
		stat[test_passed],
		stat[test_failed],
		stat[test_skipped]);
#elif defined(TEST_OUTPUT_LIBCHECKXML)
	fprintf(stdout, OUTPUT_LIBCHECK_TEMPLATE_END,
		(float)tstons(&ts_start) / SEC_NS);
#endif

#ifndef TEST_NOLIBSEGFAULT
	dlclose(dlhandle);
#endif

	exit(0 == stat[test_failed] ? EXIT_SUCCESS : EXIT_FAILURE);
}
/* vim:set ft=c ts=4 sw=4 noet: */
