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
#ifndef TEST_NOLIBSEGFAULT
# include <dlfcn.h> /* dlopen */
#else
# include <execinfo.h> /* backtrace, backtrace_symbols_fd */
#endif
#include <limits.h> /* LONG_MAX */
#include <signal.h> /* signal */
#include <sys/mman.h> /* memfd_create */
#include <time.h> /* clock_gettime */

#include "test.h"

int test_case_depth = 0;
struct test_case_info test_case_info[7];
struct test_test_info *currtest;

#define US_NS 1000ul
#define MS_NS 1000000ul
#define SEC_NS 1000000000ul

static void cat(int fd) {
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

static unsigned long nstohtime(unsigned long ns) {
	if (ns < 10 * US_NS)
		return ns;
	else if (ns < 10 * MS_NS)
		return ns / US_NS;
	else if (ns < 10 * SEC_NS)
		return ns / MS_NS;
	else
		return ns / SEC_NS;
}

static char const *nstohunit(unsigned long ns) {
	if (ns < 10 * US_NS)
		return "ns";
	else if (ns < 10 * MS_NS)
		return "us";
	else if (ns < 10 * SEC_NS)
		return "ms";
	else
		return "s";
}

static unsigned long tstons(struct timespec const *const ts) {
	return ts->tv_sec * SEC_NS + ts->tv_nsec;
}

static void tsdiff(struct timespec *end, struct timespec *start, struct timespec *result) {
	result->tv_sec  = end->tv_sec  - start->tv_sec  + (end->tv_nsec < start->tv_nsec);
	result->tv_nsec = end->tv_nsec - start->tv_nsec + (end->tv_nsec < start->tv_nsec ? SEC_NS : 0);
}

static void print_hline(int n, char ch) {
	int i;

	for (i = 0;i < n;++i)
		fputc(ch, stdout);
	fputc('\n', stdout);
}

#ifdef TEST_NOLIBSEGFAULT
void sighandler(int sig, siginfo_t *info, void *ucontext)
{
	void *array[50];
	int size;

	fprintf(stderr, "*** %s, address is %p\n\n",
		strsignal(sig), info->si_addr);

	size = backtrace(array, 50);

	fprintf(stderr, "Backtrace:\n");
	backtrace_symbols_fd(array, size, STDERR_FILENO);

	exit(EXIT_FAILURE);
}
#endif

int main(int argc, char **argv) {

	enum { test_unit, test_total, test_passed, test_failed, test_skipped, test_result_count };
	unsigned stat[test_result_count] = {0};
	int width = 0;
	unsigned long *shm_total_ns;
#ifndef TEST_NOLIBSEGFAULT
	void *dlhandle;

	dlhandle = dlopen("/lib/libSegFault.so", RTLD_LAZY);
	if (!dlhandle)
		/* Don't treat it fatal error. */
		perror("dlopen");
#else
	struct sigaction sigact;

	sigact.sa_sigaction = sighandler;
	sigact.sa_flags = SA_RESTART | SA_SIGINFO;

	if (sigaction(SIGSEGV, &sigact, NULL)) {
		perror("sigaction");
		exit(EXIT_FAILURE);
	}
#endif

	for (currtest = &__start_test;currtest < &__stop_test;++currtest) {
		int len;
		
		if (currtest->run) {
			++stat[test_total];

			if (!currtest->desc)
				continue;

			len = strlen(currtest->desc) + 17;
			if (len > width)
				width = len;

		} else {
			++stat[test_unit];

			len = 5 + strlen(currtest->desc) + strlen(currtest->file) + 4;
			if (len > width)
				width = len;
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

	for (currtest = &__start_test;currtest < &__stop_test;++currtest) {
		char succ_info[50];
		int argi;
		int skip;

		if (!currtest->run) {
			fprintf(stdout, "\nunit " ANSI_BOLD "%s" ANSI_RESET " (%s):\n",
				currtest->desc, currtest->file);

			print_hline(width, '-');
			continue;
		}

		fprintf(stdout, "test " ANSI_BOLD "%s" ANSI_RESET " ... %*s",
			currtest->desc,
			width - 17 - (int)strlen(currtest->desc), "");
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

			if (strstr(currtest->desc, filter))
				skip = should_skip;
		}

		if (skip || 0 == currtest->repeat) {
			++stat[test_skipped];
			fprintf(stdout, ANSI_BLUE ANSI_BOLD "skipped" ANSI_RESET "\n");
			continue;
		}

		currtest->outfd = memfd_create(currtest->desc, 0);
		if (-1 == currtest->outfd) {
			perror("memfd_create");
			exit(EXIT_FAILURE);
		}

		switch (fork()) {
		case -1:
			perror("fork");
			exit(EXIT_FAILURE);
		case 0: {
			struct timespec ts_start;
			struct timespec ts_end;
			unsigned repeat = currtest->repeat;

			/* Run test in forked children. */
			dup2(currtest->outfd, STDERR_FILENO);
			dup2(currtest->outfd, STDOUT_FILENO);

			/* Do not buffer output streams. */
			setvbuf(stdout, NULL, _IONBF, 0);
			setvbuf(stderr, NULL, _IONBF, 0);

			clock_gettime(CLOCK_MONOTONIC, &ts_start);
			while (repeat-- > 0)
				currtest->run();
			clock_gettime(CLOCK_MONOTONIC, &ts_end);

			tsdiff(&ts_end, &ts_start, &ts_start);
			*shm_total_ns = tstons(&ts_start);

			exit(EXIT_SUCCESS);
			break;
		}
		default:
			/* Wait test to be finished. */ \
			wait(&currtest->status);

			currtest->total_ns = *shm_total_ns;

			++stat[(EXIT_SUCCESS == currtest->status ? test_passed : test_failed)];

			if (EXIT_SUCCESS == currtest->status) {
				if (currtest->repeat <= 1)
					sprintf(succ_info, ", %4lu %s",
						nstohtime(currtest->total_ns),
						nstohunit(currtest->total_ns));
				else
					sprintf(succ_info, ", %4lu %s / %9lu iters = %4lu %s/iter",
						nstohtime(currtest->total_ns),
						nstohunit(currtest->total_ns),
						currtest->repeat,
						nstohtime((currtest->total_ns + currtest->repeat - 1) / currtest->repeat),
						nstohunit((currtest->total_ns + currtest->repeat - 1) / currtest->repeat));
			} else {
				succ_info[0] = '\0';
			}

			fprintf(stdout, ANSI_BOLD "%s" ANSI_RESET "%s\n",
				EXIT_SUCCESS == currtest->status
				 ? ANSI_GREEN "ok"
				 : ANSI_RED "FAILED",
				succ_info);

			if ('I' == TEST_OUTPUT) {
				if (EXIT_SUCCESS != currtest->status) {
					cat(currtest->outfd);
				}
			}
		}

	}

	munmap(shm_total_ns, sizeof(*shm_total_ns));

	if ('A' == TEST_OUTPUT && stat[test_failed] > 0) {
		print_hline(width, '=');

		for (currtest = &__start_test;++currtest < &__stop_test;) {
			if (EXIT_SUCCESS != currtest->status) {
				fprintf(stderr, "test " ANSI_BOLD "%s" ANSI_RESET " (%s:%u):\n",
					currtest->desc, currtest->file, currtest->line);
				cat(currtest->outfd);
				fprintf(stderr, "\n");
			}
		}
	}

	print_hline(width, '=');

	fprintf(stdout,
		ANSI_BOLD "test result" ANSI_RESET ": " ANSI_BOLD "%s" ANSI_RESET ". "
		ANSI_GREEN "%u passed"  ANSI_RESET "; "
		ANSI_RED   "%u failed"  ANSI_RESET "; "
		ANSI_BLUE  "%u skipped" ANSI_RESET "\n",
		0 == stat[test_failed]
		 ? ANSI_GREEN "ok"
		 : ANSI_RED "FAILED",
		stat[test_passed],
		stat[test_failed],
		stat[test_skipped]);

#ifndef TEST_NOLIBSEGFAULT
	dlclose(dlhandle);
#endif

	exit(0 == stat[test_failed]
		 ? EXIT_SUCCESS
		 : EXIT_FAILURE);
}
/* vim:set ft=c ts=4 sw=4 noet: */
