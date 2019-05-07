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
#ifndef TEST_H
#define TEST_H

#include <fcntl.h> /* O_WRONLY */
#include <stdio.h> /* fprintf, perror */
#include <stdlib.h> /* exit */
#include <string.h> /* strcmp, memcmp */
#include <sys/wait.h> /* wait */
#include <unistd.h> /* fork, dup2 */
#include <stdint.h> /* size_t */

#ifndef TEST_CASESEP
# define TEST_CASESEP " / "
#endif

#ifndef TEST_NOSTYLES
# define ANSI_BOLD  "\x1b[1m"
# define ANSI_RESET "\x1b[0m"
# ifndef TEST_NOCOLORS
#  define ANSI_RED "\x1b[31m"
#  define ANSI_GREEN "\x1b[32m"
#  define ANSI_BLUE "\x1b[34m"
# else
#  define ANSI_RED ""
#  define ANSI_GREEN ""
#  define ANSI_BLUE ""
# endif
#else
#define ANSI_BOLD  ""
#define ANSI_RESET ""
#define ANSI_RED ""
#define ANSI_GREEN ""
#define ANSI_BLUE ""
#endif

#ifndef TEST_NOFORK
# define TEST_PARAM_ void
# define TEST_FORK_IMPL_ fork()
# define TEST_CASE_EXIT_IMPL exit(EXIT_SUCCESS)
# define FAILURE(code) exit((code));
#else
# define TEST_PARAM_ int *const _test_result
# define TEST_FORK_IMPL_ 0
# define TEST_CASE_EXIT_IMPL ftruncate(currtest->outfd, test_case_info[--test_case_depth].outpos)
# define FAILURE(code) do { *_test_result = (code); return; } while (0);
#endif

/** Info about tests and suites. */
struct test_test_info {
	void(*const run)(TEST_PARAM_)  __attribute__((aligned(16)));
	char const *const desc;
	char const *const file;
	unsigned line;
	unsigned long const iters;
	int outfd;
	unsigned long total_ns;
	char a[4];
};

/** Info about test case. */
struct test_case_info {
	char const *name;
	char const *file;
	unsigned line;
	off_t outpos;
};

extern int test_case_depth;
extern struct test_case_info test_case_info[7];
extern struct test_test_info *currtest;
extern struct test_test_info *currsuite;

/* Default XML output. */
#ifdef TEST_OUTPUT_XML
# define TEST_OUTPUT_LIBCHECKXML
#endif

/* Default output format. */
#if !defined(TEST_OUTPUT_HUMAN) && \
    !defined(TEST_OUTPUT_LIBCHECKXML)
# define TEST_OUTPUT_HUMAN
#endif

#define BENCHIFND_(static_rtype, name, desc, repeat) \
	static_rtype name(TEST_PARAM_); \
	TEST_VAR_(name) = {name, desc, __FILE__, __LINE__, repeat, -1}; \
	static_rtype name(TEST_PARAM_)

/* Mothers of everything. */
#define BENCHIFND(name, desc, repeat, cond) \
	BENCHIFND_(void, name, desc, !!(cond) * repeat)

#define BENCHND(name, desc, repeat) \
	BENCHIFND(name, desc, repeat, 1)

#define TESTIFND(name, desc, cond) \
	BENCHIFND(name, desc, 1, cond)

#define TESTND(name, desc, cond) \
	TESTIFND(name, desc, cond)

/* Must be static, because we can't generate unique names. */
#define BENCHIFD(desc, repeat, cond) \
	BENCHIFND_(static void, TOKENPASTE(test_at_line_, __LINE__), desc, !!(cond) * repeat)

#define BENCHD(desc, repeat) \
	BENCHIFD(desc, repeat, 1)

#define TESTIFD(desc, cond) \
	BENCHIFD(desc, 1, cond)

#define TESTD(desc) \
	TESTIFD(desc, 1)

#define BENCHIF(name, repeat, cond) \
	BENCHIFND(name, #name, repeat, cond)

#define BENCH(name, repeat) \
	BENCHIF(name, repeat, 1)

#define TESTIF(name, cond) \
	BENCHIF(name, 1, cond)

#define TEST(name) \
	TESTIF(name, 1)

/* Have to hack with rtype, because empty macro argument is not valid. */

#define CASE(name) \
	CASED(#name)

#define CASED(case_desc) \
	assert_msg(test_case_depth < (sizeof(test_case_info) / sizeof(*test_case_info)), "cannot nest more cases"); \
	test_case_info[test_case_depth].name = (case_desc); \
	test_case_info[test_case_depth].file = __FILE__; \
	test_case_info[test_case_depth].line = __LINE__; \
	test_case_info[test_case_depth].outpos = lseek(currtest->outfd, 0, SEEK_CUR); \
\
	switch (TEST_FORK_IMPL_) { \
	case -1: \
		perror("fork"); \
		exit(EXIT_FAILURE); \
	case 0: { \
		int i; \
		++test_case_depth; \
\
		/* Prints what test cases we are in. */ \
		fprintf(stdout, "case "); \
		for (i = 0;i < test_case_depth - 1;++i) \
			fprintf(stdout, ANSI_BOLD "%s" ANSI_RESET TEST_CASESEP, \
				test_case_info[i].name); \
\
		fprintf(stdout, ANSI_BOLD "%s" ANSI_RESET " (%s:%u):\n", \
			test_case_info[i].name, \
			test_case_info[i].file, \
			test_case_info[i].line \
		); \
		goto TOKENPASTE(test_case_run_, __LINE__); \
	} \
	default: { \
		int stat_val; \
		 /* Wait test to finish. */ \
		wait(&stat_val); \
		if (WIFEXITED(stat_val) && EXIT_SUCCESS == WEXITSTATUS(stat_val)) \
			/* We don't care about output of passed cases. */ \
			ftruncate(currtest->outfd, test_case_info[test_case_depth].outpos); \
		else \
			/* Propagate *exit code*. */ \
			exit(WIFEXITED(stat_val) ? WEXITSTATUS(stat_val) : EXIT_FAILURE); \
	} \
	} \
	for (;0;TEST_CASE_EXIT_IMPL) TOKENPASTE(test_case_run_, __LINE__):

#define TEST_VAR_(name) \
	static struct test_test_info test_##name __attribute__((no_reorder, __used__, unused, __section__("test"))) \

#define SUITE(name) \
	SUITED(#name)

#define SUITED(desc) \
	SUITE_(desc)

#define SUITE_(desc) \
	TEST_VAR_(unit) = {NULL, desc, __FILE__, 0, 0, -1};

enum { test_hook_setup_suite, test_hook_teardown_suite, test_hook_setup_test, test_hook_teardown_test };

#define HOOK_SUITE_ HOOK__SCOPE__(1)
#define HOOK_GLOBAL_ HOOK__SCOPE__(2)

#define HOOK(scope) \
	TOKENPASTE3(HOOK_, scope, _)

#define SETUP() \
	static void test_setup(void); \
	HOOK(SUITE) \
	{ \
		if (test_hook_setup_suite == event) \
			test_setup(); \
	} \
	static void test_setup(void)

#define TEARDOWN() \
	static void test_teardown(void); \
	HOOK(SUITE) \
	{ \
		if (test_hook_teardown_suite == event) \
			test_teardown(); \
	} \
	static void test_teardown(void)

#define HOOK__SCOPE__(scope) \
	HOOK__SCOPE___(scope, TOKENPASTE(test_hook_at_line_, __LINE__))

#define HOOK__SCOPE___(scope, name) \
	static void name(int); \
	TEST_VAR_(name) = {(void(*)(void))name, NULL, NULL, scope, 0, -1}; \
	static void name(int event)

#define STRINGIFY(t) #t
#define TOKENPASTE_(a, b) a##b
#define TOKENPASTE3_(a, b, c) a##b##c
#define TOKENPASTE(a, b) TOKENPASTE_(a, b)
#define TOKENPASTE3(a, b, c) TOKENPASTE3_(a, b, c)

#ifdef TEST_OUTPUT_HUMAN
# define TEST_ASSERT_LOC __FILE__, __LINE__,
# define TEST_ASSERT_LOCFMT "%s:%u: "
#else
# define TEST_ASSERT_LOC
# define TEST_ASSERT_LOCFMT
#endif

#define TEST_ASSERT_FORMAT \
	TEST_ASSERT_LOCFMT ANSI_RED ANSI_BOLD "Assertion" ANSI_RESET " %s failed.\n"
#define TEST_ASSERT_FORMAT_CMPTWO(fmt) \
	TEST_ASSERT_LOCFMT ANSI_RED ANSI_BOLD "Assertion" ANSI_RESET " %s failed; left=" fmt ", right=" fmt ".\n"
#define TEST_ASSERT_FORMAT_CMPONE(fmt) \
	TEST_ASSERT_LOCFMT ANSI_RED ANSI_BOLD "Assertion" ANSI_RESET " %s failed; got " fmt ".\n"

#define TEST_ASSERT_FORMAT_CMPSEL(lhs, rhs) \
__builtin_choose_expr(__builtin_constant_p(lhs) + __builtin_constant_p(rhs) == 1, \
		TEST_ASSERT_FORMAT_CMPSEL_GEN_(__typeof__(rhs), ONE), \
		TEST_ASSERT_FORMAT_CMPSEL_GEN_(__typeof__(rhs), TWO))

#define TEST_ASSERT_FORMAT_CMPSEL_GEN_(type, kind) \
	TEST_CHOEXPR_(TEST_ISTYPE_(type, char *) || TEST_ISTYPE_(type, char[]), \
	              TEST_ASSERT_FORMAT_CMP##kind("\"%s\""), \
	TEST_CHOEXPR_(__builtin_types_compatible_p(type, char), \
	              TEST_ASSERT_FORMAT_CMP##kind("'%c'"), \
	TEST_CHOEXPR_(__builtin_types_compatible_p(type, double), \
	              TEST_ASSERT_FORMAT_CMP##kind("%f"), \
	              TEST_ASSERT_FORMAT_CMP##kind("%d"))))

#define assert_msg(c, msg) \
	if (!(c)) {  \
		fprintf(stderr, TEST_ASSERT_FORMAT, TEST_ASSERT_LOC msg); \
		FAILURE(EXIT_FAILURE); \
	}

#define assert(c) \
	assert_msg(c, "`" #c "'")

#define assert_memequal(lhs, rhs, size) \
	assert(0 == memcmp(lhs, rhs, size));

#define TEST_ISTYPE_(val, type) \
	__builtin_types_compatible_p(__typeof__(val), type)

#define TEST_CHOEXPR_(cond, thenexpr, elseexpr) \
	__builtin_choose_expr(cond, thenexpr, elseexpr)

#define assert_cmp_(lhs, rhs) \
	TEST_CHOEXPR_(TEST_ISTYPE_(lhs, char *) || TEST_ISTYPE_(lhs, char[]), \
	              (0 == strcmp((char *)(size_t)lhs, (char *)(size_t)rhs)), \
	              (lhs == rhs))

/* I really hope every type is here. */
union test_converter_any_union {
	void *ptr;
	void const *const_ptr;
	float flt;
	double dbl;
	char c;
	unsigned char uc;
	short s;
	unsigned short us;
	int i;
	unsigned int ui;
	long l;
	unsigned long ul;
} __attribute__((__transparent_union__));

inline __attribute__((const))
double test_any2dbl(union test_converter_any_union u) {
	return u.dbl; /* Awesome... */
}

#define TEST_TYPEFORPRINT(val) \
	__builtin_choose_expr(__builtin_types_compatible_p(__typeof__(val), float), test_any2dbl(val), val)

#define assert_eq assert_equal
#define assert_lt assert_less
#define assert_gt assert_less

#define static_assert(cond, msg) typedef char _test_static_assert_lhs_and_rhs_differ_in_type[cond - 1];

#define assert_equal(lhs, rhs) \
do { \
	__typeof__(lhs) const lhsv = (lhs); \
	__typeof__(rhs) const rhsv = (rhs); \
	if (!assert_cmp_(lhsv, rhsv)) { \
		fprintf(stderr, TEST_ASSERT_FORMAT_CMPSEL(lhs, rhs), \
			TEST_ASSERT_LOC \
			STRINGIFY(lhs == rhs), \
			__builtin_choose_expr(__builtin_constant_p(rhs) || !__builtin_constant_p(lhs), TEST_TYPEFORPRINT(lhsv), TEST_TYPEFORPRINT(rhsv)), \
			TEST_TYPEFORPRINT(rhsv)), \
		FAILURE(EXIT_FAILURE); \
	} \
} while (0);

#endif /* TEST_H */
/* vim:set ft=c ts=4 sw=4 noet: */
