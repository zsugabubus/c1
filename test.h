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
#ifndef TEST_H
#define TEST_H

#include <alloca.h> /* alloca */
#include <fcntl.h> /* O_WRONLY */
#include <setjmp.h> /* setjmp, longjmp */
#include <stdint.h> /* size_t */
#include <stdio.h> /* fprintf, perror */
#include <stdlib.h> /* exit */
#include <string.h> /* strcmp, memcmp */
#include <sys/types.h> /* ftruncate */
#include <sys/wait.h> /* wait */
#include <unistd.h> /* fork, dup2, ftruncate */

#ifndef TEST_VERBOSE
# define TEST_VERBOSE 0
#endif

#define EXIT_SKIP 77
#define EXIT_NONFATAL_FAILURE 123

extern struct test_suite_info *currsuite;
extern struct test_test_info *currtest;
extern struct test_case_info *currcase;

#define BENCHIFND_(static_rtype, id, name, repeat) \
	static_rtype id(void); \
	TEST_VAR_(test, id) = {{test_object_id_test}, name, __FILE__, __LINE__, id, repeat, -1, 0, EXIT_SUCCESS}; \
	static_rtype id(void)

/* Mothers of everything. */
#define BENCHIFND(id, name, repeat, cond) \
	BENCHIFND_(void, id, name, !!(cond) * repeat)

#define BENCHND(id, name, repeat) \
	BENCHIFND(id, name, repeat, 1)

#define TESTIFND(id, name, cond) \
	BENCHIFND(id, name, 1, cond)

#define TESTND(id, name, cond) \
	TESTIFND(id, name, cond)

/* Must be static, because we can't generate unique names. */
#define BENCHIFD(name, repeat, cond) \
	BENCHIFND_(static void, TOKENPASTE(test_at_line_, __LINE__), name, !!(cond) * repeat)

#define BENCHD(name, repeat) \
	BENCHIFD(name, repeat, 1)

#define TESTIFD(name, cond) \
	BENCHIFD(name, 1, cond)

#define TESTD(name) \
	TESTIFD(#name, 1)

#define BENCHIF(id, repeat, cond) \
	BENCHIFND(id, #id, repeat, cond)

#define BENCH(id, repeat) \
	BENCHIF(id, repeat, 1)

#define TESTIF(id, cond) \
	BENCHIF(id, 1, cond)

#define TEST(id) \
	TESTIF(id, 1)

/* Have to hack with rtype, because empty macro argument is not valid. */

#define CASE(id) \
	CASED(#id)

enum { test_return_code_passed, test_return_code_fatal_failure, test_return_code_nonfatal_failur };

#ifndef TEST_NOFORK
#define test_fork_(obj) test_fork(&(obj).result)
#else
#define test_fork_(obj) setjmp((obj).env)
#endif

#define CASED(casename) \
	if (currcase) { \
		if (!currcase->child) { \
			currcase->child = alloca(sizeof(struct test_case_info)); \
			currcase->child->parent = currcase; \
		} \
		currcase = currcase->child; \
	} else { \
		currcase = alloca(sizeof(struct test_case_info)); \
		currcase->parent = NULL; \
	} \
	currcase->name = (casename); \
	currcase->file = __FILE__; \
	currcase->line = __LINE__; \
	currcase->child = NULL; \
	currcase->result = EXIT_SUCCESS; \
	currcase->outlen = lseek(currtest->outfd, 0, SEEK_CUR); \
\
	if (!test_fork_(*currcase)) { \
		test_print_case_path(); \
		goto TOKENPASTE(test_case_run_, __LINE__); \
	} else { \
		switch (currcase->result) { \
		case EXIT_SUCCESS: \
		case EXIT_SKIP: \
			/* Clear output of passed case. */ \
			/* FIXME: warning: implicit declaration of function ‘truncate’. */ \
			ftruncate(currtest->outfd, currcase->outlen); \
			break; \
		case EXIT_NONFATAL_FAILURE: \
			*(currcase->parent ? &currcase->parent->result : &currtest->result) = EXIT_NONFATAL_FAILURE; \
			break; \
		default: {/* Propagate return code. */ \
			int result = currcase->result; \
			currcase = currcase->parent; \
			test_exit(result); \
		} \
		} \
		currcase = currcase->parent; \
	} \
	for (;0;test_exit(currcase->result)) TOKENPASTE(test_case_run_, __LINE__):

#define TEST_VAR_(type, id) \
	static struct test_##type##_info test_##id __attribute__((no_reorder, __used__, unused, __section__("test"))) \

#define SUITE(id) \
	SUITED(#id)

#define SUITED(name) \
	SUITE_(name)

#define SUITE_(name) \
	TEST_VAR_(suite, suite) = {{test_object_id_suite}, name, __FILE__, 0, 0};

enum test_object_id {
	test_object_id_suite,
	test_object_id_hook,
	test_object_id_test
};

enum test_event {
	test_event_setup_suite,
	test_event_teardown_suite,
	test_event_setup_test,
	test_event_teardown_test
};

#define HOOK_SUITE_ HOOK__SCOPE__(1)
#define HOOK_GLOBAL_ HOOK__SCOPE__(2)

#define HOOK(scope) \
	TOKENPASTE3(HOOK_, scope, _)

#define SETUP() \
	static void test_setup(void); \
	HOOK(SUITE) \
	{ \
		if (test_event_setup_suite == event) \
			test_setup(); \
	} \
	static void test_setup(void)

#define TEARDOWN() \
	static void test_teardown(void); \
	HOOK(SUITE) \
	{ \
		if (test_event_teardown_suite == event) \
			test_teardown(); \
	} \
	static void test_teardown(void)

#define HOOK__SCOPE__(scope) \
	HOOK__SCOPE___(scope, TOKENPASTE(test_hook_at_line_, __LINE__))

#define HOOK__SCOPE___(scope, id) \
	static void id(int); \
	TEST_VAR_(hook, id) = {{test_object_id_hook}, id, scope}; \
	static void id(int event)

#define STRINGIFY(t) #t
#define TOKENPASTE_(a, b) a##b
#define TOKENPASTE3_(a, b, c) a##b##c
#define TOKENPASTE(a, b) TOKENPASTE_(a, b)
#define TOKENPASTE3(a, b, c) TOKENPASTE3_(a, b, c)

# define TEST_FAILURE_LOC_ __FILE__, __LINE__,
# define TEST_FAILURE_LOCFMT_ "%s:%u: "

#define TEST_TEST_ASSERT_FAILURE_(code) FATAL_FAILURE(code)
#define TEST_TEST_EXPECT_FAILURE_(ignored) NONFATAL_FAILURE(EXIT_NONFATAL_FAILURE)

#define TEST_TEST_ASSERT_NOUN_ "Assertion"
#define TEST_TEST_EXPECT_NOUN_ "Expectation"

#define TEST_SUCCESS_FORMAT_TEMPLATE_(test) \
	TEST_FAILURE_LOCFMT_ "\x1b[32m" test##NOUN_ "\x1b[0m" " %s passed.\n"
#define TEST_FAILURE_FORMAT_TEMPLATE_(test) \
	TEST_FAILURE_LOCFMT_ "\x1b[1;31m" test##NOUN_ "\x1b[0m" " %s failed.\n"
#define TEST_FAILURE_FORMAT_CMPTWO_TEMPLATE_(test, fmt) \
	TEST_FAILURE_LOCFMT_ "\x1b[1;31m" test##NOUN_ "\x1b[0m" " %s failed; left=" fmt ", right=" fmt ".\n"
#define TEST_FAILURE_FORMAT_CMPONE_TEMPLATE_(test, fmt) \
	TEST_FAILURE_LOCFMT_ "\x1b[1;31m" test##NOUN_ "\x1b[0m" " %s failed; got " fmt ".\n"

#define TEST_FAILURE_FORMAT_CMPSEL_(test, type, kind) \
	__builtin_choose_expr(test_istype_(type, char *) || test_istype_(type, char[]), \
	                      TEST_FAILURE_FORMAT_CMP##kind##_TEMPLATE_(test, "\"%s\""), \
	__builtin_choose_expr(test_istype_(type, char), \
	                      TEST_FAILURE_FORMAT_CMP##kind##_TEMPLATE_(test, "'%c'"), \
	__builtin_choose_expr(test_istype_(type, double) || test_istype_(type, float), \
	                      TEST_FAILURE_FORMAT_CMP##kind##_TEMPLATE_(test, "%f"), \
	__builtin_choose_expr(test_istype_(type, void *), \
	                      TEST_FAILURE_FORMAT_CMP##kind##_TEMPLATE_(test, "%p"), \
	                      TEST_FAILURE_FORMAT_CMP##kind##_TEMPLATE_(test, "%d")))))

#define test_msg_(test, cond, msg) \
	if (!(cond)) { \
		fprintf(stderr, TEST_FAILURE_FORMAT_TEMPLATE_(test), TEST_FAILURE_LOC_ msg); \
		test##FAILURE_(EXIT_FAILURE); \
	} \

#define test_istype_(val, type) \
	__builtin_types_compatible_p(__typeof__(val), type)

#define test_cmp_(lhs, rhs) \
	__builtin_choose_expr(test_istype_(lhs, char *) || test_istype_(lhs, char[]), \
	                      (0 == strcmp((char *)(size_t)lhs, (char *)(size_t)rhs)), \
	                      (lhs == rhs))

#define test_equal_(test, lhs, rhs) \
do { \
	__typeof__(lhs) const lhsv = lhs; /* no parenthesis: ("invalid") */ \
	__typeof__(rhs) const rhsv = rhs; \
	if (!test_cmp_(lhsv, rhsv)) { \
		__builtin_choose_expr(__builtin_constant_p(lhs) + __builtin_constant_p(rhs) == 1, \
			fprintf(stderr, TEST_FAILURE_FORMAT_CMPSEL_(test, __typeof__(rhs), ONE), \
				TEST_FAILURE_LOC_ STRINGIFY(lhs == rhs), __builtin_choose_expr(__builtin_constant_p(rhs), lhsv, rhsv)) \
			, \
			fprintf(stderr, TEST_FAILURE_FORMAT_CMPSEL_(test, __typeof__(rhs), TWO), \
				TEST_FAILURE_LOC_ STRINGIFY(lhs == rhs), lhsv, rhsv) \
			); \
		test##FAILURE_(EXIT_FAILURE); \
	} else if (TEST_VERBOSE) { \
		fprintf(stderr, TEST_SUCCESS_FORMAT_TEMPLATE_(test), \
			TEST_FAILURE_LOC_ STRINGIFY(lhs == rhs)); \
	} \
} while(0)

/* Basic tests */

#define assert_msg(cond, msg) test_msg_(TEST_TEST_ASSERT_, cond, msg)
#define expect_msg(cond, msg) test_msg_(TEST_TEST_EXPECT_, cond, msg)

/* Simple tests */

#define assert_true(cond) test_msg_(TEST_TEST_ASSERT_, cond, "`" #cond "'")
#define assert_null(expr) assert_true(NULL == (expr))
#define assert_nonnull(expr) assert_true(NULL != (expr))
#define assert_false(cond) assert_true(!(cond))
#define expect_true(cond) test_msg_(TEST_TEST_EXPECT_, cond, "`" #cond "'")
#define expect_false(cond) expect_true(!(cond))

/* Smart tests */

#define assert_equal(lhs, rhs) test_equal_(TEST_TEST_ASSERT_, lhs, rhs)
#define expect_equal(lhs, rhs) test_equal_(TEST_TEST_EXPECT_, lhs, rhs)

#define assert_memequal(lhs, rhs, size) \
	assert(0 == memcmp(lhs, rhs, size));

void test_print_suite_path(void);
void test_print_test_path(void);
void test_print_case_path(void);

#define FATAL_FAILURE(code) test_exit(code);
#define NONFATAL_FAILURE(code) \
do { \
	int *result = (currcase ? &currcase->result : &currtest->result); \
	if (EXIT_SUCCESS == *result) \
		*result = (code); \
} while(0)

struct test_info_header {
	unsigned char const type;
};

struct test_suite_info {
	struct test_info_header const header;
	char const *const name;
	char const *const file;
	unsigned num_tests;
	int setup_ran;
} __attribute__((aligned(64)));

struct test_hook_info {
	struct test_info_header const header;
	void(*const hook)(int event);
	unsigned scope;
} __attribute__((aligned(64)));

struct test_test_info {
	struct test_info_header const header;
	char const *const name;
	char const *const file;
	unsigned line;
	void(*const run)(void);
	unsigned long iters;
	int outfd;
	unsigned long total_ns;
	int result;
} __attribute__((aligned(64)));

/** Info about test case. */
struct test_case_info {
	char const *name;
	char const *file;
	unsigned line;
	off_t outlen;
	struct test_case_info *parent;
	struct test_case_info *child;
	int result;
#ifdef TEST_NOFORK
	jmp_buf env;
#endif
};

void test_exit(int code);
int test_fork(int *result) __attribute__((returns_twice));

#endif /* TEST_H */
/* vim:set ft=c ts=4 sw=4 noet: */
