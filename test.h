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
#ifndef C1_TEST_H
#define C1_TEST_H

#undef _GNU_SOURCE
#define _GNU_SOURCE
#include <alloca.h>
#include <fcntl.h>
#include <setjmp.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>

#ifndef TEST_VERBOSE
# define TEST_VERBOSE 0
#else
# undef TEST_VERBOSE
# define TEST_VERBOSE 1
#endif

#ifndef TEST_NOGATHEROUTPUT
# define TEST_NOGATHEROUTPUT 0
#else
# undef TEST_NOGATHEROUTPUT
# define TEST_NOGATHEROUTPUT 1
#endif

#define EXIT_SKIP 77
#define EXIT_NONFATAL_FAILURE 123

#define SKIP test_exit(EXIT_SKIP)
#define FATAL_FAILURE(code) test_exit(code)
#define NONFATAL_FAILURE(code) do { \
	int *status = (test_current_case_ ? &test_current_case_->status : &test_current_test_->status); \
	if (EXIT_SUCCESS == *status) \
		*status = (code); \
} while (0)

#ifndef TEST_NOFORK
# define test_fork_(o) test_fork__(&(o).status)
int test_fork__(int *status) __attribute__((returns_twice));
#else
# define test_fork_(o) setjmp((o).env)
#endif

#define SUITE(name) \
	TEST_BUILD_SUITE_(#name)

#define TEST_BUILD_SUITE_(name) \
	TEST_VAR_(suite, suite) = {{test_object_type_suite}, name, __FILE__, __LINE__};

#define TEST(name) \
	TEST_BUILD_TEST_(#name, 1)

#define BENCH(name, repeat) \
	TEST_BUILD_TEST_(#name, repeat)

#define TEST_BUILD_TEST_(name, repeat) \
	TEST_BUILD_TEST__(TEST_TOKENPASTE_(_test__line, __LINE__), name, repeat)

#define TEST_BUILD_TEST__(proc, name, repeat) \
	static void proc(void); \
	TEST_VAR_(test, proc) = { \
		{test_object_type_test}, \
		name, \
		__FILE__, __LINE__, \
		proc, \
		repeat, \
		-1, \
		0, \
		EXIT_SUCCESS, \
		0, \
	}; \
	static void proc(void)

#define CASE(casename) \
	if (NULL != test_current_case_) { \
		if (NULL == test_current_case_->child) { \
			test_current_case_->child = alloca(sizeof(struct test_case_)); \
			test_current_case_->child->parent = test_current_case_; \
		} \
		test_current_case_ = test_current_case_->child; \
	} else { \
		test_current_case_ = alloca(sizeof(struct test_case_)); \
		test_current_case_->parent = NULL; \
	} \
	test_current_case_->name = #casename; \
	test_current_case_->file = __FILE__; \
	test_current_case_->line = __LINE__; \
	test_current_case_->child = NULL; \
	test_current_case_->status = EXIT_SUCCESS; \
	test_current_case_->outoff = lseek(test_current_test_->outfd, 0, SEEK_CUR); \
	test_shared_->top_level = 1; \
\
	if (!test_fork_(*test_current_case_)) { \
		test_print_case_header_(); \
		goto TEST_TOKENPASTE_(_test_case_run_line, __LINE__); \
	} else { \
		switch (test_current_case_->status) { \
		case EXIT_SUCCESS: \
		case EXIT_SKIP: \
			/* drop output of passed case */ \
			ftruncate(test_current_test_->outfd, test_current_case_->outoff); \
			break; \
\
		case EXIT_NONFATAL_FAILURE: \
			*(NULL != test_current_case_->parent \
			  ? &test_current_case_->parent->status \
			  : &test_current_test_->status \
			) = EXIT_NONFATAL_FAILURE; \
			break; \
\
		/* propagate failure */ \
		default: { \
			int const status = test_current_case_->status; \
			test_current_case_ = test_current_case_->parent; \
			test_exit(status); \
		} \
		} \
		test_current_case_ = test_current_case_->parent; \
	} \
	for (; 0; test_exit(test_current_case_->status)) TEST_TOKENPASTE_(_test_case_run_line, __LINE__):

#define TEST_VAR_(type, id) \
	static struct test_##type##_ test_##id __attribute__((no_reorder, __used__, unused, __section__("test"))) \

#define HOOK(scope) \
	TEST_TOKENPASTE3_(_HOOK_SCOPE_, scope, _)

#define SETUP() \
	static void test_hook_setup_(void); \
	HOOK(SUITE) \
	{ \
		if (test_event_setup_suite == event) \
			test_hook_setup_(); \
	} \
	static void test_hook_setup_(void)

#define teardown() \
	static void test_hook_teardown_(void); \
	HOOK(SUITE) \
	{ \
		if (test_event_teardown_suite == event) \
			test_hook_teardown_(); \
	} \
	static void test_hook_teardown_(void)

#define HOOK_SCOPE_SUITE_ TEST_BUILD_HOOK_(1)
#define HOOK_SCOPE_GLOBAL_ TEST_BUILD_HOOK_(2)

#define TEST_BUILD_HOOK_(scope) \
	TEST_BUILD_HOOK__(scope, TEST_TOKENPASTE_(test_hook_, __LINE__))

#define TEST_BUILD_HOOK__(scope, proc) \
	static void proc(int); \
	TEST_VAR_(HOOK, proc) = {{test_object_type_hook}, proc, scope}; \
	static void proc(int event)

#define TEST_TOKENPASTE__(a, b) a##b
#define TEST_TOKENPASTE3__(a, b, c) a##b##c
#define TEST_TOKENPASTE_(a, b) TEST_TOKENPASTE__(a, b)
#define TEST_TOKENPASTE3_(a, b, c) TEST_TOKENPASTE3__(a, b, c)

#define TEST_TEST_ASSERT_FAILURE_(status) FATAL_FAILURE(status)
#define TEST_TEST_EXPECT_FAILURE_(_ignored) NONFATAL_FAILURE(EXIT_NONFATAL_FAILURE)

#define TEST_TEST_ASSERT_NOUN_ "Assertion"
#define TEST_TEST_EXPECT_NOUN_ "Expectation"

#define TEST_FORMAT_SUCCESS_(test) \
	"%s:%u: \x1b[32m" test##NOUN_ "\x1b[0m %s passed.\n"
#define TEST_FORMAT_FAILURE_(test) \
	"%s:%u: \x1b[1;31m" test##NOUN_ "\x1b[0m %s \x1b[1;31mfailed\x1b[0m.\n"
#define TEST_FORMAT_TWO_(test, fmt) \
	"%s:%u: \x1b[1;31m" test##NOUN_ "\x1b[0m %s \x1b[1;31mfailed\x1b[0m\n  left=" fmt ";\n  right=" fmt ".\n"
#define TEST_FORMAT_ONE_(test, fmt) \
	"%s:%u: \x1b[1;31m" test##NOUN_ "\x1b[0m %s \x1b[1;31mfailed\x1b[0m; got " fmt ".\n"

#if __STDC_VERSION__ >= 201112L
/* TODO: use _Generic macros */
#endif
#if defined(__GNUC__) || defined(clang)
# define TEST_FORMAT_(test, type, kind) \
	__builtin_choose_expr(TEST_ISTYPE_(type, char *) || TEST_ISTYPE_(type, char[]) || TEST_ISTYPE_(type, char(*)[]), \
	                      TEST_FORMAT_##kind##_(test, "“%s”"), \
	__builtin_choose_expr(TEST_ISTYPE_(type, char), \
	                      TEST_FORMAT_##kind##_(test, "‘%c’"), \
	__builtin_choose_expr(TEST_ISTYPE_(type, double) || TEST_ISTYPE_(type, float), \
	                      TEST_FORMAT_##kind##_(test, "%f"), \
	__builtin_choose_expr(TEST_ISTYPE_(type, void *), \
	                      TEST_FORMAT_##kind##_(test, "%p"), \
	__builtin_choose_expr(TEST_ISTYPE_(type, short), \
	                      TEST_FORMAT_##kind##_(test, "%h"), \
	__builtin_choose_expr(TEST_ISTYPE_(type, unsigned short), \
	                      TEST_FORMAT_##kind##_(test, "%hu"), \
	__builtin_choose_expr(TEST_ISTYPE_(type, int), \
	                      TEST_FORMAT_##kind##_(test, "%d"), \
	__builtin_choose_expr(TEST_ISTYPE_(type, unsigned int), \
	                      TEST_FORMAT_##kind##_(test, "%u"), \
	__builtin_choose_expr(TEST_ISTYPE_(type, long), \
	                      TEST_FORMAT_##kind##_(test, "%l"), \
	__builtin_choose_expr(TEST_ISTYPE_(type, unsigned long), \
	                      TEST_FORMAT_##kind##_(test, "%lu"), \
	__builtin_choose_expr(TEST_ISTYPE_(type, long long), \
	                      TEST_FORMAT_##kind##_(test, "%ll"), \
	__builtin_choose_expr(TEST_ISTYPE_(type, unsigned long long), \
	                      TEST_FORMAT_##kind##_(test, "%llu"), \
	                      "<?>" \
	                      ))))))))))))

# define TEST_COND_(test, cond, msg) \
	if (++test_shared_->num_assertions, !(cond)) { \
		fprintf(stderr, TEST_FORMAT_FAILURE_(test), __FILE__, __LINE__, msg); \
		test##FAILURE_(EXIT_FAILURE); \
	}

# define TEST_ISTYPE_(val, type) \
	__builtin_types_compatible_p(__typeof__(val), type)

# define TEST_CMP__(lhs, cmp, rhs) \
	__builtin_choose_expr(TEST_ISTYPE_(lhs, char *) || TEST_ISTYPE_(lhs, char[]) || TEST_ISTYPE_(lhs, char (*)[]), \
	                      (strcmp((char const*)(size_t)lhs, (char const*)(size_t)rhs) cmp 0), \
	                      ((lhs) cmp ((__typeof__(lhs))rhs))/* FIXME: It can result in false positives. */)

# define TEST_CMP_(test, lhs, cmp, rhs) do { \
	__typeof__(lhs) const lhsv = lhs; /* Don't put parenthesis: ("invalid") */ \
	__typeof__(rhs) const rhsv = rhs; \
	if (++test_shared_->num_assertions, !TEST_CMP__(lhsv, cmp, rhsv)) { \
		__builtin_choose_expr(__builtin_constant_p(lhs) + __builtin_constant_p(rhs) == 1, \
			fprintf(stderr, TEST_FORMAT_(test, __typeof__(rhs), ONE), \
				__FILE__, __LINE__, #lhs " " #cmp " " #rhs, __builtin_choose_expr(__builtin_constant_p(rhs), lhsv, rhsv)) \
			, \
			fprintf(stderr, TEST_FORMAT_(test, __typeof__(rhs), TWO), \
				__FILE__, __LINE__, #lhs " " #cmp " " #rhs, lhsv, rhsv) \
			); \
		test##FAILURE_(EXIT_FAILURE); \
	} else if (TEST_VERBOSE) { \
		fprintf(stderr, TEST_FORMAT_SUCCESS_(test), \
			__FILE__, __LINE__, #lhs " " #cmp " " #rhs); \
	} \
} while(0)

#else
# define TEST_COND_(test, cond, msg) \
	if (++test_shared_->num_assertions, !(cond)) { \
		fprintf(stderr, TEST_FORMAT_FAILURE_(test), __FILE__, __LINE__, msg); \
		test##FAILURE_(EXIT_FAILURE); \
	}

# define TEST_CMP_(test, lhs, cmp, rhs) TEST_COND_(test, ((lhs) cmp (rhs)), #lhs " " #cmp " " #rhs)
#endif

#define assert_msg(cond, msg) TEST_COND_(TEST_TEST_ASSERT_, cond, msg)
#define expect_msg(cond, msg) TEST_COND_(TEST_TEST_EXPECT_, cond, msg)

#define assert_true(cond)    TEST_COND_(TEST_TEST_ASSERT_, cond,           "`"     #cond "'")
#define assert_false(cond)   TEST_COND_(TEST_TEST_ASSERT_, !(cond),        "`" "!" #cond "'")
#define assert_null(expr)    TEST_COND_(TEST_TEST_ASSERT_, (expr) == NULL, "`"     #expr " == NULL" "'")
#define assert_nonnull(expr) TEST_COND_(TEST_TEST_ASSERT_, (expr) != NULL, "`"     #expr " != NULL" "'")
#define assert_memequal(lhs, rhs, size) assert_equal(memcmp(lhs, rhs, size), 0);

#define expect_true(cond)    TEST_COND_(TEST_TEST_EXPECT_,   cond,         "`"     #cond "'")
#define expect_false(cond)   TEST_COND_(TEST_TEST_EXPECT_, !(cond),        "`" "!" #cond "'")
#define expect_null(expr)    TEST_COND_(TEST_TEST_EXPECT_, (expr) == NULL, "`"     #expr " == NULL" "'")
#define expect_nonnull(expr) TEST_COND_(TEST_TEST_EXPECT_, (expr) != NULL, "`"     #expr " != NULL" "'")
#define expect_memequal(lhs, rhs, size) expect_equal(memcmp(lhs, rhs, size), 0);

#define assert_cmp(lhs, cmp, rhs) TEST_CMP_(TEST_TEST_ASSERT_, lhs, cmp, rhs)
#define expect_cmp(lhs, cmp, rhs) TEST_CMP_(TEST_TEST_EXPECT_, lhs, cmp, rhs)

#define assert_equal(lhs, rhs)   assert_cmp(lhs, ==, rhs)
#define expect_equal(lhs, rhs)   expect_cmp(lhs, ==, rhs)
#define assert_less(lhs, rhs)    assert_cmp(lhs, <,  rhs)
#define expect_less(lhs, rhs)    expect_cmp(lhs, <,  rhs)
#define assert_greater(lhs, rhs) assert_cmp(lhs, >,  rhs)
#define expect_greater(lhs, rhs) expect_cmp(lhs, >,  rhs)

extern struct test_shared_ *test_shared_;
extern struct test_test_ *test_current_test_;
extern struct test_case_ *test_current_case_;

enum test_object_type {
	test_object_type_suite,
	test_object_type_hook,
	test_object_type_test
};

enum test_event {
	test_event_setup_suite,
	test_event_teardown_suite,
	test_event_setup_test,
	test_event_teardown_test
};

void test_print_case_header_(void);

struct test_shared_ {
	size_t num_assertions;
	unsigned long total_ns;
	int top_level;
};

struct test_header_ {
	enum test_object_type const type;
} __attribute__((aligned(64)));

struct test_suite_ {
	struct test_header_ const header;

	char const *const name;
	char const *const file;
	unsigned const line;

} __attribute__((aligned(64)));

struct test_hook_ {
	struct test_header_ const header;
	void(*const proc)(int event);
	unsigned scope;
} __attribute__((aligned(64)));

struct test_test_ {
	struct test_header_ const header;

	char const *const name;
	char const *const file;
	unsigned const line;

	void(*const run)(void);
	unsigned long num_iters;

	int outfd;
	unsigned long time_ns;
	int status;
	int last_in_suite;
} __attribute__((aligned(64)));

struct test_case_ {
	char const *name;
	char const *file;
	unsigned line;

	off_t outoff;
	struct test_case_ *parent;
	struct test_case_ *child;
	int status;
#ifdef TEST_NOFORK
	jmp_buf env;
#endif
};

void test_exit(int status) __attribute__((noreturn));

#endif
